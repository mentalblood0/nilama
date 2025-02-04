import std/[httpclient, json, sugar, tables, macros]

type Message* = object
  role: string
  content: string

type Options* = object
  seed: int
  temperature: float

type Chat* = object
  model: string
  messages: seq[Message]
  client: HttpClient
  options: Options

type Property* = object
  `type`: string
  description: string

type Parameters* = object
  `type`: string
  properties: Table[string, Property]
  required: seq[string]

type Function* = object
  name: string
  description: string
  parameters: Parameters

type Tool* = object
  `type`: string
  function: Function

type ChatRequest = object
  model: string
  messages: seq[Message]
  stream: bool
  options: Options
  tools: seq[Tool]

var tools: seq[Tool]
var tools_adapters: Table[string, (JsonNode) -> JsonNode]

macro register_tool(definition: untyped): untyped =
  let args = definition[0][3]
  let name = definition[0][0]
  let name_str = $name

  var properties: Table[string, Property]
  var args_names: seq[string]
  for i in 1 .. (args.len - 1):
    let ad = args[i]
    properties[$ad[0]] = Property(`type`: $ad[1], description: "")
    args_names.add $ad[0]
  let new_tool = Tool(
    type: "function",
    function: Function(
      name: $name,
      description: $name,
      parameters:
        Parameters(`type`: "object", properties: properties, required: args_names),
    ),
  )

  let adapter = block:
    let r = new_nim_node(nnkStmtList)
    r.add(new_nim_node(nnkPrefix))
    r[0].add(ident("%*"))
    r[0].add(new_nim_node(nnkCall))
    r[0][1].add(name)
    for i in 1 .. (args.len - 1):
      let adef = args[i]
      let aname = $adef[0]
      let atype = $adef[1]
      let json_method =
        {"int": "get_int", "string": "get_str", "float": "get_float"}.to_table[atype]
      r[0][1].add(
        new_call(
          new_dot_expr(
            new_nim_node(nnkBracketExpr).add(ident("j")).add(new_str_lit_node(aname)),
            ident(json_method),
          )
        )
      )
    r

  quote:
    `definition`
    tools.add `new_tool`
    tools_adapters[`name_str`] = (j: JsonNode) => `adapter`

register_tool:
  func add_two_integers(first_integer: int, second_integer: int): int =
    first_integer + second_integer

register_tool:
  func concatenate_two_strings(first_string: string, second_string: string): string =
    first_string & second_string

proc write*(
    chat: var Chat, message: string, server_address: string = "http://127.0.0.1:11434"
) =
  chat.messages.add(Message(role: "user", content: message))
  let req = ChatRequest(
    model: chat.model,
    messages: chat.messages,
    stream: false,
    options: chat.options,
    tools: tools,
  )
  let resp = chat.client.request(
    server_address & "/api/chat", http_method = HttpPost, body = $ %*req
  )
  let message = resp.body.parse_json["message"]

  if "tool_calls" in message:
    for tc in message["tool_calls"]:
      let name = tc["function"]["name"].get_str()
      let args = tc["function"]["arguments"]
      chat.messages.add(Message(role: "tool", content: $tools_adapters[name](args)))
  else:
    chat.messages.add(Message(role: "assistant", content: message["content"].get_str()))

  chat.client.close()

when is_main_module:
  var chat = Chat(
    model: "granite3.1-dense",
    messages: @[],
    client: new_http_client(),
    options: Options(seed: 101, temperature: 0),
  )
  chat.write("Add two integer numbers: 2 and 1")
  echo chat.messages[^1].content
  chat.write("Concatenate two strings: 'lalala' and 'lololo'")
  echo chat.messages[^1].content
