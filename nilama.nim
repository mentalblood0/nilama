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

type ToolAdapter = object
  name: string
  f: (JsonNode) -> JsonNode

var tools_adapters: seq[ToolAdapter]

macro register_tool(definition: untyped): untyped =
  let args = definition[0][3]
  let name = definition[0][0]

  var properties: Table[string, Property]
  var required: seq[string]
  for i in 1 .. (args.len - 1):
    let ad = args[i]
    properties[$ad[0]] = Property(`type`: $ad[1], description: "")
    required.add $ad[0]
  let new_tool = Tool(
    type: "function",
    function: Function(
      name: $name,
      description: $name,
      parameters:
        Parameters(`type`: "object", properties: properties, required: required),
    ),
  )

  let first_arg = $args[1][0]
  let second_arg = $args[2][0]

  let f = (j: JsonNode) => j
  let new_tool_adapter = ToolAdapter(name: $name, f: nil)

  quote:
    `definition`

    tools.add `new_tool`

    tools_adapters.add `new_tool_adapter`
    tools_adapters[^1].f = proc(j: JsonNode): JsonNode =
      let first = j[`first_arg`].get_int()
      let second = j[`second_arg`].get_int()
      %*`name`(first, second)

register_tool:
  func add_two_integers(first_integer: int, second_integer: int): int =
    first_integer + second_integer

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
  dump resp.body
  let message = resp.body.parse_json["message"]
  chat.messages.add(Message(role: "assistant", content: message["content"].get_str()))

  if "tool_calls" in message:
    for tc in message["tool_calls"]:
      let function_name = tc["function"]["name"].get_str()
      for a in tools_adapters:
        if a.name == function_name:
          echo a.f(tc["function"]["arguments"])

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
