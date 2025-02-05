import std/[httpclient, json, sugar, tables, macros, os, options, times, strutils]

type Message* = object
  role: string
  content: string

type Options* = object
  seed: Option[int]
  temperature: Option[float]

type Chat* = object
  model: string
  messages: seq[Message]
  options: Option[Options]

type Property = object
  `type`: string
  description: string

type Parameters = object
  `type`: string
  properties: Table[string, Property]
  required: seq[string]

type Function = object
  name: string
  description: string
  parameters: Parameters

type Tool = object
  `type`: string
  function: Function

type ChatRequest = object
  model: string
  messages: seq[Message]
  stream: bool
  options: Option[Options]
  tools: seq[Tool]

type Config* = object
  chat: Chat
  server_address: string

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
    let r = new_nim_node nnkStmtList
    r.add new_nim_node nnkPrefix
    r[0].add ident "%*"
    r[0].add new_nim_node nnkCall
    r[0][1].add name
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

proc prompt*(config: var Config) =
  let req = ChatRequest(
    model: config.chat.model,
    messages: config.chat.messages,
    stream: false,
    options: config.chat.options,
    tools: tools,
  )
  let client = new_http_client()
  let resp = client.request(
    config.server_address & "/api/chat", http_method = HttpPost, body = $ %*req
  )
  let message = resp.body.parse_json["message"]
  client.close

  if "tool_calls" in message:
    for tc in message["tool_calls"]:
      let name = tc["function"]["name"].get_str()
      let args = tc["function"]["arguments"]
      config.chat.messages.add Message(
        role: "tool", content: $tools_adapters[name](args)
      )
  else:
    config.chat.messages.add Message(
      role: "assistant", content: message["content"].get_str()
    )

proc run_config_processor*(config_name: string) =
  let configs_dir = get_config_dir() / "nilama"
  let config_path = configs_dir / config_name & ".json"

  var last_write_time = now().to_time
  while true:
    sleep 1000

    if not config_path.file_exists:
      echo "not exists"
      continue

    let info = config_path.get_file_info

    let t = info.last_write_time
    if last_write_time == t:
      continue
    last_write_time = t

    var config = block:
      try:
        (parse_file configs_dir / config_name & ".json").to Config
      except JsonParsingError as e:
        echo "JSON parsing error: " & e.msg
        continue
    if not config.chat.messages[^1].content.ends_with "//":
      continue

    config.chat.messages[^1].content = config.chat.messages[^1].content[0 .. ^3]
    prompt config

    config_path.write_file pretty %*config

when is_main_module:
  run_config_processor param_str(1)
