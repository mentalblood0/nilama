import
  std/[
    httpclient, json, sugar, tables, macros, os, options, strutils, posix, inotify,
    asyncdispatch,
  ]

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
      let json_method = {
        "int": "get_int", "string": "get_str", "float": "get_float", "bool": "get_bool"
      }.to_table[atype]
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

type Message* = object
  role*: string
  content*: string

type Options* = object
  seed*: Option[int]
  temperature*: Option[float]

type Chat* = object
  model*: string
  messages*: seq[Message]
  options*: Option[Options]

type ChatRequest* = object
  model*: string
  messages*: seq[Message]
  stream*: bool
  options*: Option[Options]
  tools*: seq[Tool]

type Config* = object
  chat*: Chat
  server_address*: string

proc prompt*(config: Config): Future[Config] {.async.} =
  let req = ChatRequest(
    model: config.chat.model,
    messages: config.chat.messages,
    stream: false,
    options: config.chat.options,
    tools: tools,
  )
  let client = new_async_http_client()
  let resp =
    await client.post_content(config.server_address & "/api/chat", body = $ %*req)
  let message = resp.parse_json["message"]
  client.close

  result = config
  if "tool_calls" in message:
    for tc in message["tool_calls"]:
      let name = tc["function"]["name"].get_str()
      let args = tc["function"]["arguments"]
      result.chat.messages.add Message(
        role: "tool", content: $tools_adapters[name](args)
      )
  else:
    result.chat.messages.add Message(
      role: "assistant", content: message["content"].get_str()
    )

proc process(config_path: string) {.async.} =
  var config = block:
    try:
      config_path.parse_file.to Config
    except [JsonParsingError, JsonKindError]:
      echo "JSON parsing error: " & get_current_exception().msg
      return
  if config.chat.messages.len == 0:
    return
  if not config.chat.messages[^1].content.ends_with "//":
    return

  config.chat.messages[^1].content = config.chat.messages[^1].content[0 .. ^3]
  config = await prompt config

  config_path.write_file pretty %*config

proc run_config_processor*() {.async.} =
  const max_watches = 1024

  let inotify_fd = inotify_init()
  let config_dir = get_config_dir() / "nilama"
  let wd = inotify_fd.inotify_add_watch(config_dir.cstring, IN_CLOSE_WRITE)

  var events: array[max_watches, byte]
  while (let n = read(inotifyFd, addr events, max_watches); n) > 0:
    for e in inotify_events(addr events, n):
      let path = config_dir / $cast[cstring](addr e[].name)
      if path.ends_with ".json":
        await path.process

when is_main_module:
  wait_for run_config_processor()
