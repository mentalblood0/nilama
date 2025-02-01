import std/[httpclient, json, sugar]

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

type ChatRequest = object
  model: string
  messages: seq[Message]
  stream: bool
  options: Options

proc write*(
    chat: var Chat, message: string, server_address: string = "http://127.0.0.1:11434"
) =
  chat.messages.add(Message(role: "user", content: message))
  let req = ChatRequest(
    model: chat.model, messages: chat.messages, stream: false, options: chat.options
  )
  let resp = chat.client.request(
    server_address & "/api/chat", http_method = HttpPost, body = $ %*req
  )
  chat.messages.add(
    Message(
      role: "assistant", content: resp.body.parse_json["message"]["content"].get_str()
    )
  )
  chat.client.close()

when is_main_module:
  var chat = Chat(
    model: "granite3.1-dense",
    messages: @[],
    client: new_http_client(),
    options: Options(seed: 101, temperature: 0),
  )
  chat.write "Why is the sky blue?"
  echo chat.messages[^1].content
