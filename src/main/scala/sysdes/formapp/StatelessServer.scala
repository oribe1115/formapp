package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}
import scala.collection.mutable.ArrayBuffer

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{
    Element,
    InputElement,
    NotFound,
    Ok,
    Request,
    RequestBody,
    Response,
    ResponseBody,
    TextAreaElement,
    TextElement
  }

  override def handle(request: Request): Response =
    request match {
      case Request("GET", "/", _, _, _)                 => index()
      case Request("POST", "/form/name", _, _, body)    => nameForm(body)
      case Request("POST", "/form/gender", _, _, body)  => genderForm(body)
      case Request("POST", "/form/message", _, _, body) => messageForm(body)
      case _ =>
        NotFound(
          s"Requested resource '${request.path}' for ${request.method} is not found."
        )
    }

  def index(): Response = {
    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]
    elements.append(new TextElement("アンケート開始", true))
    elements.append(new InputElement("submit", "", "start"))

    val resBody = new ResponseBody(
      "/form/name",
      "post",
      elements.toArray
    )
    Ok(resBody.toString())
  }

  def nameForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("名前:", false))
    elements.append(new InputElement("text", "name", ""))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
    }

    val resBody = new ResponseBody(
      "/form/gender",
      "post",
      elements.toArray
    )
    Ok(resBody.toString())
  }

  def genderForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("性別:", false))
    elements.append(new InputElement("radio", "gender", "male"))
    elements.append(new TextElement("男性", false))
    elements.append(new InputElement("radio", "gender", "female"))
    elements.append(new TextElement("女性", false))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
    }

    val resBody = new ResponseBody(
      "/form/message",
      "post",
      elements.toArray
    )
    Ok(resBody.toString())
  }

  def messageForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("メッセージ:", true))
    elements.append(new TextAreaElement("message", ""))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
    }

    val resBody = new ResponseBody(
      "/form/confirm",
      "post",
      elements.toArray
    )
    Ok(resBody.toString())
  }
}
