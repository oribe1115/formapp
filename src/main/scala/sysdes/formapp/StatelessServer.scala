package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{
    Element,
    FormElement,
    InputElement,
    NotFound,
    Ok,
    RadioElement,
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
      case Request("POST", "/form/confirm", _, _, body) => confirm(body)
      case Request("POST", "/form/finish", _, _, _)     => finish()
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
      Array[Element](new FormElement("/form/name", "post", elements.toArray))
    )
    Ok(resBody.toString())
  }

  def nameForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)
    val params = reqBody.parseParams()

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("名前:", false))
    elements.append(
      new InputElement("text", "name", params.getOrElse("name", ""))
    )
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
    }

    val resBody = new ResponseBody(
      Array[Element](new FormElement("/form/gender", "post", elements.toArray))
    )
    Ok(resBody.toString())
  }

  def genderForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)
    val params = reqBody.parseParams()

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    val backElements: ArrayBuffer[Element] = new ArrayBuffer[Element]()

    val initialWithFemale = (params.getOrElse("gender", "") == "female")

    elements.append(new TextElement("性別:", false))
    elements.append(new RadioElement("gender", "male", !initialWithFemale))
    elements.append(new TextElement("男性", false))
    elements.append(new RadioElement("gender", "female", initialWithFemale))
    elements.append(new TextElement("女性", false))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
      backElements.append(new InputElement("hidden", key, value))
    }

    backElements.append(new InputElement("submit", "", "back"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/message", "post", elements.toArray),
        new FormElement("/form/name", "post", backElements.toArray)
      )
    )
    Ok(resBody.toString())
  }

  def messageForm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)
    val params = reqBody.parseParams()

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    val backElements: ArrayBuffer[Element] = new ArrayBuffer[Element]()

    elements.append(new TextElement("メッセージ:", true))
    elements.append(
      new TextAreaElement("message", params.getOrElse("message", ""), false)
    )
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    for ((key, value) <- reqBody.parseParams()) {
      elements.append(new InputElement("hidden", key, value))
      backElements.append(new InputElement("hidden", key, value))
    }

    backElements.append(new InputElement("submit", "", "back"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/confirm", "post", elements.toArray),
        new FormElement("/form/gender", "post", backElements.toArray)
      )
    )
    Ok(resBody.toString())
  }

  def confirm(body: Option[String]): Response = {
    val reqBody: RequestBody = new RequestBody(body)
    val params = reqBody.parseParams()

    val name = params.getOrElse("name", "")
    val gender = params.getOrElse("gender", "")
    val message = params.getOrElse("message", "")

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    val backElements: ArrayBuffer[Element] = new ArrayBuffer[Element]()

    elements.append(new TextElement(s"名前:${name}", true))
    elements.append(new TextElement(s"性別:${gender}", true))
    elements.append(new TextElement(s"メッセージ:", true))
    elements.append(new TextAreaElement("message", message, true))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "submit"))

    for ((key, value) <- params) {
      elements.append(new InputElement("hidden", key, value))
      backElements.append(new InputElement("hidden", key, value))
    }

    backElements.append(new InputElement("submit", "", "back"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/finish", "post", elements.toArray),
        new FormElement("/form/message", "post", backElements.toArray)
      )
    )
    Ok(resBody.toString())
  }

  def finish(): Response = {
    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("送信しました", false))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "back to top"))
    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/", "get", elements.toArray)
      ) // indexにうまく飛べない
    )
    Ok(resBody.toString())
  }
}
