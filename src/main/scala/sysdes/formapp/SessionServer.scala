package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}
import scala.collection.mutable.HashMap
import java.util.UUID
import scala.collection.mutable.ArrayBuffer

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  private val userData: HashMap[UUID, FormData] = HashMap()

  def newUser(): String = {
    val sessionID          = UUID.randomUUID()
    val formData: FormData = FormData("", "", "")
    userData.put(sessionID, formData)
    sessionID.toString()
  }

  def updateFormData(sessionID: String, formData: FormData): Unit = {
    userData.update(UUID.fromString(sessionID), formData)
  }

  def getFormData(sessionID: String): Option[FormData] = {
    userData.get(UUID.fromString(sessionID))
  }
}

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server._

  def handle(request: Request): Response =
    request match {
      case Request("GET", "/", _, _, _)  => index()
      case Request("POST", "/", _, _, _) => index()
      case Request("POST", "/form/name", _, header, body) =>
        middleware(header, body, nameForm)
      case Request("POST", "/form/gender", _, header, body) =>
        middleware(header, body, genderForm)
      case Request("POST", "/form/message", _, header, body) =>
        middleware(header, body, messageForm)
      case Request("POST", "/form/confirm", _, header, body) =>
        middleware(header, body, confirm)
      case Request("POST", "/form/finish", _, _, _) =>
        finish()
      case _ =>
        NotFound(
          s"Requested resource '${request.path}' for ${request.method} is not found."
        )
    }

  def index(): Response = {
    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("アンケート開始", true))
    elements.append(new InputElement("submit", "", "start"))
    val resBody = new ResponseBody(
      Array[Element](new FormElement("/form/name", "post", elements.toArray))
    )

    val res: Response = Ok(resBody.toString())
    val sessionID     = SessionServerHandler.newUser()
    res.addCookie("sessionID", sessionID)

    res
  }

  def nameForm(
      sessionID: String,
      formData: FormData,
      params: HashMap[String, String]
  ): Response = {
    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("名前:", false))
    elements.append(
      new InputElement("text", "name", formData.name)
    )
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/gender", "post", elements.toArray),
        new FormElement(
          "/",
          "post",
          Array[Element](new InputElement("submit", "", "back"))
        )
      )
    )

    Ok(resBody.toString())
  }

  def genderForm(
      sessionID: String,
      formData: FormData,
      params: HashMap[String, String]
  ): Response = {
    val name = params.getOrElse("name", "")
    if (name != "") {
      formData.name = name
      SessionServerHandler.updateFormData(sessionID, formData)
    }

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    val initialWithFemale              = (formData.gender == "female")

    elements.append(new TextElement("性別:", false))
    elements.append(new RadioElement("gender", "male", !initialWithFemale))
    elements.append(new TextElement("男性", false))
    elements.append(new RadioElement("gender", "female", initialWithFemale))
    elements.append(new TextElement("女性", false))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/message", "post", elements.toArray),
        new FormElement(
          "/form/name",
          "post",
          Array[Element](new InputElement("submit", "", "back"))
        )
      )
    )
    Ok(resBody.toString())
  }

  def messageForm(
      sessionID: String,
      formData: FormData,
      params: HashMap[String, String]
  ): Response = {
    val gender = params.getOrElse("gender", "")
    if (gender != "") {
      formData.gender = gender
      SessionServerHandler.updateFormData(sessionID, formData)
    }

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()

    elements.append(new TextElement("メッセージ:", true))
    elements.append(
      new TextAreaElement("message", formData.message, false)
    )
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "next"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/confirm", "post", elements.toArray),
        new FormElement(
          "/form/gender",
          "post",
          Array[Element](new InputElement("submit", "", "back"))
        )
      )
    )
    Ok(resBody.toString())
  }

  def confirm(
      sessionID: String,
      formData: FormData,
      params: HashMap[String, String]
  ): Response = {
    val message = params.getOrElse("message", "")
    if (message != "") {
      formData.message = message
      SessionServerHandler.updateFormData(sessionID, formData)
    }

    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement(s"名前:${formData.name}", true))
    elements.append(new TextElement(s"性別:${formData.gender}", true))
    elements.append(new TextElement(s"メッセージ:", true))
    elements.append(new TextAreaElement("message", formData.message, true))
    elements.append(new TextElement("", true))
    elements.append(new InputElement("submit", "", "submit"))

    val resBody = new ResponseBody(
      Array[Element](
        new FormElement("/form/finish", "post", elements.toArray),
        new FormElement(
          "/form/message",
          "post",
          Array[Element](new InputElement("submit", "", "back"))
        )
      )
    )
    Ok(resBody.toString())
  }

  def finish(): Response = {
    val resBody = new ResponseBody(
      Array[Element](
        new TextElement("送信しました", true),
        new HrefElement("/", "back to top")
      )
    )
    Ok(resBody.toString())
  }

  def sessionIDNotFound(): Response = {
    val elements: ArrayBuffer[Element] = new ArrayBuffer[Element]()
    elements.append(new TextElement("セッションIDが確認できませんでした", true))
    elements.append(new TextElement("初めから回答しなおしてください", true))
    elements.append(new HrefElement("/", "back to top"))

    val resBody = new ResponseBody(
      elements.toArray
    )

    BadRequest(resBody.toString())
  }

  def getSessionIDAndFormData(
      reqHeader: RequestHeader
  ): (Option[String], Option[FormData]) = {
    reqHeader.getFromCookie("sessionID") match {
      case Some(sessionID) => {
        SessionServerHandler.getFormData(sessionID) match {
          case Some(formData) => {
            (Some(sessionID), Some(formData))
          }
          case None => (Some(sessionID), None)
        }
      }
      case None => (None, None)
    }
  }

  def middleware(
      header: HashMap[String, String],
      body: Option[String],
      mainFunc: (String, FormData, HashMap[String, String]) => Response
  ): Response = {
    val reqHeader            = new RequestHeader(header)
    val reqBody: RequestBody = new RequestBody(body)
    val params               = reqBody.parseParams()

    getSessionIDAndFormData(reqHeader) match {
      case (Some(sessionID), Some(formData)) => {
        mainFunc(sessionID, formData, params)
      }
      case (_, _) => sessionIDNotFound()
    }
  }
}

case class FormData(
    var name: String,
    var gender: String,
    var message: String
)
