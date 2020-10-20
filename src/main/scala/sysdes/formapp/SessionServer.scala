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

  def newUser(): UUID = {
    val sessionID          = UUID.randomUUID()
    val formData: FormData = FormData("", "", "")
    userData.put(sessionID, formData)
    sessionID
  }

  def updateFormData(sessionID: UUID, formData: FormData): Unit = {
    userData.update(sessionID, formData)
  }

  def getFormData(sessionID: UUID): Option[FormData] = {
    userData.get(sessionID)
  }
}

class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{
    Element,
    FormElement,
    InputElement,
    NotFound,
    Ok,
    Request,
    Response,
    ResponseBody,
    TextAreaElement,
    TextElement
  }

  def handle(request: Request): Response =
    request match {
      case Request("GET", "/", _, _, _) => index()
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
    res.addCookie("sessionID", sessionID.toString())

    res
  }

}

case class FormData(
    var name: String,
    var gender: String,
    var message: String
)
