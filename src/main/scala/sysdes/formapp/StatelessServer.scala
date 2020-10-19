package sysdes.formapp

import java.net.Socket
import sysdes.formapp.server.{Handler, Server}
import scala.collection.mutable.ArrayBuffer

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{InputData, NotFound, Ok, Request, Response, ResponseBody}

  override def handle(request: Request): Response =
    request match {
      case Request("GET", "/", _, _, _)             => index()
      case Request("POST", "/form/name", _, _, _)   => nameForm()
      case Request("POST", "/form/gender", _, _, _) => genderForm()
      case _ =>
        NotFound(
          s"Requested resource '${request.path}' for ${request.method} is not found."
        )
    }

  def index(): Response = {
    val inputData: ArrayBuffer[InputData] = new ArrayBuffer[InputData]
    inputData += (InputData("submit", "", "start"))
    val resBody = new ResponseBody(
      "/form/name",
      "post",
      "アンケート開始<br>",
      inputData.toArray
    )
    Ok(resBody.toString())
  }

  def nameForm(): Response = {
    val inputData: ArrayBuffer[InputData] = new ArrayBuffer[InputData]
    inputData += (InputData("text", "name", ""))
    inputData += (InputData("submit", "", "送信"))
    val resBody = new ResponseBody(
      "/form/gender",
      "post",
      "名前:",
      inputData.toArray
    )
    Ok(resBody.toString())
  }

  def genderForm(): Response = {
    val inputData: ArrayBuffer[InputData] = new ArrayBuffer[InputData]
    inputData += (InputData("radio", "gender", "male"))
    inputData += (InputData("radio", "gender", "female"))
    inputData += (InputData("submit", "", "送信"))
    val resBody = new ResponseBody(
      "/form/message",
      "post",
      "性別:",
      inputData.toArray
    )
    Ok(resBody.toString())
  }

}
