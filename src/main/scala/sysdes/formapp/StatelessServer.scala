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
      case Request("GET", "/", _, _, _) => index()
      case _ =>
        NotFound(
          s"Requested resource '${request.path}' for ${request.method} is not found."
        )
    }

  def index(): Response = {
    val inputData: ArrayBuffer[InputData] = new ArrayBuffer[InputData]
    inputData.+=(InputData("submit", "", "start"))
    val resBody = new ResponseBody(
      "/form",
      "get",
      "アンケート開始<br>",
      inputData.toArray
    )
    Ok(resBody.toString())
  }

}
