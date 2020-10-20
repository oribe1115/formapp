package sysdes.formapp.server

import scala.collection.mutable.HashMap
import java.net.URLDecoder

case class Request(
    method: String,
    path: String,
    version: String,
    headers: HashMap[String, String] = HashMap(),
    var body: Option[String] = None
)

object Request {
  import java.io.BufferedReader

  def parse(in: BufferedReader): Option[Request] = {
    // リクエスト行をパース
    // 正常リクエストであれば，リクエストオブジェクトを構築
    val requestPattern = """(.*?)\s+(.*?)\s+(.*?)$""".r
    in.readLine match {
      case requestPattern(method, path, version) =>
        Some(Request(method, path, version))
      case _ => None
    }
  }.map(request => {
    // ヘッダ情報の取得
    val headerPattern = """(.*?):\s?(.*)$""".r
    for (line <- Iterator.continually(in.readLine).takeWhile(!_.isEmpty)) {
      line match {
        case headerPattern(key, value) => request.headers.put(key, value)
      }
    }
    // ボディ情報の取得（"Content-Length"に指定されたバイト数分のみ取得する）
    for (bodyLength <- request.headers.get("Content-Length").map(_.toInt)
         if bodyLength > 0) {
      val buffer = new Array[Char](bodyLength)
      in.read(buffer) match {
        case i if i != -1 => request.body = Some(String.valueOf(buffer))
      }
    }
    request
  })
}

class RequestBody(body: Option[String]) {
  def parseParams(): HashMap[String, String] = {
    body match {
      case Some(b) => {
        val pairs                           = b.split("&")
        val params: HashMap[String, String] = HashMap()
        for (pair <- pairs) {
          val p = pair.split("=")
          if (p.length > 1) {
            val key   = URLDecoder.decode(p(0))
            val value = URLDecoder.decode(p(1))
            params.put(key, value)
          }
        }

        params
      }
      case None => HashMap.empty
    }
  }
}

class RequestHeader(header: HashMap[String, String]) {
  def getFromCookie(key: String): Option[String] = {
    val cookie                            = header.get("Cookie")
    val pairsMap: HashMap[String, String] = HashMap()

    cookie match {
      case Some(c) => {
        val pairs = c.split(" ;")
        for (pair <- pairs) {
          val p = pair.split("=")
          p.length match {
            case 2 => {
              val key   = URLDecoder.decode(p(0))
              val value = URLDecoder.decode(p(1))
              pairsMap.put(key, value)
            }
          }
        }
      }
      case None => None
    }

    val target = pairsMap.get(key)
    target match {
      case Some(t) => Some(t)
      case None    => None
    }
  }
}
