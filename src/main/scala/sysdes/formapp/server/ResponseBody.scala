package sysdes.formapp.server

class ResponseBody(
    formAction: String,
    formMethod: String,
    text: String,
    input: Array[InputData]
) {
  override def toString() = {
    // """<html>
    //   |<body>
    //   |    <form action="/action" method="get">
    //   |        アンケート開始
    //   |        <input type="submit" value="start" />
    //   |    </form>
    //   |</body>
    //   |</html>""".stripMargin

    val base: StringBuilder = new StringBuilder()
    base.append("<html><body>")
    base.append(
      "<form action=\"" + formAction + "\"  method=\"" + formMethod + "\">"
    )
    base.append(text)
    for (data <- input) {
      data.inputType match {
        case "submit" => {
          base.append("<input type=\"submit\" value=\"" + data.value + "\" />")
        }
        case _ => {
          base.append(
            "<input type=\"" + data.inputType + "\" name=\"" + data.name + "\" value=\"" + data.value + "\" />"
          )
        }
      }
    }
    base.append("</form>")
    base.append("</body></html>")

    base.toString()
  }
}

case class InputData(
    inputType: String,
    name: String,
    value: String
)
