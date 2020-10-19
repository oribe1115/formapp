package sysdes.formapp.server

import scala.collection.mutable

class ResponseBody(
    formAction: String,
    formMethod: String,
    text: String,
    input: Array[InputData]
) {
  override def toString() = {
    val inputTags: StringBuilder = new StringBuilder()
    for (data <- input) {
      data.inputType match {
        case "submit" => {
          inputTags.append(s"""<input type="submit" value="${data.value}" />""")
        }
        case _ => {
          inputTags.append(
            s"""<input type="${data.inputType}" name="${data.name}" value="${data.value}" />"""
          )
        }
      }

    }

    val body = s"""<html>
                 |<body>
                 |    <form action="${formAction}" method="${formMethod}">
                 |        ${text}
                 |        ${inputTags.toString()}
                 |    </form>
                 |</body>
                 |</html>""".stripMargin

    body
  }
}

case class InputData(
    inputType: String,
    name: String,
    value: String
)
