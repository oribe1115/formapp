package sysdes.formapp.server

import scala.collection.mutable

class ResponseBody(
    formAction: String,
    formMethod: String,
    elements: Array[Element]
) {
  override def toString() = {
    val elementsForOutput: StringBuilder = new StringBuilder()
    for (e <- elements) {
      elementsForOutput.append(e.formatOutput)
    }

    val body = s"""<html>
                 |<body>
                 |    <form action="${formAction}" method="${formMethod}">
                          ${elementsForOutput.toString()}
                 |    </form>
                 |</body>
                 |</html>""".stripMargin

    body
  }
}

abstract class ElementBase {
  def toHTMLElemt(): String
}

trait Element extends ElementBase {
  def formatOutput = s"|        ${toHTMLElemt()}\n"
}

class TextElement(text: String, afterBreak: Boolean) extends ElementBase with Element {
  def toHTMLElemt(): String = if (afterBreak) text + "<br>" else text
}

class InputElement(inputType: String, name: String, value: String) extends ElementBase with Element {
  def toHTMLElemt(): String = {
    inputType match {
      case "submit" => s"""<input type="submit" value="${value}" />"""
      case _ =>
        s"""<input type="${inputType}" name="${name}" value="${value}" />"""
    }
  }
}
