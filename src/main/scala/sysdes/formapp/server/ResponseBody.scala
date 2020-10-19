package sysdes.formapp.server

import scala.collection.mutable

class ResponseBody(
    elements: Array[Element]
) {
  override def toString() = {
    val elementsForOutput: StringBuilder = new StringBuilder()
    for (e <- elements) {
      elementsForOutput.append(e.formatOutput)
    }

    s"""<html>
      |<body>
          ${elementsForOutput.toString()}
      |</body>
      |</html>""".stripMargin

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

class RadioElement(name: String, value: String, checked: Boolean) extends ElementBase with Element {
  def toHTMLElemt(): String = {
    checked match {
      case true =>
        s"""<input type="radio" name="${name}" value="${value}" checked="checked" />"""
      case _ => s"""<input type="radio" name="${name}" value="${value}" />"""
    }
  }
}

class TextAreaElement(name: String, value: String, disabled: Boolean) extends ElementBase with Element {
  def toHTMLElemt(): String = {
    if (disabled) {
      s"""<textarea name="${name}" disabled>${value}</textarea>"""
    } else {
      s"""<textarea name="${name}">${value}</textarea>"""
    }
  }
}

class FormElement(action: String, method: String, elements: Array[Element]) extends ElementBase with Element {
  val top    = s"""<form action="${action}" method="${method}">"""
  val bottom = "</form>"

  def toHTMLElemt(): String = {
    val buffer: StringBuilder = new StringBuilder()
    buffer.append(top)
    for (element <- elements) {
      buffer.append(element.toHTMLElemt())
    }
    buffer.append(bottom)
    buffer.toString()
  }

  override def formatOutput: String = {
    val buffer: StringBuilder = new StringBuilder()
    buffer.append(s"|    ${top}\n")
    for (element <- elements) {
      buffer.append(element.formatOutput)
    }
    buffer.append(s"|    ${bottom}\n")
    buffer.toString()
  }
}
