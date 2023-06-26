package aholland.scala.utils

object StringUtils:
 def indent(indents: Int): String = "   " * indents
 def columnPad(s: String, colWidth: Int, fillChar: Char = ' '): String = fillChar.toString * (colWidth - s.length) + s
 //TODO improve naming
 def columnPadLJ(s: String, colWidth: Int): String = s + " " * (colWidth - s.length)
 val toEmptyString: Any => String = _ => ""
 def removeBrackets(s: String): String = s.replaceAll("[()]", "")
 def toLines(s: String): List[String] = s.split("\\r?\\n").toList
 def isMultiLine(s: String): Boolean = 1 < toLines(s).length //TODO smarter implementation?
 def trunc(s: String, index: Int): String = s.substring(0, Math.max(0, Math.min(s.length - 1, index)))
 def concatenator(separator: String): (String, String) => String =
  (a: String, b: String) => a + separator + b
 def concord(number: Long, singularForm: String, pluralForm: String): String = if number.abs == 1 then singularForm else pluralForm
 def doubleQuote(s: String): String = "\"" + s + "\""
 def filterOut(s: String, chars: String): String = s.filterNot(sChar => chars.indexOf(sChar) > -1)
