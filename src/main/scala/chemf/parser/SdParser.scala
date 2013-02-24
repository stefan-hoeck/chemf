package chemf.parser

class SdParser {
}

object SdParser {
  private val d = "[\\d ]" //digit or space
  private val d3 = s"($d$d$d)" //3 digits

  //aaabbblllfffcccsssxxxrrrpppiiimmmvvvvvv
  //             a  b  l  f  c  s  x  r  p  i  m  v
  val counts = s"$d3$d3$d3$d3$d3$d3$d3$d3$d3$d3$d3 V([23])000".r
}

// vim: set ts=2 sw=2 et:
