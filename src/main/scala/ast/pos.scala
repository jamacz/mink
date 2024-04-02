package ast

case class Pos(filename: String, line: Int, column: Int) {
  override def toString: String = s"$filename($line:$column)"
}
