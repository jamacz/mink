package ast

import parsley.generic

case class Program(
    packageName: Option[Ident],
    imports: List[Ident],
    instructions: List[Instruction],
    funcs: List[Func]
) {
  override def toString: String =
    s"${instructions.mkString(" ")}\n${funcs.mkString("\n")}"

  def toPattern: String =
    s"MAIN: ${instructions.map(_.toPattern).mkString("::")})::Nil\n\n" +
      funcs
        .map(f =>
          s"${f.name}: ${f.instructions.map(_.toPattern).mkString("::")}::Nil\n\n"
        )
        .mkString

}
object Program
    extends generic.ParserBridge4[Option[Ident], List[Ident], List[
      Instruction
    ], List[
      Func
    ], Program]

case class Func(
    pos: Pos,
    name: Ident,
    funcParams: Option[List[FuncParam]],
    export: Boolean,
    instructions: List[Instruction]
) {
  override def toString: String =
    s"$name${funcParams.map(fs => s" $$ ${fs.mkString(" ")}").getOrElse(" ")}${if (export) "?"
      else ""}= ${instructions.mkString(" ")}"
}
object Func
    extends generic.ParserBridge5[Pos, Ident, Option[List[
      FuncParam
    ]], Boolean, List[
      Instruction
    ], Func]

case class FuncParam(pos: Pos, name: String) {
  override def toString: String = name
}
object FuncParam extends generic.ParserBridge2[Pos, String, FuncParam]

case class Ident(pos: Pos, name: List[String]) {
  override def toString: String = name.mkString(".")
  override def equals(that: Any): Boolean = that match {
    case Ident(_, n) => n == name
    case _           => false
  }

  override def hashCode(): Int = name.hashCode()
}
object Ident extends generic.ParserBridge2[Pos, List[String], Ident]
