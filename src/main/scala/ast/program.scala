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

  def toAST: String =
    s"Program(${packageName.map(_.toAST)}, List(${imports
        .map(_.toAST)
        .mkString(", ")}), List(${instructions.map(_.toAST).mkString(", ")}), List(\n" +
      funcs
        .map(f =>
          s"Func(pos, ${f.name.toAST}, ${f.funcParams.map(p =>
              s"List(${p.map(_.toAST).mkString(", ")})"
            )}, ${f.export}, List(${f.instructions.map(_.toAST).mkString(", ")}))\n\n"
        )
        .mkString + ")\n)"

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
  def toAST = s"FuncParam(pos, \"$name\")"
}
object FuncParam extends generic.ParserBridge2[Pos, String, FuncParam]

case class Ident(pos: Pos, name: List[String]) {
  override def toString: String = name.mkString(".")
  def toPattern: String =
    s"Ident(_, List(${name.map(s => s"\"$s\"").mkString(", ")}))"
  def toAST: String =
    s"Ident(pos, List(${name.map(s => s"\"$s\"").mkString(", ")}))"
  override def equals(that: Any): Boolean = that match {
    case Ident(_, n) => n == name
    case _           => false
  }

  override def hashCode(): Int = name.hashCode()
}
object Ident extends generic.ParserBridge2[Pos, List[String], Ident]
