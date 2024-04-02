package ast

import parsley.generic

case class Match(
    left: List[Instruction],
    pos: Pos,
    right: List[Instruction]
) extends Instruction {
  override def toString: String =
    s"${left.mkString(" ")} : ${right.mkString(" ")}"
}
object Match
    extends generic.ParserBridge3[List[Instruction], Pos, List[
      Instruction
    ], Match]

sealed trait Instruction {
  val pos: Pos
}

case class Block(pos: Pos, contents: List[Instruction]) extends Instruction {
  override def toString: String = s"(${contents.mkString(" ")})"
}
object Block extends generic.ParserBridge2[Pos, List[Instruction], Block]

case class AddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!${n}"
}
object AddToStackItem extends generic.ParserBridge2[Pos, Int, AddToStackItem]

case class NewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"?!${n}"
}
object NewStackItem extends generic.ParserBridge2[Pos, Int, NewStackItem]

case class DeferAddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!.${n}"
}

case class DeferNewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"?.!.${n}"
}

case class Call(pos: Pos, name: Ident) extends Instruction {
  override def toString: String = s"$name"
}
object Call extends generic.ParserBridge2[Pos, Ident, Call]

case class ResolvedCall(
    pos: Pos,
    name: Ident,
    funcParams: List[List[Instruction]]
) extends Instruction {
  override def toString: String =
    s"${(name :: funcParams.map(_.toString)).mkString(" ")}"
}

sealed trait RecursionLevel
case object NoRecursion extends RecursionLevel
case object DeferTailRecursion extends RecursionLevel
case object TailRecursion extends RecursionLevel

case class Loop(
    pos: Pos,
    contents: List[Instruction],
    optimisationLevel: RecursionLevel
) extends Instruction {
  override def toString: String = s"{${contents.mkString(" ")}}"
}
object Loop
    extends generic.ParserBridge3[Pos, List[
      Instruction
    ], RecursionLevel, Loop]

case class DeferTailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"[${contents.mkString(" ")}]"
}

case class TailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"<${contents.mkString(" ")}>"
}

case class Continue(pos: Pos, recurseLevel: Int) extends Instruction {
  override def toString: String = s",${"." * recurseLevel}"
}
object Continue extends generic.ParserBridge2[Pos, Int, Continue]

case class Print(pos: Pos) extends Instruction {
  override def toString: String = s"#"
}
object Print extends generic.ParserBridge1[Pos, Print]

// case class Inspect(pos: Pos) extends Instruction {
//   override def toString: String = s"`"
// }
// object Inspect extends generic.ParserBridge1[Pos, Inspect]
