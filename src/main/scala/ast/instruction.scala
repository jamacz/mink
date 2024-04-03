package ast

import parsley.generic

case class Match(
    left: List[Instruction],
    pos: Pos,
    right: List[Instruction]
) extends Instruction {
  override def toString: String =
    s"(${left.mkString(" ")} : ${right.mkString(" ")})"
  def toPattern: String =
    s"Match(List(${left.map(_.toPattern).mkString(", ")}), _, List(${right.map(_.toPattern).mkString(", ")}))"
}
object Match
    extends generic.ParserBridge3[List[Instruction], Pos, List[
      Instruction
    ], Match]

sealed trait Instruction {
  val pos: Pos
  def toPattern: String
}

case class Block(pos: Pos, contents: List[Instruction]) extends Instruction {
  override def toString: String = s"(${contents.mkString(" ")})"
  def toPattern: String =
    s"Block(_, List(${contents.map(_.toPattern).mkString(", ")}))"
}
object Block extends generic.ParserBridge2[Pos, List[Instruction], Block]

case class AddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!${n}"
  def toPattern: String = s"AddToStackItem(_, ${n})"
}
object AddToStackItem extends generic.ParserBridge2[Pos, Int, AddToStackItem]

case class NewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"?!${n}"
  def toPattern: String = s"NewStackItem(_, ${n})"
}
object NewStackItem extends generic.ParserBridge2[Pos, Int, NewStackItem]

case class DeferAddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!.${n}"
  def toPattern: String = s"DeferAddToStackItem(_, ${n})"
}

case class DeferNewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!.${n}?."
  def toPattern: String = s"DeferNewStackItem(_, ${n})"
}

case class Call(pos: Pos, name: Ident) extends Instruction {
  override def toString: String = s"$name"
  def toPattern: String = s"Call(_, $name)"
}
object Call extends generic.ParserBridge2[Pos, Ident, Call]

case class ResolvedCall(
    pos: Pos,
    name: Ident,
    funcParams: List[List[Instruction]]
) extends Instruction {
  override def toString: String =
    s"${(name :: funcParams.map(_.toString)).mkString(" ")}"
  def toPattern: String =
    s"ResolvedCall(_, $name, List(${funcParams
        .map(ps => s"List(${ps.map(_.toPattern).mkString(", ")})")
        .mkString(", ")}))"
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
  def toPattern: String =
    s"Loop(_, List(${contents.map(_.toPattern).mkString(", ")}), _)"
}
object Loop
    extends generic.ParserBridge3[Pos, List[
      Instruction
    ], RecursionLevel, Loop]

case class DeferTailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"[${contents.mkString(" ")}]"
  def toPattern: String =
    s"DeferTailRecLoop(_, List(${contents.map(_.toPattern).mkString(", ")}))"
}

case class TailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"<${contents.mkString(" ")}>"
  def toPattern: String =
    s"TailRecLoop(_, List(${contents.map(_.toPattern).mkString(", ")}))"
}

case class Continue(pos: Pos, recurseLevel: Int) extends Instruction {
  override def toString: String = s",${"." * recurseLevel}"
  def toPattern: String = s"Continue(_, $recurseLevel)"
}
object Continue extends generic.ParserBridge2[Pos, Int, Continue]

case class Print(pos: Pos) extends Instruction {
  override def toString: String = s"#"
  def toPattern: String = s"Print(_)"
}
object Print extends generic.ParserBridge1[Pos, Print]

// case class Inspect(pos: Pos) extends Instruction {
//   override def toString: String = s"`"
// }
// object Inspect extends generic.ParserBridge1[Pos, Inspect]

case class SkipOnes(pos: Pos) extends Instruction {
  override def toString: String = s"$$skip"
  def toPattern: String = s"SkipOnes(_)"
}

case class Skip(pos: Pos, instrs: List[Instruction]) extends Instruction {
  override def toString: String = s"$$skip(${instrs.mkString(" ")})"
  def toPattern: String =
    s"Skip(_, List(${instrs.map(_.toPattern).mkString(", ")}))"
}

case class Pop(pos: Pos) extends Instruction {
  override def toString: String = s"$$pop"
  def toPattern: String = s"Pop(_)"
}

case class Zero(pos: Pos) extends Instruction {
  override def toString: String = s"$$zero"
  def toPattern: String = s"Zero(_)"
}

case class Add(pos: Pos) extends Instruction {
  override def toString: String = s"$$add"
  def toPattern: String = s"Add(_)"
}

case class Subtract(pos: Pos) extends Instruction {
  override def toString: String = s"$$sub"
  def toPattern: String = s"Subtract(_)"
}

case class Clone(pos: Pos) extends Instruction {
  override def toString: String = s"$$clone"
  def toPattern: String = s"Clone(_)"
}

case class Swap(pos: Pos) extends Instruction {
  override def toString: String = s"$$swap"
  def toPattern: String = s"Swap(_)"
}

case class Multiply(pos: Pos) extends Instruction {
  override def toString: String = s"$$mul"
  def toPattern: String = s"Multiply(_)"
}

case class DivMod(pos: Pos) extends Instruction {
  override def toString: String = s"$$divmod"
  def toPattern: String = s"DivMod(_)"
}
