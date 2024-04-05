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
  def toAST: String =
    s"Match(List(${left.map(_.toAST).mkString(", ")}), pos, List(${right.map(_.toAST).mkString(", ")}))"
}
object Match
    extends generic.ParserBridge3[List[Instruction], Pos, List[
      Instruction
    ], Match]

sealed trait Instruction {
  val pos: Pos
  def toPattern: String
  def toAST: String
}

case class Block(pos: Pos, contents: List[Instruction]) extends Instruction {
  override def toString: String = s"(${contents.mkString(" ")})"
  def toPattern: String =
    s"Block(_, List(${contents.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"Block(pos, List(${contents.map(_.toAST).mkString(", ")}))"
}
object Block extends generic.ParserBridge2[Pos, List[Instruction], Block]

case class AddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!${n}"
  def toPattern: String = s"AddToStackItem(_, ${n})"
  def toAST: String = s"AddToStackItem(pos, ${n})"
}
object AddToStackItem extends generic.ParserBridge2[Pos, Int, AddToStackItem]

case class NewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"?!${n}"
  def toPattern: String = s"NewStackItem(_, ${n})"
  def toAST: String = s"NewStackItem(pos, ${n})"
}
object NewStackItem extends generic.ParserBridge2[Pos, Int, NewStackItem]

case class DeferAddToStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!.${n}"
  def toPattern: String = s"DeferAddToStackItem(_, ${n})"
  def toAST: String = s"DeferAddToStackItem(pos, ${n})"
}

case class DeferNewStackItem(pos: Pos, n: Int) extends Instruction {
  override def toString: String = s"!.${n}?."
  def toPattern: String = s"DeferNewStackItem(_, ${n})"
  def toAST: String = s"DeferNewStackItem(pos, ${n})"
}

case class Call(pos: Pos, name: Ident) extends Instruction {
  override def toString: String = s"$name"
  def toPattern: String = s"Call(_, ${name.toPattern})"
  def toAST: String = s"Call(pos, ${name.toAST})"
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
    s"ResolvedCall(_, ${name.toPattern}, List(${funcParams
        .map(ps => s"List(${ps.map(_.toPattern).mkString(", ")})")
        .mkString(", ")}))"
  def toAST: String =
    s"ResolvedCall(pos, ${name.toAST}, List(${funcParams
        .map(ps => s"List(${ps.map(_.toAST).mkString(", ")})")
        .mkString(", ")}))"
}

sealed trait RecursionLevel
case object NoRecursion extends RecursionLevel
case object DeferTailRecursion extends RecursionLevel
case object TailRecursion extends RecursionLevel

case class Loop(
    pos: Pos,
    contents: List[Instruction]
) extends Instruction {
  override def toString: String = s"{${contents.mkString(" ")}}"
  def toPattern: String =
    s"Loop(_, List(${contents.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"Loop(pos, List(${contents.map(_.toAST).mkString(", ")}))"
}
object Loop
    extends generic.ParserBridge2[Pos, List[
      Instruction
    ], Loop]

case class DeferTailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"[${contents.mkString(" ")}]"
  def toPattern: String =
    s"DeferTailRecLoop(_, List(${contents.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"DeferTailRecLoop(pos, List(${contents.map(_.toAST).mkString(", ")}))"
}

case class TailRecLoop(pos: Pos, contents: List[Instruction])
    extends Instruction {
  override def toString: String = s"<${contents.mkString(" ")}>"
  def toPattern: String =
    s"TailRecLoop(_, List(${contents.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"TailRecLoop(pos, List(${contents.map(_.toAST).mkString(", ")}))"
}

case class Continue(pos: Pos, recurseLevel: Int) extends Instruction {
  override def toString: String = s",${"." * recurseLevel}"
  def toPattern: String = s"Continue(_, $recurseLevel)"
  def toAST: String = s"Continue(pos, $recurseLevel)"
}
object Continue extends generic.ParserBridge2[Pos, Int, Continue]

case class Print(pos: Pos) extends Instruction {
  override def toString: String = s"#"
  def toPattern: String = s"Print(_)"
  def toAST: String = s"Print(pos)"
}
object Print extends generic.ParserBridge1[Pos, Print]

// case class Inspect(pos: Pos) extends Instruction {
//   override def toString: String = s"`"
// }
// object Inspect extends generic.ParserBridge1[Pos, Inspect]

case class Pop(pos: Pos) extends Instruction {
  override def toString: String = s"$$pop"
  def toPattern: String = s"Pop(_)"
  def toAST: String = s"Pop(pos)"
}

case class RecPop(pos: Pos, instructions: List[Instruction])
    extends Instruction {
  override def toString: String = s"$$rec_pop(${instructions.mkString(" ")})"
  def toPattern: String =
    s"RecPop(_, List(${instructions.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"RecPop(pos, List(${instructions.map(_.toAST).mkString(", ")}))"
}

case class DeferPop(pos: Pos, instructions: List[Instruction])
    extends Instruction {
  override def toString: String = s"$$defer_pop(${instructions.mkString(" ")})"
  def toPattern: String =
    s"DeferPop(_, List(${instructions.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"DeferPop(pos, List(${instructions.map(_.toAST).mkString(", ")}))"
}

case class Add(pos: Pos) extends Instruction {
  override def toString: String = s"$$add"
  def toPattern: String = s"Add(_)"
  def toAST: String = s"Add(pos)"
}

case class Subtract(pos: Pos) extends Instruction {
  override def toString: String = s"$$sub"
  def toPattern: String = s"Subtract(_)"
  def toAST: String = s"Subtract(pos)"
}

case class Multiply(pos: Pos) extends Instruction {
  override def toString: String = s"$$mul"
  def toPattern: String = s"Multiply(_)"
  def toAST: String = s"Multiply(pos)"
}

case class DivMod(pos: Pos) extends Instruction {
  override def toString: String = s"$$divmod"
  def toPattern: String = s"DivMod(_)"
  def toAST: String = s"DivMod(pos)"
}

case class Clone(pos: Pos) extends Instruction {
  override def toString: String = s"$$clone"
  def toPattern: String = s"Clone(_)"
  def toAST: String = s"Clone(pos)"
}

case class Swap(pos: Pos) extends Instruction {
  override def toString: String = s"$$swap"
  def toPattern: String = s"Swap(_)"
  def toAST: String = s"Swap(pos)"
}

case class Skip(pos: Pos, instructions: List[Instruction]) extends Instruction {
  override def toString: String = s"$$skip(${instructions.mkString(" ")})"
  def toPattern: String =
    s"Skip(_, List(${instructions.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"Skip(pos, List(${instructions.map(_.toAST).mkString(", ")}))"
}

case class DeferSkip(pos: Pos, instructions: List[Instruction])
    extends Instruction {
  override def toString: String = s"$$defer_skip(${instructions.mkString(" ")})"
  def toPattern: String =
    s"DeferSkip(_, List(${instructions.map(_.toPattern).mkString(", ")}))"
  def toAST: String =
    s"DeferSkip(pos, List(${instructions.map(_.toAST).mkString(", ")}))"
}
