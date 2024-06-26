package optimiser

import ast._

object chain {
  def mergeOnesBlock(
      instructions: List[Instruction],
      current: List[Instruction] = List()
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case AddToStackItem(pos, n) :: AddToStackItem(_, m) :: rest => {
        mergeOnesBlock(AddToStackItem(pos, n + m) :: rest, current)
      }
      case NewStackItem(pos, n) :: AddToStackItem(_, m) :: rest => {
        mergeOnesBlock(NewStackItem(pos, n + m) :: rest, current)
      }
      case Match(left, pos, right) :: rest => {
        val newLeft = mergeOnesBlock(left)
        val newRight = mergeOnesBlock(right)
        mergeOnesBlock(rest, current :+ Match(newLeft, pos, newRight))
      }
      case Loop(pos, instructions) :: rest => {
        val newInstructions = mergeOnesBlock(instructions)
        mergeOnesBlock(rest, current :+ Loop(pos, newInstructions))
      }
      case i :: rest => {
        mergeOnesBlock(rest, current :+ i)
      }
    }
  }

  def mergeOnes(
      program: Program
  ): Program = {
    Program(
      program.packageName,
      program.imports,
      mergeOnesBlock(program.instructions),
      program.funcs.map(f =>
        Func(
          f.pos,
          f.name,
          f.funcParams,
          f.export,
          mergeOnesBlock(f.instructions)
        )
      )
    )
  }
}
