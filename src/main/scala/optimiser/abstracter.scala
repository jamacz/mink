package optimiser

import ast._

object abstracter {
  def scanProgram(
      program: Program,
      optimisationLevel: Int
  ): Program = {
    if (optimisationLevel <= 1) return program
    Program(
      program.packageName,
      program.imports,
      scanFull(program.instructions, optimisationLevel),
      program.funcs.map(f =>
        Func(
          f.pos,
          f.name,
          f.funcParams,
          f.export,
          scanFull(f.instructions, optimisationLevel)
        )
      )
    )
  }

  def scanFull(
      instructions: List[Instruction],
      optimisationLevel: Int
  ): List[Instruction] = {
    val basic = scanBasic(instructions, List())
    if (optimisationLevel <= 2) return basic
    val intermediate = scanIntermediate(basic, List())
    if (optimisationLevel <= 3) return intermediate
    val advanced = scanAdvanced(intermediate, List())
    advanced
  }

  def getLastDefer(
      instructions: List[Instruction],
      current: List[Instruction]
  ): (Instruction, List[Instruction]) = {
    instructions match {
      case Nil =>
        current match {
          case h :: r => (h, r)
          case _      => throw new Exception("Unreachable")
        }
      case (d @ DeferNewStackItem(_, _)) :: rest =>
        getLastDefer(
          rest,
          d :: current
        )
      case (d @ DeferAddToStackItem(_, _)) :: rest =>
        getLastDefer(
          rest,
          d :: current
        )
      case i :: rest => {
        current match {
          case h :: r => (h, instructions.reverse ::: r)
          case _      => throw new Exception("Unreachable")
        }
      }
    }
  }

  def lastIsZero(instructions: List[Instruction]): Option[List[Instruction]] = {
    instructions.lastOption match {
      case Some(NewStackItem(_, 0)) => Some(instructions.dropRight(1))
      case Some(Match(left, pos, right)) =>
        lastIsZero(left) match {
          case Some(newLeft) =>
            lastIsZero(right) match {
              case Some(newRight) =>
                Some(instructions.dropRight(1) :+ Match(newLeft, pos, newRight))
              case None => None
            }
          case None => None
        }
      case Some(c @ Continue(_, _)) => {
        getLastDefer(
          instructions.dropRight(1).reverse,
          List(c)
        ) match {
          case (DeferNewStackItem(_, 0), newInstrs) =>
            Some(newInstrs)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def scanBasic(
      instructions: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case TailRecLoop(
            pos,
            List(Match(List(Continue(_, 0)), _, List()))
          ) :: rest => {
        scanBasic(
          rest,
          current :+ Pop(pos)
        )
      }
      case TailRecLoop(
            pos,
            List(Match(List(Continue(_, 0)), _, main))
          ) :: rest => {
        val newMain = scanBasic(main, List())
        scanBasic(
          rest,
          current :+ RecPop(pos, newMain)
        )
      }
      case DeferTailRecLoop(
            pos,
            List(Match(List(Continue(_, 0)), _, main))
          ) :: rest => {
        val newMain = scanBasic(main, List())
        scanBasic(
          rest,
          current :+ DeferPop(pos, newMain)
        )
      }
      case DeferTailRecLoop(
            pos,
            List(
              Match(List(DeferAddToStackItem(_, 1), Continue(_, 0)), _, List())
            )
          ) :: rest => {
        scanBasic(
          rest,
          current :+ Add(pos)
        )
      }
      case DeferTailRecLoop(
            pos,
            List(
              Match(List(DeferAddToStackItem(_, 1), Continue(_, 0)), _, main)
            )
          ) :: rest => {
        val newMain = scanBasic(main, List())
        lastIsZero(newMain) match {
          case Some(newMain) =>
            scanBasic(
              rest,
              current :+ Skip(pos, newMain)
            )
          case None =>
            scanBasic(
              rest,
              current :+ DeferSkip(pos, newMain)
            )
        }

      }
      case Loop(pos, instructions) :: rest => {
        val newInstructions = scanBasic(instructions, List())
        scanBasic(
          rest,
          current :+ Loop(pos, newInstructions)
        )
      }
      case DeferTailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanBasic(instructions, List())
        scanBasic(
          rest,
          current :+ DeferTailRecLoop(pos, newInstructions)
        )
      }
      case TailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanBasic(instructions, List())
        scanBasic(
          rest,
          current :+ TailRecLoop(pos, newInstructions)
        )
      }
      case Block(pos, instructions) :: rest => {
        val newInstructions = scanBasic(instructions, List())
        scanBasic(
          rest,
          current :+ Block(pos, newInstructions)
        )
      }
      case Match(left, pos, right) :: rest => {
        val newLeft = scanBasic(left, List())
        val newRight = scanBasic(right, List())
        scanBasic(
          rest,
          current :+ Match(newLeft, pos, newRight)
        )
      }
      case i :: rest => {
        scanBasic(
          rest,
          current :+ i
        )
      }
    }
  }

  def scanIntermediate(
      instructions: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case Skip(
            pos,
            List(NewStackItem(_, 0), NewStackItem(_, 0))
          ) :: TailRecLoop(
            _,
            List(
              Match(
                List(
                  Skip(
                    _,
                    List(
                      Skip(_, List(AddToStackItem(_, 1))),
                      AddToStackItem(_, 1)
                    )
                  ),
                  Continue(_, 0)
                ),
                _,
                List()
              )
            )
          ) :: rest => {
        scanIntermediate(
          rest,
          current :+ Clone(pos)
        )
      }
      case TailRecLoop(
            pos,
            List(
              Match(
                List(
                  Skip(
                    _,
                    List(
                      Match(
                        List(),
                        _,
                        List(NewStackItem(_, 0))
                      )
                    )
                  ),
                  Continue(_, 0)
                ),
                _,
                List()
              )
            )
          ) :: rest => {
        scanIntermediate(
          rest,
          current :+ Subtract(pos)
        )
      }
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) :: TailRecLoop(
            _,
            List(
              Match(
                List(
                  Skip(_, List(Skip(_, List(AddToStackItem(_, 1))))),
                  Continue(_, 0)
                ),
                _,
                List()
              )
            )
          ) :: rest => {
        scanIntermediate(
          rest,
          current :+ Swap(pos)
        )
      }
      case Skip(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ Skip(pos, newInstructions)
        )
      }
      case DeferSkip(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ DeferSkip(pos, newInstructions)
        )
      }
      case RecPop(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ RecPop(pos, newInstructions)
        )
      }
      case DeferPop(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ DeferPop(pos, newInstructions)
        )
      }
      case Loop(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ Loop(pos, newInstructions)
        )
      }
      case DeferTailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ DeferTailRecLoop(pos, newInstructions)
        )
      }
      case TailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ TailRecLoop(pos, newInstructions)
        )
      }
      case Block(pos, instructions) :: rest => {
        val newInstructions = scanIntermediate(instructions, List())
        scanIntermediate(
          rest,
          current :+ Block(pos, newInstructions)
        )
      }
      case Match(left, pos, right) :: rest => {
        val newLeft = scanIntermediate(left, List())
        val newRight = scanIntermediate(right, List())
        scanIntermediate(
          rest,
          current :+ Match(newLeft, pos, newRight)
        )
      }
      case i :: rest => {
        scanIntermediate(
          rest,
          current :+ i
        )
      }
    }
  }

  def scanAdvanced(
      instructions: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) ::
          TailRecLoop(
            _,
            List(
              Match(
                List(
                  Skip(_, List(Clone(_), Skip(_, List(Add(_))))),
                  Continue(_, 0)
                ),
                _,
                List(Pop(_))
              )
            )
          ) :: rest => {
        scanAdvanced(
          rest,
          current :+ Multiply(pos)
        )
      }
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) :: TailRecLoop(
            _,
            List(
              Skip(_, List(Clone(_))),
              Clone(_),
              Skip(
                _,
                List(
                  Skip(_, List(AddToStackItem(_, 1))),
                  Subtract(_),
                  Match(
                    List(
                      AddToStackItem(_, 1),
                      NewStackItem(_, 0),
                      AddToStackItem(_, 1),
                      Skip(_, List(Match(List(), _, List(NewStackItem(_, 0)))))
                    ),
                    _,
                    List(NewStackItem(_, 0))
                  )
                )
              ),
              Swap(_),
              Match(
                List(
                  Match(
                    List(
                      Skip(
                        _,
                        List(Skip(_, List(Pop(_), AddToStackItem(_, 1))))
                      ),
                      Continue(_, 0)
                    ),
                    _,
                    List(
                      Skip(
                        _,
                        List(Skip(_, List(Pop(_), AddToStackItem(_, 1))))
                      ),
                      Continue(_, 0)
                    )
                  )
                ),
                _,
                List(Pop(_))
              )
            )
          ) :: rest => {
        scanAdvanced(
          rest,
          current :+ DivMod(pos)
        )
      }
      case Skip(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ Skip(pos, newInstructions)
        )
      }
      case DeferSkip(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ DeferSkip(pos, newInstructions)
        )
      }
      case RecPop(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ RecPop(pos, newInstructions)
        )
      }
      case DeferPop(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ DeferPop(pos, newInstructions)
        )
      }
      case Loop(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ Loop(pos, newInstructions)
        )
      }
      case DeferTailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ DeferTailRecLoop(pos, newInstructions)
        )
      }
      case TailRecLoop(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ TailRecLoop(pos, newInstructions)
        )
      }
      case Block(pos, instructions) :: rest => {
        val newInstructions = scanAdvanced(instructions, List())
        scanAdvanced(
          rest,
          current :+ Block(pos, newInstructions)
        )
      }
      case Match(left, pos, right) :: rest => {
        val newLeft = scanAdvanced(left, List())
        val newRight = scanAdvanced(right, List())
        scanAdvanced(
          rest,
          current :+ Match(newLeft, pos, newRight)
        )
      }
      case i :: rest => {
        scanAdvanced(
          rest,
          current :+ i
        )
      }
    }
  }
}
