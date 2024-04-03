package optimiser

import ast._

object commonOps {
  def removeFirstDeferNewStackItem(
      instrs: List[Instruction],
      current: List[Instruction]
  ): Option[List[Instruction]] = {
    instrs match {
      case Nil                               => None
      case DeferNewStackItem(_, 0) :: rest   => Some(current ::: rest)
      case DeferNewStackItem(_, _) :: rest   => None
      case DeferAddToStackItem(_, _) :: rest => None
      case i :: rest => removeFirstDeferNewStackItem(rest, current :+ i)
    }
  }

  def endsWithZero(instrs: List[Instruction]): Option[List[Instruction]] = {
    instrs.lastOption match {
      case Some(NewStackItem(_, 0)) => Some(instrs.dropRight(1))
      case _                        => None
    }
  }

  def pass1(
      instrs: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instrs match {
      case Nil => current
      case Loop(
            pos,
            List(
              Match(List(Continue(_, 0), AddToStackItem(_, 1)), _, List())
            ),
            _
          ) :: rest => {
        pass1(
          rest,
          current :+ Add(pos)
        )
      }
      case Loop(
            pos,
            List(
              Match(l @ List(Continue(_, 0), AddToStackItem(_, 1)), _, main)
            ),
            o
          ) :: rest => {
        endsWithZero(main) match {
          case Some(newMain) =>
            pass1(
              rest,
              current :+ Skip(pos, pass1(newMain, List()))
            )
          case None => {
            pass1(
              rest,
              current :+ Loop(
                pos,
                List(
                  Match(
                    l,
                    pos,
                    pass1(main, List())
                  )
                ),
                o
              )
            )
          }
        }
      }
      case Loop(
            pos,
            List(Match(List(Continue(_, 0)), _, List())),
            _
          ) :: rest => {
        pass1(
          rest,
          current :+ Pop(pos)
        )
      }
      case Loop(pos, contents, optimisationLevel) :: rest => {
        pass1(
          rest,
          current :+ Loop(
            pos,
            pass1(contents, List()),
            optimisationLevel
          )
        )
      }
      case Match(left, pos, right) :: rest => {
        pass1(
          rest,
          current :+ Match(
            pass1(left, List()),
            pos,
            pass1(right, List())
          )
        )
      }
      case Block(pos, instructions) :: rest => {
        pass1(
          rest,
          current :+ Block(
            pos,
            pass1(instructions, List())
          )
        )
      }
      case i :: rest => pass1(rest, current :+ i)
    }
  }

  def pass2(
      instrs: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instrs match {
      case Nil => current
      case Loop(
            pos,
            List(
              Match(
                List(
                  Skip(
                    _,
                    List(Match(List(), _, List(NewStackItem(_, 0))))
                  ),
                  Continue(_, 0)
                ),
                _,
                List()
              )
            ),
            _
          ) :: rest => {
        pass2(
          rest,
          current :+ Subtract(pos)
        )
      }
      case Skip(pos, List(NewStackItem(_, 0), NewStackItem(_, 0))) :: Loop(
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
            ),
            _
          ) :: rest => {
        pass2(
          rest,
          current :+ Clone(pos)
        )
      }
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) :: Loop(
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
            ),
            _
          ) :: rest => {
        pass2(
          rest,
          current :+ Swap(pos)
        )
      }
      case Loop(pos, contents, optimisationLevel) :: rest => {
        pass2(
          rest,
          current :+ Loop(
            pos,
            pass2(contents, List()),
            optimisationLevel
          )
        )
      }
      case Match(left, pos, right) :: rest => {
        pass2(
          rest,
          current :+ Match(
            pass2(left, List()),
            pos,
            pass2(right, List())
          )
        )
      }
      case Skip(pos, instructions) :: rest => {
        pass2(
          rest,
          current :+ Skip(
            pos,
            pass2(instructions, List())
          )
        )
      }
      case Block(pos, instructions) :: rest => {
        pass2(
          rest,
          current :+ Block(
            pos,
            pass2(instructions, List())
          )
        )
      }
      case i :: rest => pass2(rest, current :+ i)
    }
  }

  def pass3(
      instrs: List[Instruction],
      current: List[Instruction]
  ): List[Instruction] = {
    instrs match {
      case Nil => current
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) :: Loop(
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
            ),
            _
          ) :: rest => {
        pass3(rest, current :+ Multiply(pos))
      }
      case Skip(pos, List(Skip(_, List(NewStackItem(_, 0))))) :: Loop(
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
                  Match(List(), _, List()),
                  Skip(_, List(Skip(_, List(Pop(_), AddToStackItem(_, 1))))),
                  Continue(_, 0)
                ),
                _,
                List(Pop(_))
              )
            ),
            _
          ) :: rest => {

        pass3(rest, current :+ DivMod(pos))
      }
      case Loop(pos, contents, optimisationLevel) :: rest => {
        pass3(
          rest,
          current :+ Loop(
            pos,
            pass3(contents, List()),
            optimisationLevel
          )
        )
      }
      case Match(left, pos, right) :: rest => {
        pass3(
          rest,
          current :+ Match(
            pass3(left, List()),
            pos,
            pass3(right, List())
          )
        )
      }
      case Skip(pos, instructions) :: rest => {
        pass3(
          rest,
          current :+ Skip(
            pos,
            pass3(instructions, List())
          )
        )
      }
      case Block(pos, instructions) :: rest => {
        pass3(
          rest,
          current :+ Block(
            pos,
            pass3(instructions, List())
          )
        )
      }
      case i :: rest => pass3(rest, current :+ i)
    }
  }

  def fullyAbstractPass(
      program: Program,
      pass: (List[Instruction], List[Instruction]) => List[Instruction]
  ): Program = {
    val newMain = pass(program.instructions, List())
    val newFunctions = program.funcs.map { f =>
      Func(
        f.pos,
        f.name,
        f.funcParams,
        f.export,
        pass(f.instructions, List())
      )
    }
    Program(program.packageName, program.imports, newMain, newFunctions)
  }

  def fullyAbstract(program: Program, level: Int): Program = {
    if (level <= 1) return program;
    val p1 = fullyAbstractPass(program, pass1)

    if (level <= 2) return p1;
    val p2 = fullyAbstractPass(p1, pass2)

    if (level <= 3) return p2;
    val p3 = fullyAbstractPass(p2, pass3)

    p3
  }
}
