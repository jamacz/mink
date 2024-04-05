package optimiser

import collection.mutable
import ast._

class tailcall {
  val errors = mutable.ListBuffer[OptimisationError]()

  def isRecursive(
      instructions: List[Instruction],
      level: Int
  ): Boolean = {
    instructions match {
      case Nil => false
      case Continue(_, i) :: rest if i >= level =>
        true
      case Match(left, _, right) :: rest =>
        isRecursive(left, level) ||
        isRecursive(right, level) ||
        isRecursive(rest, level)
      case Loop(_, instructions) :: rest =>
        isRecursive(instructions, level + 1) || isRecursive(rest, level)
      case _ :: rest =>
        isRecursive(rest, level)
    }
  }

  def mergeRecursionLevels(
      x: RecursionLevel,
      y: RecursionLevel
  ): RecursionLevel = {
    (x, y) match {
      case (NoRecursion, _) | (_, NoRecursion) => NoRecursion
      case (DeferTailRecursion, _) | (_, DeferTailRecursion) =>
        DeferTailRecursion
      case (TailRecursion, TailRecursion) => TailRecursion
    }
  }

  def isOnlyUnits(
      instructions: List[Instruction]
  ): Option[List[Instruction]] = {
    if (
      instructions.forall({
        case AddToStackItem(_, _) | NewStackItem(_, _) => true
        case _                                         => false
      })
    ) {
      Some(instructions.reverse.map({
        case AddToStackItem(pos, n) => DeferAddToStackItem(pos, n)
        case NewStackItem(pos, n)   => DeferNewStackItem(pos, n)
        case _                      => throw new Exception("Should not happen")
      }))
    } else {
      None
    }
  }

  def tailCallFunc(
      instructions: List[Instruction],
      current: List[Instruction]
  ): (List[Instruction], RecursionLevel) = {
    instructions match {
      case Nil => (current, TailRecursion)
      case Continue(pos, i) :: rest =>
        if (rest.isEmpty) {
          (current :+ Continue(pos, i), TailRecursion)
        } else {
          val deferred = isOnlyUnits(rest)
          deferred match {
            case Some(d) =>
              (current ::: (d :+ Continue(pos, i)), DeferTailRecursion)
            case None =>
              (current ::: (Continue(pos, i) :: rest), NoRecursion)
          }
        }
      case Match(left, pos, right) :: rest =>
        val (newLeft, leftRecursion) = tailCallFunc(
          left ::: rest,
          List()
        )
        val (newRight, rightRecursion) = tailCallFunc(
          right ::: rest,
          List()
        )
        (leftRecursion, rightRecursion) match {
          case (NoRecursion, _) | (_, NoRecursion) =>
            (current ::: Match(left, pos, right) :: rest, NoRecursion)
          case (DeferTailRecursion, _) | (_, DeferTailRecursion) =>
            (current :+ Match(newLeft, pos, newRight), DeferTailRecursion)
          case (TailRecursion, TailRecursion) =>
            (current :+ Match(newLeft, pos, newRight), TailRecursion)

        }

      case (l @ Loop(pos, instructions)) :: rest =>
        val (newInstructions, recursion) = tailCallFunc(
          instructions,
          List()
        )

        val newLoop = getNewLoop(l, newInstructions, recursion)

        if (isRecursive(instructions, 1)) {
          if (rest.isEmpty) {
            (current :+ newLoop, mergeRecursionLevels(TailRecursion, recursion))
          } else {
            val deferred = isOnlyUnits(rest)
            deferred match {
              case Some(d) =>
                (
                  current ::: (d :+ newLoop),
                  mergeRecursionLevels(DeferTailRecursion, recursion)
                )
              case None =>
                (
                  current ::: newLoop :: rest,
                  mergeRecursionLevels(NoRecursion, recursion)
                )
            }
          }
        } else {
          tailCallFunc(
            rest,
            (current :+ newLoop)
          )
        }
      case i :: rest =>
        tailCallFunc(
          rest,
          current :+ i
        )
    }
  }

  def getNewLoop(
      loop: Loop,
      newInstructions: List[Instruction],
      recursion: RecursionLevel
  ) = {
    val o: RecursionLevel = NoRecursion
    val pos = loop.pos
    val instructions = loop.contents
    recursion match {
      case TailRecursion =>
        TailRecLoop(pos, newInstructions)
      case DeferTailRecursion =>
        o match {
          case TailRecursion =>
            errors.addOne(
              CannotTailOptimise(pos, recursion)
            )
          case _ => {}
        }
        DeferTailRecLoop(pos, newInstructions)
      case NoRecursion =>
        o match {
          case TailRecursion =>
            errors.addOne(
              CannotTailOptimise(pos, recursion)
            )
          case DeferTailRecursion =>
            errors.addOne(
              CannotDeferTailOptimise(pos)
            )
          case _ => {}
        }
        Loop(pos, instructions)
    }
  }

  def tailCallBlock(
      instructions: List[Instruction]
  ): List[Instruction] = {
    val newInstructions = instructions.map({
      case l @ Loop(_, contents) => {
        val (newContents, rec) = tailCallFunc(contents, List())
        getNewLoop(l, newContents, rec)
      }
      case m @ Match(left, pos, right) => {
        val newLeft = tailCallBlock(left)
        val newRight = tailCallBlock(right)
        Match(newLeft, pos, newRight)
      }
      case i => {
        i
      }
    })
    newInstructions
  }

  def tailCall(
      program: Program
  ): (Program, List[OptimisationError]) = {
    val newInstructions = tailCallBlock(program.instructions)
    val newFuncs = program.funcs.map(f =>
      Func(f.pos, f.name, f.funcParams, f.export, tailCallBlock(f.instructions))
    )
    (
      Program(program.packageName, program.imports, newInstructions, newFuncs),
      errors.toList
    )
  }

}
