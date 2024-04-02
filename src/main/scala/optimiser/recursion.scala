package optimiser

import ast._

object recursion {
  def tryFlattenBlock(
      func: Func,
      instructions: List[Instruction],
      current: List[Instruction] = List(),
      isRecursive: Boolean = false,
      level: Int = 0
  ): (List[Instruction], Boolean) = {
    instructions match {
      case Nil => (current, isRecursive)
      case (b @ Block(pos, contents)) :: rest =>
        val (newContents, iR) = tryFlattenBlock(
          func,
          contents,
          List(),
          isRecursive,
          level
        )
        tryFlattenBlock(
          func,
          rest,
          current ::: newContents,
          iR,
          level
        )
      case (c @ ResolvedCall(pos, name, params)) :: rest =>
        val (newCall, iR) =
          if (
            name == func.name && (func.funcParams match {
              case None => params.isEmpty
              case Some(ps) =>
                ps.zip(params)
                  .foldRight(true)({
                    case (
                          (
                            FuncParam(_, n),
                            List(ResolvedCall(_, Ident(_, List(m)), List()))
                          ),
                          true
                        ) => {
                      n == m
                    }
                    case _ => false
                  })
            })
          ) {
            (Continue(pos, level), true)
          } else {
            var iR = false
            val newParams = params.map(p => {
              val (newP, isR) = tryFlattenBlock(
                func,
                p,
                List(),
                isRecursive,
                level
              )
              iR = iR || isR
              newP
            })
            (ResolvedCall(pos, name, newParams), iR)
          }
        tryFlattenBlock(
          func,
          rest,
          current :+ newCall,
          iR,
          level
        )
      case (m @ Match(left, pos, right)) :: rest =>
        val (newLeft, iR) = tryFlattenBlock(
          func,
          left,
          List(),
          isRecursive,
          level
        )
        val (newRight, iR2) = tryFlattenBlock(
          func,
          right,
          List(),
          isRecursive,
          level
        )
        tryFlattenBlock(
          func,
          rest,
          current :+ Match(newLeft, pos, newRight),
          iR || iR2,
          level
        )
      case (l @ Loop(pos, instructions, o)) :: rest =>
        val (newInstructions, iR) = tryFlattenBlock(
          func,
          instructions,
          List(),
          isRecursive,
          level + 1
        )
        tryFlattenBlock(
          func,
          rest,
          current :+ Loop(pos, newInstructions, o),
          iR,
          level
        )
      case i :: rest =>
        tryFlattenBlock(
          func,
          rest,
          current :+ i,
          isRecursive,
          level
        )
    }
  }

  def tryFlattenFunc(f: Func) = {
    val (newInstructions, isRecursive) = tryFlattenBlock(
      f,
      f.instructions
    )
    if (isRecursive) {
      Func(
        f.pos,
        f.name,
        f.funcParams,
        f.export,
        List(Loop(f.pos, newInstructions, NoRecursion))
      )
    } else {
      Func(f.pos, f.name, f.funcParams, f.export, newInstructions)
    }
  }

  // def tryFlatten(program: Program): Program = {
  //   val newFuncs = program.funcs.map(f => tryFlattenFunc(f))
  //   Program(
  //     program.packageName,
  //     program.imports,
  //     program.instructions,
  //     newFuncs
  //   )
  // }
}
