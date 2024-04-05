package optimiser

import ast._
import collection.mutable

class inliner {
  def addRecursionLevel(
      instructions: List[Instruction],
      addLevel: Int,
      current: List[Instruction] = List(),
      level: Int = 0
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case Continue(pos, i) :: rest =>
        if (i < level) {
          addRecursionLevel(
            rest,
            addLevel,
            current :+ Continue(pos, i),
            level
          )
        } else {
          addRecursionLevel(
            rest,
            addLevel,
            current :+ Continue(pos, i + addLevel),
            level
          )
        }
      case (c @ ResolvedCall(pos, name, params)) :: rest =>
        val newParams =
          params.map(p => addRecursionLevel(p, addLevel, List(), level))
        addRecursionLevel(
          rest,
          addLevel,
          current :+ ResolvedCall(pos, name, newParams),
          level
        )
      case (m @ Match(left, pos, right)) :: rest =>
        val newLeft = addRecursionLevel(left, addLevel, List(), level)
        val newRight = addRecursionLevel(right, addLevel, List(), level)
        addRecursionLevel(
          rest,
          addLevel,
          current :+ Match(newLeft, pos, newRight),
          level
        )
      case (l @ Loop(pos, instructions)) :: rest =>
        val newInstructions =
          addRecursionLevel(instructions, addLevel, List(), level + 1)
        addRecursionLevel(
          rest,
          addLevel,
          current :+ Loop(pos, newInstructions),
          level
        )
      case i :: rest =>
        addRecursionLevel(rest, addLevel, current :+ i, level)
    }
  }

  def resolveInlineFunction(
      func: Func,
      instructions: List[Instruction],
      params: List[List[Instruction]],
      current: List[Instruction] = List(),
      level: Int = 0
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case (c @ ResolvedCall(pos, i @ Ident(_, names), p)) :: rest =>
        val index = names match {
          case List(name) =>
            func.funcParams.getOrElse(List()).indexWhere(_.name == name)
          case _ => -1
        }
        if (index >= 0) {
          val newInstruction =
            addRecursionLevel(params(index), level)
          resolveInlineFunction(
            func,
            rest,
            params,
            current ::: newInstruction,
            level
          )
        } else {
          val newParams =
            p.map(p => resolveInlineFunction(func, p, params, List(), level))
          resolveInlineFunction(
            func,
            rest,
            params,
            current :+ ResolvedCall(pos, i, newParams),
            level
          )
        }

      case (m @ Match(left, pos, right)) :: rest =>
        val newLeft = resolveInlineFunction(func, left, params, List(), level)
        val newRight = resolveInlineFunction(func, right, params, List(), level)
        resolveInlineFunction(
          func,
          rest,
          params,
          current :+ Match(newLeft, pos, newRight),
          level
        )
      case (l @ Loop(pos, instructions)) :: rest =>
        val newInstructions =
          resolveInlineFunction(func, instructions, params, List(), level + 1)
        resolveInlineFunction(
          func,
          rest,
          params,
          current :+ Loop(pos, newInstructions),
          level
        )
      case i :: rest =>
        resolveInlineFunction(func, rest, params, current :+ i, level)
    }
  }

  def inlineFunction(
      func: Func,
      instructions: List[Instruction],
      myParams: List[String],
      current: List[Instruction] = List()
  ): List[Instruction] = {
    instructions match {
      case Nil => current
      case (c @ ResolvedCall(pos, name, params)) :: rest =>
        val newParams = params.map(p => inlineFunction(func, p, myParams))
        if (
          name == func.name && (name.name.length != 1 ||
            !myParams.contains(name.name.head))
        ) {
          val newInstructions =
            resolveInlineFunction(func, func.instructions, newParams)
          inlineFunction(func, rest, myParams, current ::: newInstructions)
        } else {
          inlineFunction(
            func,
            rest,
            myParams,
            current :+ ResolvedCall(pos, name, newParams)
          )
        }
      case (m @ Match(left, pos, right)) :: rest =>
        val newLeft = inlineFunction(func, left, myParams)
        val newRight = inlineFunction(func, right, myParams)
        inlineFunction(
          func,
          rest,
          myParams,
          current :+ Match(newLeft, pos, newRight)
        )
      case (b @ Block(pos, contents)) :: rest =>
        val newContents = inlineFunction(func, contents, myParams)
        inlineFunction(func, rest, myParams, current ::: newContents)
      case (l @ Loop(pos, instructions)) :: rest =>
        val newInstructions = inlineFunction(func, instructions, myParams)
        inlineFunction(
          func,
          rest,
          myParams,
          current :+ Loop(pos, newInstructions)
        )
      case i :: rest =>
        inlineFunction(func, rest, myParams, current :+ i)
    }
  }

  def inlineFunctions(program: Program): Program = {
    val (toInline, toKeep) =
      program.funcs.partitionMap(f =>
        f.funcParams match {
          case Some(params) => Left(f.name)
          case _            => Right(f.name)
        }
      )
    var newFuncs = program.funcs
    var newMain = program.instructions
    for (inlineFuncName <- toInline) {
      val inlineFunc = recursion.tryFlattenFunc(
        newFuncs.find(_.name == inlineFuncName).get
      )
      val updatedFuncs = mutable.ListBuffer[Func]()
      for (func <- newFuncs) {
        if (func.name != inlineFuncName) {
          val newInstructions = inlineFunction(
            inlineFunc,
            func.instructions,
            func.funcParams.getOrElse(List()).map(_.name)
          )
          updatedFuncs += Func(
            func.pos,
            func.name,
            func.funcParams,
            func.export,
            newInstructions
          )
        }
      }
      newMain = inlineFunction(inlineFunc, newMain, List())
      newFuncs = updatedFuncs.toList
    }
    newFuncs = newFuncs.map(f => recursion.tryFlattenFunc(f))
    Program(program.packageName, program.imports, newMain, newFuncs)
  }
}
