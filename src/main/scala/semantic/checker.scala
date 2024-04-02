package semantic

import collection.mutable
import ast._
import scala.annotation.tailrec

class checker(val programs: Map[Ident, Program]) {
  val errors = mutable.ListBuffer[SemanticError]()

  def getAllFunctions(program: ast.Program): Map[String, Func] = {
    program.funcs
      .map(f => (f.name.name.last, f))
      .groupBy(_._1)
      .map({ case (k, v) =>
        if (v.length > 1) {
          val pn = program.packageName match {
            case Some(p) => Ident(v(0)._2.pos, p.name :+ k)
            case None    => Ident(v(0)._2.pos, List(k))
          }
          errors.addOne(FunctionRedefinition(pn, v.map(_._2.pos)))
        }
        (k, v(0)._2)
      })
  }

  private def identsMatch(total: Ident, fst: Ident, snd: Ident): Boolean = {
    val id = Ident(snd.pos, fst.name ::: snd.name)
    if (total.name == id.name) {
      true
    } else {
      fst match {
        case Ident(_, Nil) => false
        case Ident(_, l) =>
          identsMatch(total, Ident(fst.pos, l.dropRight(1)), snd)
      }
    }
  }

  private def getLocalFuncByIdent(
      packageName: Option[Ident],
      allFunctions: Map[String, Func],
      name: Ident
  ): Option[(Ident, Func)] = {
    allFunctions.foreach({ case (s, f) =>
      val i = packageName match {
        case Some(p) => Ident(f.pos, p.name :+ s)
        case None    => Ident(f.pos, List(s))
      }
      if (identsMatch(i, packageName.getOrElse(Ident(f.pos, List())), name)) {
        return Some((i, f))
      }
    })
    None
  }

  private def getImportedFuncByIdent(
      imports: List[Ident],
      importedFunctions: Map[Ident, Func],
      name: Ident
  ): Option[List[(Ident, Func)]] = {
    var funcs: List[(Ident, Func)] = Nil
    for (imp <- imports) {
      val id = Ident(name.pos, imp.name ::: name.name)
      if (importedFunctions.contains(id)) {
        funcs = (id, importedFunctions(id)) :: funcs
      }
    }
    funcs match {
      case Nil => None
      case l   => Some(l)
    }
  }

  private def checkScope(
      packageName: Option[Ident],
      imports: List[Ident],
      instructions: List[Instruction],
      allFunctions: Map[String, Func],
      importedFunctions: Map[Ident, Func],
      functionParams: Map[String, FuncParam],
      level: Int,
      currentInstructions: List[Instruction] = List()
  ): List[Instruction] = {
    instructions match {
      case Nil => currentInstructions
      case (c @ Call(pos, name)) :: rest => {
        if (name.name.length == 1 && functionParams.contains(name.name(0))) {
          checkScope(
            packageName,
            imports,
            rest,
            allFunctions,
            importedFunctions,
            functionParams,
            level,
            currentInstructions :+ ResolvedCall(
              pos,
              name,
              List()
            )
          )
        } else {
          val afContains = getLocalFuncByIdent(packageName, allFunctions, name)
          val ifContains =
            getImportedFuncByIdent(imports, importedFunctions, name)
          if (afContains.isDefined || ifContains.isDefined) {
            val (ident, func) = if (afContains.isDefined) {
              afContains.get
            } else {
              val funcs = ifContains.get
              if (funcs.length > 1) {
                errors.addOne(AmbiguousFunction(pos, name))
              }
              funcs(0)
            }
            val r =
              checkScope(
                packageName,
                imports,
                rest,
                allFunctions,
                importedFunctions,
                functionParams,
                level
              )
            r.splitAt(func.funcParams.getOrElse(List()).length) match {
              case (params, rest) =>
                if (params.length != func.funcParams.getOrElse(List()).length) {
                  errors.addOne(
                    TooFewFunctionParams(
                      pos,
                      ident,
                      func.funcParams.getOrElse(List()).length,
                      params.length
                    )
                  )
                }
                val newInstruction = ResolvedCall(
                  pos,
                  ident,
                  params.map(p =>
                    p match {
                      case Block(_, contents) => contents
                      case i                  => List(i)
                    }
                  )
                )
                currentInstructions ::: newInstruction :: rest
            }
          } else {
            errors.addOne(UndefinedFunction(pos, name))
            checkScope(
              packageName,
              imports,
              rest,
              allFunctions,
              importedFunctions,
              functionParams,
              level,
              currentInstructions :+ c
            )
          }
        }
      }
      case Block(pos, contents) :: rest => {
        checkScope(
          packageName,
          imports,
          rest,
          allFunctions,
          importedFunctions,
          functionParams,
          level,
          currentInstructions :+ (checkScope(
            packageName,
            imports,
            contents,
            allFunctions,
            importedFunctions,
            functionParams,
            level
          ) match {
            case List(i) => i
            case l       => Block(pos, l)
          })
        )
      }
      case Match(left, pos, right) :: rest => {
        checkScope(
          packageName,
          imports,
          rest,
          allFunctions,
          importedFunctions,
          functionParams,
          level,
          currentInstructions :+ Match(
            checkScope(
              packageName,
              imports,
              left,
              allFunctions,
              importedFunctions,
              functionParams,
              level
            ),
            pos,
            checkScope(
              packageName,
              imports,
              right,
              allFunctions,
              importedFunctions,
              functionParams,
              level
            )
          )
        )
      }
      case Loop(pos, instructions, o) :: rest => {
        checkScope(
          packageName,
          imports,
          rest,
          allFunctions,
          importedFunctions,
          functionParams,
          level,
          currentInstructions :+ Loop(
            pos,
            checkScope(
              packageName,
              imports,
              instructions,
              allFunctions,
              importedFunctions,
              functionParams,
              level + 1
            ),
            o
          )
        )
      }
      case Continue(pos, i) :: rest => {
        if (i >= level) {
          errors.addOne(CannotRecurse(pos, level, i + 1))
        }
        checkScope(
          packageName,
          imports,
          rest,
          allFunctions,
          importedFunctions,
          functionParams,
          level,
          currentInstructions :+ Continue(pos, i)
        )
      }
      case i :: rest => {
        checkScope(
          packageName,
          imports,
          rest,
          allFunctions,
          importedFunctions,
          functionParams,
          level,
          currentInstructions :+ i
        )
      }
    }
  }

  def checkProgram(program: ast.Program): (Program, List[SemanticError]) = {
    var functions = getAllFunctions(program)
    val importedPackages = program.imports
      .flatMap({ case i @ Ident(pos, path) =>
        val ps = programs
          .filter(p =>
            p._1.name.length >= i.name.length && p._1.name
              .zip(i.name)
              .forall({ case (a, b) => a == b })
          )
        if (ps.isEmpty)
          errors.addOne(UnknownModule(pos, i))
        ps.map(_._2).toList
      })
      .groupBy(_.packageName)
      .map(_._2.head)
    val importedFunctions = importedPackages
      .flatMap({ case p =>
        getAllFunctions(p)
          .filter(f => f._2.export)
          .map(f => (Ident(f._2.pos, p.packageName.get.name :+ f._1), f._2))
      })
      .toMap
    functions = functions.map { case (name, func) =>
      (
        name,
        Func(
          func.pos,
          program.packageName match {
            case Some(p) => Ident(func.pos, p.name :+ name)
            case None    => Ident(func.pos, List(name))
          },
          func.funcParams,
          func.export,
          checkScope(
            program.packageName,
            program.imports,
            func.instructions,
            functions,
            importedFunctions,
            func.funcParams.getOrElse(List()).map(p => (p.name, p)).toMap,
            0
          )
        )
      )
    }
    val newProgram =
      Program(
        program.packageName,
        program.imports,
        checkScope(
          program.packageName,
          program.imports,
          program.instructions,
          functions,
          importedFunctions,
          Map(),
          0
        ),
        functions.values.toList
      )
    (newProgram, errors.toList)
  }
}
