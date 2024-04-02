package optimiser

import ast.RecursionLevel
import ast.DeferTailRecursion

sealed trait OptimisationError

case class CannotTailOptimise(pos: ast.Pos, actual: RecursionLevel)
    extends OptimisationError {
  override def toString: String =
    s"$pos: tail call optimisation error\n" +
      s"  cannot tail call optimise\n" +
      (if (actual == DeferTailRecursion)
         s"  could only defer-tail call optimise"
       else "")
}

case class CannotDeferTailOptimise(pos: ast.Pos) extends OptimisationError {
  override def toString: String =
    s"$pos: tail call optimisation error\n" +
      s"  cannot defer-tail call optimise"
}
