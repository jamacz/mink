package semantic

import ast.Pos
import ast.Ident

trait SemanticError {}

case class UndefinedFunction(pos: Pos, name: Ident) extends SemanticError {
  override def toString: String =
    s"$pos: function call error\n" +
      s"  call to undefined function \"$name\""
}

case class FunctionRedefinition(name: Ident, positions: List[Pos])
    extends SemanticError {
  override def toString: String =
    s"function redefinition error\n" +
      s"  function \"$name\" redefined at\n" +
      positions
        .map(p => s"    $p")
        .mkString("\n")
}

case class TooFewFunctionParams(
    pos: Pos,
    name: Ident,
    expected: Int,
    actual: Int
) extends SemanticError {
  override def toString: String =
    s"$pos: function call error\n" +
      s"too few parameters for function \"$name\"\n" +
      s"  need $expected, found $actual"
}

case class UnknownPackage(pos: Pos, name: Ident) extends SemanticError {
  override def toString: String =
    s"$pos: import error\n" +
      s"  unknown package \"$name\""
}

case class AmbiguousFunction(pos: Pos, name: Ident) extends SemanticError {
  override def toString: String =
    s"$pos: function call error\n" +
      s"  ambiguous function call \"$name\""
}

case class CannotRecurse(pos: Pos, max: Int, found: Int) extends SemanticError {
  override def toString: String =
    s"$pos: recursion error\n" +
      s"  cannot recurse $found levels, recursion depth is only $max"
}
