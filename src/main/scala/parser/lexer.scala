package parser

import parsley.Parsley
import parsley.token.descriptions.LexicalDesc
import parsley.token.descriptions.SymbolDesc
import parsley.token.Lexer
import parsley.token.descriptions.NameDesc
import parsley.token.predicate
import parsley.token.descriptions.SpaceDesc
import parsley.token.descriptions.text.TextDesc
import parsley.token.descriptions.text.EscapeDesc

private[parser] object lexer {
  private val operators = Set(
    "(",
    ")",
    "=",
    "!",
    "?",
    ":",
    "$",
    ",",
    "{",
    "}",
    ".",
    "%",
    "#"
  )
  private val identPredicate =
    predicate.Basic(x => !x.isWhitespace && !operators.contains(x.toString))

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = identPredicate,
      identifierLetter = identPredicate
    ),
    symbolDesc = SymbolDesc.plain
      .copy(
        hardOperators = operators
      ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = ";"
    ),
    textDesc = TextDesc.plain.copy(
      stringEnds = Set("\""),
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set(
          '"',
          '\\'
        ),
        singleMap = Map(
          ('n', '\n')
        )
      )
    )
  )

  private val lexer = new Lexer(desc)

  val identifier = lexer.lexeme.names.identifier
  val integer = lexer.lexeme.natural.decimal32
  val string = lexer.lexeme.text.string.fullUtf16

  val implicits = lexer.lexeme.symbol.implicits

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
