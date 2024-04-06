package parser

import parsley.errors.combinator._
import parsley.position.pos
import parsley.combinator.choice
import parsley.combinator.option
import parsley.combinator.sepBy1
import parsley.combinator.countMany
import parsley.combinator.countSome
import parsley.Parsley.pure
import parsley.combinator.many
import lexer.implicits.implicitSymbol
import parsley.Parsley.atomic
import parsley.errors.DefaultErrorBuilder
import parsley.errors.tokenextractors
import parsley.errors.ErrorBuilder
import java.io.File
import ast._

import parsley.errors.combinator._
import parsley.combinator.choice
import lexer.implicits.implicitSymbol
import ast._
import parsley.position.pos
import parsley.Parsley
import parsley.Parsley.pure
import parsley.combinator.many
import parsley.combinator.option
import parsley.Parsley.atomic
import parsley.lift.lift2
import parsley.Parsley.notFollowedBy
import java.nio.charset.StandardCharsets
import java.nio.file.Path

class program {
  private class CustomErrorBuilder
      extends DefaultErrorBuilder
      with tokenextractors.SingleChar {
    override def format(
        pos: String,
        source: Option[String],
        lines: Seq[String]
    ): String = {
      s"$filename$pos: syntax error\n" +
        s"${lines.map(l => s"  $l").mkString("\n")}"
    }

    override def source(sourceName: Option[String]): Option[String] =
      sourceName

    override def pos(line: Int, col: Int): String = s"($line,$col)"

    override def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        errorPointsAt: Int,
        errorWidth: Int
    ): Seq[String] = {
      s"| $line" ::
        s"| ${" " * errorPointsAt}${"^" * errorWidth}" :: Nil

    }
  }

  private implicit val builder: ErrorBuilder[String] = new CustomErrorBuilder

  var filename: String = ""
  var parentPath: Path = null

  lazy val tokenPos: Parsley[Pos] =
    pos.map(p => Pos(filename, p._1, p._2))

  def parse(fn: String, pth: Path, content: String) = {
    filename = fn
    parentPath = pth
    lexer.fully(program).parse(content)
  }

  lazy val program =
    lexer.fully(
      Program(option("=" ~> ident), many("$" ~> ident), instruction, many(func))
    )

  lazy val ident =
    Ident(tokenPos, sepBy1(lexer.identifier, "."))

  private[parser] lazy val func =
    Func(
      tokenPos,
      Ident(tokenPos, lexer.identifier.map(List(_))),
      option("$" ~> many(FuncParam(tokenPos, lexer.identifier))),
      choice(
        "?" ~> pure(true),
        pure(false)
      ) <~ "=",
      instruction
    ).label("function")

  private[parser] lazy val instruction: Parsley[List[Instruction]] = lift2(
    (as: List[Instruction], m: Option[(Pos, List[ast.Instruction])]) =>
      m match {
        case None               => as
        case Some((pos, right)) => List(Match(as, pos, right))
      },
    many(atom),
    option(tokenPos <~ ":".label("match") <~> many(atom))
  ) <~ notFollowedBy(":").explain("matching has unknown associativity")

  private[parser] lazy val atom: Parsley[Instruction] = choice(
    Block(tokenPos <~ "(", instruction <~ ")"),
    Loop(tokenPos <~ "{", instruction <~ "}"),
    AddToStackItem(tokenPos <~ "!", pure(1)),
    NewStackItem(tokenPos <~ "?", pure(0)),
    atomic(NewStackItem(tokenPos <~ "%", lexer.integer)),
    RawC(
      tokenPos <~ "%",
      lexer.string.map(n => {
        parentPath.resolve(n).toString()
      }),
      lexer.string
    ),
    Continue(tokenPos <~ ",", countMany(".")),
    (tokenPos <~> lexer.string).map(s =>
      Block(
        s._1,
        NewStackItem(s._1, 0) :: (s._2
          .getBytes(StandardCharsets.UTF_8)
          .reverse
          .map(c => NewStackItem(s._1, c))
          .toList)
      )
    ),
    Print(tokenPos <~ "#"),
    Read(tokenPos <~ "@"),
    // Inspect(tokenPos <~ "`"),
    Call(
      tokenPos,
      notFollowedBy(lexer.identifier <~ choice("?" ~> "=", "=", "$")) ~> ident
    )
  ).label("unary operator")
}
