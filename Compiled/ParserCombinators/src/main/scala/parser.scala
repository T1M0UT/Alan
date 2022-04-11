package Parser
import scala.io.StdIn.readLine

class parser {
  case class ~[+A, +B](_1: A, _2: B)

  type IsSeq[A] = A => Seq[_]

  abstract class Parser[I : IsSeq, T]{
    def parse(in: I): Set[(T, I)]

    def parse_all(in: I) : Set[T] =
      for ((hd, tl) <- parse(in);
           if tl.isEmpty) yield hd
  }


  // Parser combinators

  // Sequence parser
  class SeqParser[I : IsSeq, T, S](p: => Parser[I, T],
                                   q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
    def parse(in: I) =
      for ((hd1, tl1) <- p.parse(in);
           (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
  }

  // Alternative parser
  class AltParser[I : IsSeq, T](p: => Parser[I, T],
                                q: => Parser[I, T]) extends Parser[I, T] {
    def parse(in: I) = p.parse(in) ++ q.parse(in)
  }

  // Map parser
  class MapParser[I : IsSeq, T, S](p: => Parser[I, T],
                                   f: T => S) extends Parser[I, S] {
    def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
  }

  // Any string parser
  case class StrParser(s: String) extends Parser[String, String] {
    def parse(sb: String) = {
      val (prefix, suffix) = sb.splitAt(s.length)
      if (prefix == s) Set((prefix, suffix)) else Set()
    }
  }

  // Variable id parser
  case object IdParser extends Parser[String, String] {
    val reg = "[a-z,A-Z][a-z,A-Z,0-9,_]*".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => Set(sb.splitAt(s.length))
    }
  }

  // String literal parser
  case object StringParser extends Parser[String, String] {
    val reg = """[a-z,A-Z,0-9, ,>,<,=,!,:,;,\,,.,_]*""".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => Set(sb.splitAt(s.length))
    }
  }

  // Number parser
  case object NumParser extends Parser[String, Int] {
    val reg = "[0-9]+".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => {
        val (hd, tl) = sb.splitAt(s.length)
        Set((hd.toInt, tl))
      }
    }
  }

  // p"a" style interpolation string parser
  implicit def parser_interpolation(sc: StringContext) = new {
    def p(args: Any*) = StrParser(sc.s(args:_*))
  }

  // Parser operations
  implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
    def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
    def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
    def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
  }

  // Expression structure
  abstract class Stmt
  abstract class AExp
  abstract class BExp

  type Block = List[Stmt]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
  case class While(b: BExp, bl: Block) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt
  case class WriteVar(s: String) extends Stmt
  case class WriteStr(s: String) extends  Stmt
  case class Read(s: String) extends Stmt
  case class For(id: String, value: AExp, uptoValue: AExp, bl: Block) extends Stmt

  case class Var(s: String) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

  case object True extends BExp
  case object False extends BExp
  case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
  case class And(b1: BExp, b2: BExp) extends BExp
  case class Or(b1: BExp, b2: BExp) extends BExp

  // arithmetic expressions
  lazy val AExp: Parser[String, AExp] =
    ((Te ~ p"+" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) }
      || (Te ~ p"-" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) }
      || Te
    )

  lazy val Te: Parser[String, AExp] =
    ((Fa ~ p"*" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) }
      || (Fa ~ p"%" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("%", x, z) }
      || (Fa ~ p"/" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) }
      || Fa
      )

  lazy val Fa: Parser[String, AExp] = {
    ((p"(" ~ AExp ~ p")").map{ case _ ~ y ~ _ => y }
      || IdParser.map(Var)
      || NumParser.map(Num)
      )
  }

  // Boolean expressions
  lazy val BExp: Parser[String, BExp] =
    ((AExp ~ p"==" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) }
      || (AExp ~ p"!=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) }
      || (AExp ~ p"<" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) }
      || (AExp ~ p">" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) }
      || (AExp ~ p"<=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) }
      || (AExp ~ p">=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">=", x, z) }
      || (p"(" ~ BExp ~ p")" ~ p"&&" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => And(y, v) }
      || (p"(" ~ BExp ~ p")" ~ p"||" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Or(y, v) }
      || (p"true".map[BExp]{ _ => True })
      || (p"false".map[BExp]{ _ => False })
      || (p"(" ~ BExp ~ p")").map[BExp]{ case _ ~ x ~ _ => x }
      )

  // a single statement
  lazy val Stmt: Parser[String, Stmt] =
    (p"skip".map[Stmt]{_ => Skip }
      || (p"for" ~ IdParser ~ p"=" ~ AExp ~ p"upto" ~ AExp ~ p"do" ~ Block).map[Stmt]{ case _ ~ x ~ _ ~ y ~ _ ~ z ~ _ ~ w => For(x, y, z, w)}
      || (p"read" ~ IdParser).map[Stmt]{ case _ ~ y => Read(y) }
      || (p"read(" ~ IdParser ~ p")").map[Stmt]{ case _ ~ y ~ _ => Read(y) }
      || (p"""write"""" ~ StringParser ~ p""""""").map[Stmt]{ case _ ~ s ~ _ => WriteStr(s) }
      || (p"write(" ~ IdParser ~ p")").map[Stmt]{ case _ ~ y ~ _ => WriteVar(y) }
      || (p"write" ~ IdParser).map[Stmt]{ case _ ~ y => WriteVar(y) }
      || (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) }
      || (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) }
      || (IdParser ~ p"=" ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) }
      )


  // statements
  lazy val Stmts: Parser[String, Block] =
    (Stmt ~ p";" ~ Stmts ~ p";").map[Block]{ case x ~ _ ~ z ~ _ => x :: z } ||
      (Stmt ~ p";" ~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
      (Stmt.map[Block]{ s => List(s) })

  // Blocks (enclosed in curly braces)
  lazy val Block: Parser[String, Block] =
    ((p"{" ~ Stmts ~ p"}").map{ case _ ~ y ~ _ => y } ||
      (Stmt.map(s => List(s))))

}
