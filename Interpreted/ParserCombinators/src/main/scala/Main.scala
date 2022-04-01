import java.io.{File, FileWriter, PrintWriter}

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.control.Breaks._

object Main {
  def main(args: Array[String]): Unit = {

    //Tests and time measurements

    //readLine()
    //println(eval(Stmts.parse_all("start:=1000;x:=start;y:=start;z:=start;whilex>0do{whiley>0do{whilez>0do{z:=z-1};z:=start;y:=y-1};y:=start;x:=x-1}").head))

    //Preparation to read

    println("Enter the file name (e.g. fibCleared.txt): ")
    val fileName = readLine()
    val tag = fileName.split("Cleared.")
    val folder = tag(0)
    val inputFilePath = getFilePath(folder,fileName)

    //Read file

    val fSource = Source.fromFile(inputFilePath)
    var input = fSource.getLines().mkString
    fSource.close()

    //Interpretation

    val statement = Stmts.parse_all(input).head
    //eval(statement)

    //Parse Tree Output

    val treeWriter = new PrintWriter(new File("../Examples/"+ folder +"/" + tag(0) +"ParseTree." + tag(1)))
    val parse_tree: String = statement.toString()
    treeWriter.write(parse_tree)
    treeWriter.close()
  }
  def getFilePath(folder : String, fileName : String): String ={
    return "../Examples/" + folder + "/" + fileName;
  }

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
    val reg = "[a-z][a-z,0-9]*".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => Set(sb.splitAt(s.length))
    }
  }

  // String type parser
  case object StringParser extends Parser[String, String] {
    val reg = "[a-z,A-Z,0-9, ]".r
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
  case class Read(s: String) extends Stmt

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
    (Te ~ p"+" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
      (Te ~ p"-" ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || Te

  lazy val Te: Parser[String, AExp] =
    (Fa ~ p"*" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } ||
      (Fa ~ p"%" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("%", x, z) } ||
      (Fa ~ p"/" ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } || Fa

  lazy val Fa: Parser[String, AExp] =
    (p"(" ~ AExp ~ p")").map{ case _ ~ y ~ _ => y } ||
      IdParser.map(Var) ||
      NumParser.map(Num)

  // Boolean expressions
  lazy val BExp: Parser[String, BExp] =
    (AExp ~ p"==" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } ||
      (AExp ~ p"!=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } ||
      (AExp ~ p"<" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
      (AExp ~ p">" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
      (AExp ~ p"<=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
      (AExp ~ p">=" ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">=", x, z) } ||
      (p"(" ~ BExp ~ p")" ~ p"&&" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => And(y, v) } ||
      (p"(" ~ BExp ~ p")" ~ p"||" ~ BExp).map[BExp]{ case _ ~ y ~ _ ~ _ ~ v => Or(y, v) } ||
      (p"true".map[BExp]{ _ => True }) ||
      (p"false".map[BExp]{ _ => False }) ||
      (p"(" ~ BExp ~ p")").map[BExp]{ case _ ~ x ~ _ => x }

  // a single statement
  lazy val Stmt: Parser[String, Stmt] =
    ((p"skip".map[Stmt]{_ => Skip }) ||
      (IdParser ~ p":=" ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
      (p"write(" ~ IdParser ~ p")").map[Stmt]{ case _ ~ y ~ _ => WriteVar(y) } ||
      (p"write" ~ IdParser).map[Stmt]{ case _ ~ y => WriteVar(y) } ||
      //(StrParser("write\"") ~ StringParser ~ StrParser("\"")).map[Stmt]{ case _ ~ y ~ _ => WriteStr(y) } ||
      (p"if" ~ BExp ~ p"then" ~ Block ~ p"else" ~ Block)
        .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
      (p"while" ~ BExp ~ p"do" ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) } ||
      (p"read" ~ IdParser).map[Stmt]{ case _ ~ y => Read(y) } ||
      (p"read(" ~ IdParser ~ p")").map[Stmt]{ case _ ~ y ~ _ => Read(y) }
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

  // An interpreter for the WHILE language
  type Env = Map[String, Int]

  def eval_aexp(a: AExp, env: Env) : Int = a match {
    case Num(i) => i
    case Var(s) => env(s)
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
    case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
    case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
  }

  def eval_bexp(b: BExp, env: Env) : Boolean = b match {
    case True => true
    case False => false
    case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
    case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
    case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
  }

  def eval_stmt(s: Stmt, env: Env) : Env = s match {
    case Skip => env
    case Assign(x, a) => env + (x -> eval_aexp(a, env))
    case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
    case While(b, bl) =>
      if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
      else env
    case WriteVar(x) => { println(env(x)); env }
    //case WriteStr(x) => { println(x); env }
    case Read(x) => {
      val input = Num(readLine().toInt);
      env + (x-> eval_aexp(input, env));
    }
  }

  def eval_bl(bl: Block, env: Env) : Env = bl match {
    case Nil => env
    case s::bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block) : Env = eval_bl(bl, Map())

}
