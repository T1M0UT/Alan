package Compiler
import Parser.parser
import scala.language.implicitConversions
import scala.language.reflectiveCalls

class compile extends parser {

  // for generating new labels
  var counter: Int = 1

  def Fresh(x: String): String = {
    counter += -1
    x ++ "_" ++ counter.toString()
  }

  // convenient string interpolations
  // for instructions and labels

  implicit def string_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
  }

  // Map for variables
  type Env = Map[String, Int]

  // this allows us to write things like
  // i"iadd" and l"Label"

  def compile_op(op: String): String = op match {
    case "+" => i"iadd"
    case "-" => i"isub"
    case "*" => i"imul"
    case "/" => i"idiv"
    case "%" => i"irem"
  }

  // arithmetic expression compilation
  def compile_aexp(a: AExp, env : Env) : String = a match {
    case Num(i) => i"ldc $i"
    case Var(s) => i"iload ${env(s)} \t\t; $s"
    case Aop(op, a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
  }

  // boolean expression compilation
  def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
    case True => ""
    case False => i"goto $jmp"
    case Bop("==", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
    case Bop("!=", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
    case Bop("<", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
    case Bop("<=", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpgt $jmp"
    case Bop(">", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp"
    case Bop(">=", a1, a2) =>
      compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmplt $jmp"
  }

  // statement compilation
  def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
    case Skip => ("", env)
    case Assign(x, a) => {
      val index = env.getOrElse(x, env.keys.size)
      (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
    }
    case If(b, bl1, bl2) => {
      val if_else = Fresh("If_else")
      val if_end = Fresh("If_end")
      val (instrs1, env1) = compile_block(bl1, env)
      val (instrs2, env2) = compile_block(bl2, env1)
      (compile_bexp(b, env, if_else) ++
        instrs1 ++
        i"goto $if_end" ++
        l"$if_else" ++
        instrs2 ++
        l"$if_end", env2)
    }
    case While(b, bl) => {
      val loop_begin = Fresh("Loop_begin")
      val loop_end = Fresh("Loop_end")
      val (instrs1, env1) = compile_block(bl, env)
      (l"$loop_begin" ++
        compile_bexp(b, env, loop_end) ++
        instrs1 ++
        i"goto $loop_begin" ++
        l"$loop_end", env1)
    }
    case For(id, value, uptoValue, bl) => {
      val loop_begin = Fresh("Loop_begin")
      val loop_end = Fresh("Loop_end")
      val (iteratorAssignInstrs, env0) = compile_stmt(Assign(id, value), env)
      val (instrs, env1) = compile_block(bl, env0)
      val loopBeginning = (
        iteratorAssignInstrs ++
        l"$loop_begin" ++
          compile_bexp(Bop("<=", Var(id), uptoValue), env1, loop_end) ++
          instrs
        )
      val (iteratorIncInstrs, env2) = compile_stmt(Assign(id, Aop("+", Var(id), Num(1))), env1)
      (
        loopBeginning ++
        iteratorIncInstrs ++
          i"goto $loop_begin" ++
        l"$loop_end", env2
      )
    }
    case WriteVar(x) =>
      (i"iload ${env(x)} \t\t; $x" ++
        i"invokestatic XXX/XXX/write(I)V", env)
    case WriteStr(x) =>
      (i"""ldc "$x"""" ++
        i"invokestatic XXX/XXX/writeStr(Ljava/lang/String;)V", env)
    case Read(x) => {
      val index = env.getOrElse(x, env.keys.size)
      (i"invokestatic XXX/XXX/read()I" ++
       i"istore $index \t\t; $x", env + (x -> index))
    }

  }

  // compilation of a block (i.e. list of instructions)
  def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
    case Nil => ("", env)
    case s::bl => {
      val (instrs1, env1) = compile_stmt(s, env)
      val (instrs2, env2) = compile_block(bl, env1)
      (instrs1 ++ instrs2, env2)
    }
  }

  // main compilation function for blocks
  def compile(bl: Block, class_name: String) : String = {
    val instructions = compile_block(bl, Map.empty)._1
    (beginning ++ instructions ++ ending).replace("XXX", class_name)
  }

  def run(bl: Block, class_name: String) = {
    val code = compile(bl, class_name)
    val folder = os.pwd / os.up / "Examples" / s"$class_name"
    os.makeDir.all(folder)
    os.write.over(folder / s"$class_name.j", code)
    os.proc("java", "-jar", "jasmin-2.4/jasmin.jar", s"$folder/$class_name.j").call()
    print(os.proc("java", s"$class_name/$class_name").call().out.text())
  }

  val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/println(I)V
    return
.end method

.method public static writeStr(Ljava/lang/String;)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method

.method public static read()I
    .limit locals 10
    .limit stack 10

    ldc 0
    istore 1  ; this will hold our final integer
Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10   ; the newline delimiter  CHANGED 10 -> 13
    isub
    ifeq Label2
    iload 2
    ldc 32   ; the space delimiter
    isub
    ifeq Label2

    iload 2
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
Label2:
    ;when we come here we have our integer computed in local variable 1
    iload 1
    ireturn
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

  val ending = """
; COMPILED CODE ENDS
   return

.end method
"""

}
