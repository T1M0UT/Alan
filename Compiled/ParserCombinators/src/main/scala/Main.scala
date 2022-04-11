import java.io.{File, FileWriter, InputStream, PrintWriter}
import scala.io.Source
import scala.io.StdIn.readLine
import Compiler.compile

import scala.sys.error

object Main{
  val usage = """
  Usage: Main path/filename (e.g. ../../fib.txt.cleared)
  """
  def main(args: Array[String]): Unit = {
    if (args.length != 1) println(usage)
    val path = args(0)
    //println(args(0))
    //val path = "/Users/tymur/VSCodeProjects/AlanTest/Smt.alan.cleared"

    //val fileName = args(1)
    val Compiler = new compile()

    //Tests and time measurements

    //readLine()
    /*{
      //println(Compiler.run("readn;writen;","test"))
      val parse_trees = Compiler.Stmts.parse_all("readn;writen;")
      if (parse_trees.isEmpty ) println("Parsing test failed")
      val parse_tree = parse_trees.head
      println("Compiling test...")
      println(Compiler.run(parse_tree, "test"))
    }*/

    //Preparation to read

    //println("Enter the file name (e.g. fibCleared.txt): ")
    //val fileName = readLine()


    val pathAndName = path.split(".txt.cleared")(0)
    val pathNames = pathAndName.split("/")
    val length = pathNames.length
    // Class name as 'fib' is the last element of the path
    val className = pathNames(length-1)
    val inputFilePath = path

    //println(className, inputFilePath)

    //Read file

    val fSource = Source.fromFile(inputFilePath)
    val input = fSource.getLines().mkString
    fSource.close()

    //println(input)

    //Parse and compile

    try {
      println("Parsing...")
      val parse_trees = Compiler.Stmts.parse_all(input)
      if (parse_trees.isEmpty ) println("Parsing failed")
      val parse_tree = parse_trees.head
      println("Compiling...")
      Compiler.run(parse_tree, className)
    } catch {
      case e => e.printStackTrace()
    }

    //eval(parse_tree)

    //Parse Tree Output

    //val treeWriter = new PrintWriter(new File("../Examples/"+ className +"/" + tag(0) +"ParseTree." + tag(1)))
    //treeWriter.write(parse_tree.toString())
    //treeWriter.close()
  }

//  def getFilePath(folder: String, fileName: String): String ={
//    return "../Examples/" + folder + "/" + fileName;
//  }
}
