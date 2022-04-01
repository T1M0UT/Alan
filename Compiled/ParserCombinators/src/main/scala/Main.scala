import java.io.{File, FileWriter, PrintWriter}
import scala.io.Source
import scala.io.StdIn.readLine
import Compiler.compile

object Main{
  def main(args: Array[String]): Unit = {

    val Compiler = new compile()

    //Tests and time measurements

    //readLine()
    //println(Compiler.run("readn;writen;","test"))

    //Preparation to read

    println("Enter the file name (e.g. fibCleared.txt): ")
    val fileName = readLine()
    val tag = fileName.split("Cleared.")
    val className = tag(0)
    val inputFilePath = getFilePath(className, fileName)

    //Read file

    val fSource = Source.fromFile(inputFilePath)
    val input = fSource.getLines().mkString
    fSource.close()

    //Parse and compile

    val parse_tree = Compiler.Stmts.parse_all(input).head
    Compiler.run(parse_tree, className)
    //eval(parse_tree)

    //Parse Tree Output

    val treeWriter = new PrintWriter(new File("../Examples/"+ className +"/" + tag(0) +"ParseTree." + tag(1)))
    treeWriter.write(parse_tree.toString())
    treeWriter.close()
  }

  def getFilePath(folder: String, fileName: String): String ={
    return "../Examples/" + folder + "/" + fileName;
  }


}
