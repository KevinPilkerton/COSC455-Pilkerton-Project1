import scala.io.Source._
import java.awt.Desktop
import java.io.{File, IOException}

object Compiler{

  var currentToken : String = ""
  var fileContents : String = ""

  val Scanner = new MyLexicalAnalyzer
  val Parser = new MySyntaxAnalyzer
  val SemanticAnalyzer = new MySemanticAnalyzer

  def main(args: Array[String]): Unit = {
    // checks the inuptted file
    checkFile(args)
    //reads the file and stores it as a string in fileContents
    readFile(args(0))
    // lexer checks the tokens
    Scanner.getNextToken()
    // checks tokens against the syntax
    Parser.gittex()
    // prints error if error in syntax
    if(Parser.getError)
      sys.exit(1)
    // converts tree into html code
    SemanticAnalyzer.convertToHTML()

    // opens default browser and html code from semantic
    SemanticAnalyzer.openHTMLFileInBrowser(fileContents)
  }

  def readFile(file : String) = {
    val source = scala.io.Source.fromFile(file)
    fileContents = try source.mkString finally source.close()
  }

  def checkFile(args : Array[String]) = {
    if (args.length != 1) {
      println("USAGE ERROR: Enter a filename!")
      System.exit(1)
    }
    else if (! args(0).endsWith(".mkd")) {
      println("USAGE ERROR: Incorrect File Extension!")
      System.exit(1)
    }
  }

}