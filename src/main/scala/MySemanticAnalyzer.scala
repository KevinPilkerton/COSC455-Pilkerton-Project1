import java.awt.Desktop
import java.io.{File, IOException, PrintWriter}

/**
  * Created by Kevin on 10/11/2016.
  */
class MySemanticAnalyzer
{
  var treeToHTML = new scala.collection.mutable.Stack[String] // converts tree to html
  var syntaxParseTree = Compiler.Parser.parseTree.reverse // takes syntax parse tree and removes elements from it
  var nextToken = syntaxParseTree.pop() // first token
  var empty = syntaxParseTree.isEmpty // checks if parsetree is empty
  var temp : String = null // temporary variable
  var token : String = null // token

  //Checks whether or not the token is text or not
  def validText(t : String) : Boolean =
  {
    if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT))
    {
      return true
    }
    else
    {
      return false
    }
  }
  // converts Syntax Tree to HTML
  def convertToHTML() = {
    while(!empty)
      {
        // checks if the token is valid text
        if(validText(nextToken))
        {
          treeToHTML.push(nextToken)
          nextToken = syntaxParseTree.pop()
        }
        // checks if token is document begin tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.DOCB))
          {
            treeToHTML.push("<html>")
            nextToken = syntaxParseTree.pop()
          }
        // checks if token is title begin tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.TITLEB))
          {
            treeToHTML.push("<head>")
            treeToHTML.push("<title>")
            treeToHTML.push(syntaxParseTree.pop())
            treeToHTML.push("</title>")
            treeToHTML.push("</head>")
            nextToken = syntaxParseTree.pop()
          }
        // checks if token is heading tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.HEADING))
          {
            treeToHTML.push("<h1>")
            treeToHTML.push(syntaxParseTree.pop())
            treeToHTML.push("</h1>")
            nextToken = syntaxParseTree.pop()
          }
        // checks if token is image tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
          {
            treeToHTML.push("<img src = \\")
            temp = syntaxParseTree.pop()
            syntaxParseTree.pop()
            syntaxParseTree.pop()
            treeToHTML.push(syntaxParseTree.pop())
            treeToHTML.push("\" alt=")
            treeToHTML.push(temp)
            treeToHTML.push("\">")
          }
        // checks if token is link tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.LINKB)){
          temp = syntaxParseTree.pop()
          syntaxParseTree.pop()
          nextToken =syntaxParseTree.pop()
          if(nextToken.equalsIgnoreCase(CONSTANTS.ADDRESSB))
            nextToken = syntaxParseTree.pop()
          treeToHTML.push("<a href= \"")
          treeToHTML.push(nextToken)
          treeToHTML.push("\">")
          treeToHTML.push(temp)
          treeToHTML.push("</a>")

          nextToken = syntaxParseTree.pop()
        }
        // checks if token is paragraph begin tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.PARAB))
        {
          treeToHTML.push("<p>")
          treeToHTML.push(syntaxParseTree.pop())
        }
        // checks if token is paragraph end tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.PARAE))
        {
          treeToHTML.push("</p>")
          nextToken = syntaxParseTree.pop()

        }
        // checks if token is italics tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.ITALICS))
        {
          treeToHTML.push("<li>")
          treeToHTML.push(syntaxParseTree.pop())
          treeToHTML.push("<//li>")
          nextToken = syntaxParseTree.pop()
        }
        // checks if token is new line tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.NEWLINE)){
          treeToHTML.push("<br>")
          nextToken = syntaxParseTree.pop()
        }
        // checks if token is varable definition tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.DEFB))
        {
          syntaxParseTree.pop()
          syntaxParseTree.pop()
          token = nextToken
          nextToken = syntaxParseTree.pop()
        }
        // checks if token is use tag
        if(nextToken.equalsIgnoreCase(CONSTANTS.USEB)){
          syntaxParseTree.push(temp)
          syntaxParseTree.pop()
          nextToken = syntaxParseTree.pop()
        }
      }
    // creates file "output.html"
    val file = new File("output.html")
    // prints the contents of the stack
    writeToFile()
    // outputs the file into the default browser
    openHTMLFileInBrowser("output.html")
    def writeToFile(): Unit ={
      val pw = new PrintWriter(file)
      pw.write(treeToHTML.reverse.toString())
      pw.close()
    }
    /* * Hack Scala/Java function to take a String filename and open in default web browswer. */
    def openHTMLFileInBrowser(htmlFileStr : String) = {
      val file : File = new File(htmlFileStr.trim)
      println(file.getAbsolutePath)
      if (!file.exists())
        sys.error("File " + htmlFileStr + " does not exist.")

      try {
        Desktop.getDesktop.browse(file.toURI)
      }
      catch {
        case ioe: IOException => sys.error("Failed to open file:  " + htmlFileStr)
        case e: Exception => sys.error("Something is wrong")
      }
    }
  }
}
