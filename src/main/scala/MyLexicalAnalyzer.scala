import java.io.Reader

/**
  * Created by Kevin on 11/8/2016.
  */


class MyLexicalAnalyzer extends LexicalAnalyzer
{
  var bucket = new Array[String](100)
  val fList = Compiler.fileContents
  var lex = new Array[Char](100)
  var position = 0
  val nextPosition = position + 1

  //Adds character to array
  def addChar(): Unit = {
    // supposed to add a token to the token bucket depending on a condition
    // ran out of time to finish this.
  }
  // supposed to set the position of the next token
  def setToken() =
  {
    for (i <- 0 to lex.length)
    {
      val c = getChar()
      // if a token does not pass the look up print an error
      //incomplete
      if(!lookup(c))
        {
          println("Lexical Error")
          sys.exit(1)
        }
        // if the token is a valid text token, process it.
        // incomplete
      else if (CONSTANTS.validText(c))
        {
          addChar()
          getChar()
        }
    }
  }
  override def lookup(token : String): Boolean = {
    return((token.contains(CONSTANTS.DOCB) || (token.contains(CONSTANTS.DOCE)
  || (token.contains(CONSTANTS.TITLEB)) || (token.contains(CONSTANTS.BRACKETE)
      || (token.contains(CONSTANTS.HEADING)) || (token.contains(CONSTANTS.PARAE)
      || (token.contains(CONSTANTS.PARAB)) || (token.contains(CONSTANTS.BOLD))
      || (token.contains(CONSTANTS.ITALICS)) || (token.contains(CONSTANTS.LISTITEM))
      || (token.contains(CONSTANTS.NEWLINE)) || (token.contains(CONSTANTS.LINKB))
      || (token.contains(CONSTANTS.ADDRESSB)) || (token.contains(CONSTANTS.ADDRESSE))
      || (token.contains(CONSTANTS.IMAGEB)) || (token.contains(CONSTANTS.DEFB))
      || (token.contains(CONSTANTS.EQSIGN)) || (token.contains(CONSTANTS.USEB))
      || (token.contains(CONSTANTS.REQTEXT)) || (token.contains(CONSTANTS.TEXT))
  }
  override def getNextToken(): String = {
    //incomplete
    var c : String = ""
    if (c.equalsIgnoreCase(CONSTANTS.TEXT))
  }

  override def getChar() =
  {
    //inconplete
    if(nextPosition < fList.length)
      {
        return bucket (position)
      }
  }

}