/**
  * Created by Kevin on 11/8/2016.
  */
  class MySyntaxAnalyzer extends SyntaxAnalyzer
{

    // Flag for errors
    val parseTree = new scala.collection.mutable.Stack[String]
    var errorFound : Boolean = false
    def setError() = errorFound = true
    def resetError() = errorFound = false
    def getError : Boolean = errorFound


    // implements BNF rule for gittex
    override def gittex(): Unit = {
      resetError()
      if(!errorFound) docBegin()
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        if(!errorFound) variableDefine()
      }
      if(!errorFound) title()
      if(!errorFound) body()
      if(!errorFound) docEnd()
    }


    // Implements BNF rule for paragraph
    override def paragraph(): Unit = {
      resetError()
      if(!errorFound) paragraphBegin()
      while (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        if(!errorFound) variableDefine()
      }
      while (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB) && (!Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE))) {
        if(!errorFound) innerText()
      }
      if(!errorFound) paragraphEnd()
    }
    // implements BNF for inner item
    override def innerItem(): Unit = {
      resetError()
      while(!errorFound) {
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
          variableUse()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
          bold()
          innerItem()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
          italics()
          innerItem()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
          link()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)) {
          text()
          innerText()
        }
        else {
          setError()
        }
      }
    }

    //implements BNF rule for inner text
    override def innerText(): Unit = {
      resetError()
      while(!errorFound) {
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB)) {
          variableUse()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase((CONSTANTS.HEADING))) {
          heading()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
          bold()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
          italics()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
          listItem()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase((CONSTANTS.IMAGEB))) {
          image()
          innerText()
        }
        if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TEXT)) {
          text()
          innerText()
        }
        if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LINKB)) {
          link()
          innerText()
        }
        else {
          setError()
        }
      }
    }

    // implements BNF rule for links
    override def link(): Unit = {
      resetError()
      if(!errorFound) leftB()
      if(!errorFound) text()
      if(!errorFound) rightB()
      if(!errorFound) addressBegin()
      if(!errorFound) text()
      if(!errorFound) addressEnd()
    }

    // implements BNF rule for italics
    override def italics(): Unit = {
      resetError()
      if(!errorFound) italicsBegin()
      if(!errorFound) text()
      if(!errorFound) italicsBegin()
    }

    // implements BNF rule for body
    override def body(): Unit = {
      resetError()
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        if(!errorFound) paragraph()
        if(!errorFound) body()
      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {

      }
      else if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        if(!errorFound) newline()
        if(!errorFound) body()
      }
      else {
        if(!errorFound) innerText()
        if(!errorFound) body()
      }
    }

    // implements BNF for bold
    override def bold(): Unit = {
      resetError()
      if(!errorFound) bold()
      if(!errorFound) text()
      if(!errorFound) bold()
    }

    // implements BNF for newline
    override def newline(): Unit = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.NEWLINE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.NEWLINE + " was expected")
        setError()
      }
    }

    // implmenets BNF for title
    override def title(): Unit = {
      titleBegin()
      text()
      bracketEnd()
    }

    // implmements BNF for variableDefine
    override def variableDefine(): Unit = {
      defBegin()
      text()
      equalSign()
      text()
      bracketEnd()
    }

    // implements BNF for image
    override def image(): Unit = {
      imageBegin()
      text()
      leftB()
      addressBegin()
      text()
      addressEnd()
    }

    // implements BNF for variable use
    override def variableUse(): Unit = {
      varUseBegin()
      text()
      bracketEnd()
    }

    // Implements BNF for headings
    override def heading(): Unit = {
      headingBegin()
      text()
    }

    // implmements BNF for listItem
    override def listItem(): Unit = {
      listItemBegin()
      innerItem()
    }

    // checks token for doc begin
    def docBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCB)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found: " + Compiler.currentToken + "when " + CONSTANTS.DOCB + " was expected.")
        setError()
      }
    }

    // checks token for doc end
    def docEnd() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DOCE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found: " + Compiler.currentToken + "when " + CONSTANTS.DOCE + " was expected.")
        setError()
      }
    }

    // checks token for title begin
    def titleBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.TITLEB)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.TITLEB + " was expected")
        setError()
      }
    }

    // checks token for left bracket
    def bracketEnd() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BRACKETE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.BRACKETE + " was expected")
        setError()
      }
    }

    // checks token for heading begin
    def headingBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.HEADING)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.HEADING + " was expected")
        setError()
      }
    }

    // checks token for paragraph begin
    def paragraphBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAB)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.PARAB + " was expected")
        setError()
      }
    }


    // checks token for paragraph end
    def paragraphEnd() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.PARAE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.PARAE + " was expected")
        setError()
      }
    }

    // checks token for beginning of bold
    def boldBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.BOLD)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.BOLD + " was expected")
        setError()
      }
    }

    // checks token for equal sign
    def equalSign() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.EQSIGN)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.EQSIGN + " was expected")
        setError()
      }
    }

    def varUseBegin(): Unit = {
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.USEB))
      {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.USEB + " was expected")
        setError()
      }
    }
    // checks token for beginning of italics
    def italicsBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ITALICS)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.ITALICS + " was expected")
        setError()
      }
    }

    // checks token for list
    def listItemBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.LISTITEM)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.LISTITEM + " was expected")
        setError()
      }
    }

    def addressBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSB)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSB + " was expected")
        setError()
      }
    }

    def addressEnd() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.ADDRESSE)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.ADDRESSE + " was expected")
        setError()
      }
    }

    // checks tokens for beginning
    def defBegin() = {
      if (Compiler.currentToken.equalsIgnoreCase(CONSTANTS.DEFB)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.DEFB + " was expected")
        setError()
      }
    }
    // checks token for text
    def text() = {
      if (Compiler.Scanner.validText(Compiler.currentToken)) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: " + Compiler.currentToken + " was found when Text was expected")
        setError()
      }
    }
    // cheks token for left bracket
    def leftB() = {
      if (Compiler.currentToken.equalsIgnoreCase("[")) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found [ when " + Compiler.currentToken + " was found instead")
        setError()
      }
    }

    // checks token for image begin
    def imageBegin(): Unit =
    {
      if(Compiler.currentToken.equalsIgnoreCase(CONSTANTS.IMAGEB))
      {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Syntax Error: Found " + Compiler.currentToken + " when " + CONSTANTS.IMAGEB + " was expected")
        setError()
      }
    }

    // checks token for right bracket
    def rightB() = {
      if (Compiler.currentToken.equalsIgnoreCase("]")) {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else {
        println("Syntax Error: Found ] when " + Compiler.currentToken + " was found instead")
        setError()
      }
    }
    // checks token for right parenthesis
    def rightP() = {
      if(Compiler.currentToken.equalsIgnoreCase(")"))
      {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Syntax Error: Found ) when " + Compiler.currentToken + " was found instead")
        setError()
      }
    }
    // checks token for left parenthesis
    def leftP() = {
      if(Compiler.currentToken.equalsIgnoreCase("("))
      {
        parseTree.push(Compiler.currentToken)
        Compiler.Scanner.getNextToken()
      }
      else
      {
        println("Syntax Error: Found ( when " + Compiler.currentToken + " was found instead")
        setError()
      }
    }
  }
