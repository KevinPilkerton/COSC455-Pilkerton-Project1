/**
  * Created by Kevin on 10/11/2016.
  */
trait LexicalAnalyzer {
  def addChar() : Unit
  def getChar() : Char
  def getNextToken() : Unit
  def lookup() : Boolean
}