/**
  * Created by Kevin on 10/11/2016.
  */
object CONSTANTS
{
  val DOCB : String = 	"\\BEGIN"
  val DOCE : String = 	"\\END"
  val TITLEB : String = "\\TITLE["
  val BRACKETE : String = "]"
  val HEADING : String = "#"
  val PARAB : String = 	"\\PARAB"
  val PARAE : String = 	"\\PARAE"
  val BOLD : String = "**"
  val ITALICS : String = "*"
  val LISTITEM : String = "+"
  val NEWLINE : String = 	"\\\\"
  val LINKB : String = 	"["
  val ADDRESSB : String = "("
  val ADDRESSE : String = ")"
  val IMAGEB : String = "!["
  val DEFB : String = 	"\\DEF["
  val EQSIGN : String = 	"="
  val USEB : String = "\\USE["
  val REQTEXT : String = TEXT
  val TEXT : String = _

  def validText(s : String): Boolean =
  {
    if(s.equalsIgnoreCase(TEXT))
      {
        return true
      }
    else
      {
        return false
      }
  }
}
