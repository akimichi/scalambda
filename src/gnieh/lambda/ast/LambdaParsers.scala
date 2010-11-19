package gnieh.lambda.ast

import scala.collection.mutable.Map
import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }

trait LambdaParsers extends RegexParsers with PackratParsers {
  
  val environment = Map.empty[String, LambdaExpr]

  lazy val line: Parser[Node] = 
    quit | help | beta | showSteps | hideSteps | assign | expr
  
  lazy val expr: PackratParser[LambdaExpr] =
    expr~factor ^^ {case f~p => App(f, p)} |
    (("\\" | "λ")~>variable)~("."~>expr) ^^ {case v ~ b => Abs(v, b)} |
    factor |
    failure("expression expected")

  lazy val factor: PackratParser[LambdaExpr] =
    "[A-Z][a-z_]*".r ^^ {name => parsedIdent(name)} |
    variable | "("~>expr<~")"

  lazy val variable: Parser[Var] =
    "[a-z]".r ^^ { case c => Var(c.charAt(0)) }
    
  lazy val assign: Parser[Assign] =
    ("[A-Z][a-z_]*".r<~"=")~expr ^^ {case name~expr => Assign(name, expr)}

  lazy val quit: Parser[Command] = ":quit" ^^ { _ => Quit }

  lazy val help: Parser[Command] = ":help" ^^ { _ => Help }

  lazy val beta: Parser[Command] = ":normal-order" ^^ { _ => NormalOrder }

  lazy val showSteps: Parser[Command] = ":show-steps" ^^ { _ => ShowSteps }

  lazy val hideSteps: Parser[Command] = ":hide-steps" ^^ { _ => HideSteps }
  
  private def parsedIdent(id: String): LambdaExpr =
    environment.get(id) match {
      case Some(expr) => expr
      case None => LambdaError(id + " not found in the environment")
  }
}