package gnieh.lambda.ast

import org.kiama.attribution.Attributable

sealed trait Node

sealed trait LambdaExpr extends Node with Attributable
final case class Var(name: Char) extends LambdaExpr {
	override def toString = name.toString
}
final case class Abs(v: Var, body: LambdaExpr) extends LambdaExpr {
	override def toString = "λ" + v + "." + body
}
final case class App(f: LambdaExpr, p: LambdaExpr) extends LambdaExpr {
	override def toString = {
	  val fun = f match {
	    case _: Abs => "(" + f + ")"
	    case _ => f.toString
	  }
	  val par = p match {
	    case _: App | _: Abs => "(" + p + ")"
	    case _ => p.toString
	  }
	  fun + " " + par
	}
}
final case class Subst(expr: LambdaExpr, v: Char, by: LambdaExpr) extends LambdaExpr
final case class LambdaError(message: String) extends LambdaExpr {
  override def toString = message
}

final case class Assign(name: String, expr: LambdaExpr) extends Node {
  override def toString = name + " = " + expr
}

sealed trait Command extends Node
case object Quit extends Command
case object Help extends Command
case object ShowSteps extends Command
case object HideSteps extends Command
case object NormalOrder extends Command