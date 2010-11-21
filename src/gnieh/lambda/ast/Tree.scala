/* This file is part of scalambda.
*
* See the NOTICE file distributed with this work for copyright information.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
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
case object Env extends Command
final case class LoadLib(name: String) extends Command
final case class SaveLib(name: String) extends Command