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
package gnieh.lambda
package ast

import util.environment

import org.kiama.attribution.Attributable

sealed trait Node

/**
 * AST node representing a lambda term.
 * @author Lucas Satabin
 *
 */
sealed trait LambdaExpr extends Node with Attributable {
  /**
   * Returns the String representation of this term
   * @param alias whether aliases should be displayed
   */
  def toString(alias: Boolean): String
}

/**
 * A variable simply consists of a name
 */
final case class Var(name: String) extends LambdaExpr {
  def toString(alias: Boolean): String = name
}

/**
 * A lambda abstraction of the form `\v.body'
 */
final case class Abs(v: Var, body: LambdaExpr) extends LambdaExpr {
  def toString(alias: Boolean): String = environment.getName(this) match {
    case Some(name) if alias => name
    case _ => "\u03BB" + v.toString(false) + "." + body.toString(alias)
  }
}

/**
 * Application of the form `f p'
 */
final case class App(f: LambdaExpr, p: LambdaExpr) extends LambdaExpr {
  def toString(alias: Boolean): String = environment.getName(this) match {
    case Some(name) if alias => name
    case _ =>
      val fun = f match {
        case _: Abs if !environment.containsExpr(f) || !alias => "(" + f.toString(alias) + ")"
        case _ => f.toString(alias)
      }
      val par = p match {
        case _: App | _: Abs if !environment.containsExpr(p) || !alias => "(" + p.toString(alias) + ")"
        case _ => p.toString(alias)
      }
      fun + " " + par
  }
}

/**
 * Substitution [v->by]expr
 */
final case class Subst(expr: LambdaExpr, v: String, by: LambdaExpr) extends LambdaExpr {
  def toString(alias: Boolean): String = throw new UnsupportedOperationException("toString(Boolean)")
}

/**
 * An error message
 */
final case class LambdaError(message: String) extends LambdaExpr {
  override def toString = message
  def toString(alias: Boolean): String = toString
}

final case class Assign(name: String, expr: LambdaExpr) extends Node {
  override def toString = name + " = " + expr
}

sealed trait Command extends Node
case object Quit extends Command
case object Help extends Command
case object ShowSteps extends Command
case object HideSteps extends Command
case object ShowAliases extends Command
case object HideAliases extends Command
case object NormalOrder extends Command
case object CallByName extends Command
case object CallByValue extends Command
case object Env extends Command
final case class LoadLib(name: String) extends Command
final case class SaveLib(name: String) extends Command
final case class RemoveEnv(names: List[String]) extends Command