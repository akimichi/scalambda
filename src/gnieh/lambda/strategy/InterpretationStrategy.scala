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
package strategy

import ast._

import org.kiama.rewriting.Rewriter._

/**
 * This trait describes the interface of an interpretation strategy
 * 
 * @author Lucas Satabin
 *
 */
abstract class InterpretationStrategy {

  val name: String

  /**
   * Only rewrites an application of a lambda abstraction to a substitution.
   */
  protected lazy val step: Strategy = rule {
    case App(Abs(Var(v), b), p) => Subst(b, v, p)
  }

  /**
   * Reduces all substitutions with an outermost strategy
   */
  protected lazy val substitute: Strategy = outermost {
    rule {
      case Subst(Var(x), y, by) if (x == y) => by
      case Subst(v: Var, _, _) => v
      case Subst(App(f, p), v, by) => App(Subst(f, v, by), Subst(p, v, by))
      case Subst(Abs(Var(x), b), v, by) if v != x => Abs(Var(x), Subst(b, v, by))
      case Subst(a: Abs, _, _) => a
    }
  }

  def stepStrategy: Strategy

  /**
   * Reduces the given expression by one step
   * @param expr expression to reduce
   */
  def apply(expr: LambdaExpr): Option[LambdaExpr] =
    stepStrategy(expr) match {
      case Some(e: LambdaExpr) => Some(e)
      case _ => None
    }

}