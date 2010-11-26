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

import org.kiama.attribution.Attribution._
import org.kiama.rewriting.Rewriter._

/**
 * @author Lucas Satabin
 *
 */
object CallByValueStrategy extends InterpretationStrategy {

  val name = "call-by-value"

  /**
   * Attribute isValue indicates whether a lambda expression is a value.
   * Under the call-by-value strategy, we do not try to reduce a value.
   */
  lazy val isValue: LambdaExpr ==> Boolean =
    attr {
      case _: Var | _: Abs => true
      case App(_: Var, p) => p->isValue
      case _ => false
    }

  /**
   *      t1 -> t1'
   *   ---------------   (oncetd)
   *   t1 t2 -> t1' t2
   *   
   *      t2 -> t2'
   *   ---------------
   *   v1 t2 -> v1 t2'
   *   
   *   -------------------
   *   (\x.t) v -> [x->v]t
   */
  lazy val byvalue = rule {
    case App(Abs(Var(x), body), v) if v->isValue => Subst(body, x, v)
  }

  lazy val stepStrategy = notinlambda(byvalue) <* substitute

}