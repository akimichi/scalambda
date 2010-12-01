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
 * 
 * Call-by-name strategy: normal-order strategy but no reduction allowed within a lambda abstraction.
 * It means that the parameters passed to a function are evaluated only when used in the function.
 * 
 * @author Lucas Satabin
 *
 */
object CallByNameStrategy extends InterpretationStrategy {

  val name = "call-by-name"

  /**
   *      t1 -> t1'
   *   ---------------
   *   t1 t2 -> t1' t2
   *   
   *   -----------------------
   *   (\x.t1) t2 -> [x->t2]t1
   */
  lazy val byname: Strategy = strategyf {
    case App(Abs(Var(x, _), body), v) => Some(Subst(body, x, v))
    case App(t, _) if t->isValue => None
    case t: LambdaExpr if t->isValue => None
    case t => one(byname)(t) 
  }
    
  lazy val stepStrategy = byname <* substitute

}