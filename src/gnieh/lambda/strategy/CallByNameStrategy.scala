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
 * Call-by-name strategy: No reduction allowed within a lambda abstraction.
 * 
 * @author Lucas Satabin
 *
 */
object CallByNameStrategy extends InterpretationStrategy {

  val name = "call-by-name"

  /**
   * the notinlambda strategy is similar to the oncetd strategy but stops as soon
   * as it reached a lambda abstraction
   */
  def notinlambda(s: => Strategy): Strategy =
    new Strategy {
      def apply(t: Term) = t match {
        case _: Abs => None
        case _ => (s <+ notinlambda(one(s)))(t)
      }
    }

  lazy val stepStrategy = notinlambda(step) <* substitute

}