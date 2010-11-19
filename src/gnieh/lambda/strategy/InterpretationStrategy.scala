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

/**
 * This trait describes the interface of an interpretation strategy
 * 
 * @author Lucas Satabin
 *
 */
trait InterpretationStrategy {
  
  val name: String
  
  def init
  
  /**
   * Reduces the given expression by one step
   * @param expr expression to reduce
   */
  def apply(expr: LambdaExpr): Option[LambdaExpr]

}