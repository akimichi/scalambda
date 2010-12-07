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
package analysis

import ast._

import org.kiama.attribution.Attribution._

/**
 * 
 * This object provides methods to check whether a lambda term is well formed.
 *  
 * @author Lucas Satabin
 *
 */
object BasicChecks {
  
  lazy val ok: LambdaExpr ==> Boolean =
    attr {
      case _: LambdaError => false
      case t => t.children.forall {
        case c: LambdaExpr => c->ok
      }
    }

}