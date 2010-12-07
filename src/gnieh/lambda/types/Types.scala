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
package types

import ast.LambdaExpr

sealed trait Type

/**
 * 
 * Type of booleans.
 * 
 * @author Lucas Satabin
 *
 */
case object Bool extends Type

/**
 * 
 * Type of natural numbers.
 * 
 * @author Lucas Satabin
 *
 */
case object Nat extends Type

/**
 * Type of functions.
 * 
 * @author Lucas Satabin
 *
 */
final case class Function(tp: Type, tr: Type) extends Type {
  override def toString = {
    val param =
      tp match {
        case _: Function => "(" + tp + ")"
        case _ => tp
      }
    param + " \u2192 " + tr
  }
}

/**
 * Unknown type.
 * 
 * @author Lucas Satabin
 *
 */
case object UnknownType extends Type {
  override def toString = "?"
}

/**
 * Unexpected type.
 * 
 * @author Lucas Satabin
 *
 */
final case class ErrorType(term: LambdaExpr, expected: Type, found: Type) extends Type {
  override def toString = 
    "Error while typing " + term.toString(false) +
      "\n\tExpected type: " + expected +
      "\n\tFound type:    " + found
}

case object WildcardType extends Type {
  override def toString = "_"
}