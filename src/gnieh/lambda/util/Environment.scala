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
package util

import ast._

import scala.collection.mutable.Map

object environment {
  private val wrapped = Map.empty[String, LambdaExpr]
  private val inverse = Map.empty[LambdaExpr, String]

  def inverseIterator: Iterator[(LambdaExpr, String)] = inverse.iterator

  def getExpr(key: String): Option[LambdaExpr] = wrapped.get(key)

  def getName(expr: LambdaExpr): Option[String] = inverse.get(expr)
  
  def definitions: Iterator[(String, LambdaExpr)] = wrapped.iterator
  
  def bind(name: String, expr: LambdaExpr) {
    if(inverse.contains(expr))
      println("This expression is already bound to " + inverse(expr))
    else {
      wrapped += (name -> expr)
      inverse += (expr -> name)
    }
  }

  def unbind(name: String) {
    wrapped.remove(name) match {
      case Some(expr) => inverse.remove(expr)
      case _ => // do nothing
    }
  }

  def empty {
    wrapped.empty
    inverse.empty
  }

  def size = wrapped.size
  
  def containsId(id: String) = wrapped.contains(id)
  
  def containsExpr(expr: LambdaExpr) = inverse.contains(expr)

}