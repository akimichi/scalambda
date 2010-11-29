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
import org.kiama.rewriting.Rewriter._
import org.kiama.attribution.{Attributable, Attribution}
import Attribution._

/**
 * 
 * This object allows to compute de Bruijn indices and transform terms to the 
 * corresponding de Bruijn terms.
 * 
 * @author Lucas Satabin
 *
 */
object DeBruijn {

  /**
   * This class represents a naming context with a parent.
   */
  sealed abstract class NamingContext(protected[this] val parent: NamingContext) {
    /**
     * Returns the index of the current name within this context.
     * Returns <code>-1</code> if not in context.
     */
    def getIndex(name: String): Int
    
    /**
     * Returns the name of the current index within this context.
     * Returns <code>None</code> if not in context.
     */
    def getName(index: Int): Option[String]
    
  }

  implicit object BaseNamingContext extends NamingContext(null) {

    private lazy val alphabet = Map(('a' to 'z') map { c => c.toString -> ('z' - c) }: _*)
    private lazy val reverse = Map(('a' to 'z') map { c => ('z' - c) -> c.toString }: _*)

    def getIndex(name: String) = alphabet.get(name) match {
      case Some(idx) => idx
      case None => -1
    }
    
    def getName(index: Int) = reverse.get(index)
    
  }
  
  final class AbsNamingContext(val variable: String, parent: NamingContext)
    extends NamingContext(parent) {

    require(parent != null)
    
    def getIndex(name: String) =
      if(variable == name) {
        0
      } else {
        1 + parent.getIndex(name)
      }
    
    def getName(index: Int) = 
      if(index == 0) {
        Some(variable)
      } else {
        parent.getName(index - 1)
      }
    
  }

  lazy val deBruijnTerm: NamingContext => LambdaExpr ==> DeBruijnExpr =
    paramAttr { 
      context => {
        case Var(x) => DeBruijnIndex(context.getIndex(x))
        case Abs(Var(x), b) =>
          val newContext = new AbsNamingContext(x, context)
          DeBruijnAbs(b->deBruijnTerm(newContext))
        case App(f, p) =>
          DeBruijnApp(f->deBruijnTerm(context), p->deBruijnTerm(context))
      }
    }
  
  lazy val normalTerm: NamingContext => DeBruijnExpr ==> LambdaExpr =
    paramAttr { 
      context => {
        case DeBruijnIndex(idx) => context.getName(idx) match {
          case Some(name) => Var(name)
          case None => LambdaError("index " + idx + " not in the naming context")
        }
        case DeBruijnAbs(b) =>
          
          null
        case DeBruijnApp(f, p) =>
          null
      }
    }
    
}

sealed trait DeBruijnExpr extends Attributable

final case class DeBruijnIndex(idx: Int) extends DeBruijnExpr {
  override def toString = idx.toString
}

final case class DeBruijnAbs(body: DeBruijnExpr) extends DeBruijnExpr {
  override def toString = "\u03BB." + body
}

final case class DeBruijnApp(fun: DeBruijnExpr, param: DeBruijnExpr) extends DeBruijnExpr {
  override def toString = {
      val f = fun match {
        case _: DeBruijnApp | _: DeBruijnAbs => "(" + fun + ")"
        case _ => fun.toString
      }
      val par = param match {
        case _: DeBruijnApp | _: DeBruijnAbs => "(" + param + ")"
        case _ => param.toString
      }
      f + " " + par
  }
}
