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
import types._
import org.kiama.rewriting.Rewriter._
import org.kiama.attribution.{Attributable, Attribution}
import Attribution._

/**
 * 
 * This object adds the attribute to type check lambda terms.
 * 
 * @author Lucas Satabin
 *
 */
object TypeChecker {
  
  /**
   *           x:T in G
   *          ----------            (T-Var)
   *          G |- x : T
   *  
   *         G,x:T |- t : T'
   *       -------------------      (T-Abs)
   *       G |- \x:T.t : T->T'
   *  
   *  G |- t : T->T'   G |- t' : T
   *  ----------------------------  (T-App)
   *         G |- t t' : T'
   */
  lazy val tpe: Map[String,Type] => LambdaExpr ==> Type =
    paramAttr {
      table => {
        case Var(x, UnknownType) => table.get(x) match {
          case Some(t) => t
          case _ => UnknownType
        }
        case Var(_, t) => t
        case Abs(v, b) => 
          val vtpe = v->tpe(Map())
          b->tpe(table + (v.name -> vtpe)) match {
            case err: ErrorType => err
            case btpe => Function(vtpe, btpe)
          }
        case term@App(f, p) =>
          f->tpe(table) match {
            case Function(pt, rt) if(pt == p->tpe(table)) => rt
            case Function(pt, _) => ErrorType(term, pt, p->tpe(table))
            case err: ErrorType => err
            case t => ErrorType(term, Function(p->tpe(table), WildcardType), t)
          }
      }
    }

}