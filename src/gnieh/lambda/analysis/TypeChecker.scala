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
import org.kiama.attribution.{ Attributable, Attribution }
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
   *           true: Bool           (T-True)
   *           
   *           false:Bool           (T-False)
   * 
   *            x:T in G
   *           ----------           (T-Var)
   *           G |- x : T
   *  
   *         G,x:T |- t : T'
   *       -------------------      (T-Abs)
   *       G |- \x:T.t : T->T'
   *  
   *  G |- t : T->T'   G |- t' : T
   *  ----------------------------  (T-App)
   *         G |- t t' : T'
   */
  lazy val tpe: Map[String, Type] => LambdaExpr ==> Type =
    paramAttr { table =>
      {
        case Var(x, UnknownType) =>
          table.get(x) match {
            case Some(t) => t
            case _ => UnknownType
          }
        case Var(_, t) => t
        case Abs(v, b) =>
          val vtpe = v -> tpe(Map())
          b -> tpe(table + (v.name -> vtpe)) match {
            case err: ErrorType => err
            case btpe => Function(vtpe, btpe)
          }
        case term@App(f, p) =>
          f -> tpe(table) match {
            case Function(pt, rt) if (pt == p -> tpe(table)) => rt
            case Function(pt, _) => ErrorType(term, pt, p -> tpe(table))
            case err: ErrorType => err
            case t => ErrorType(term, Function(p -> tpe(table), WildcardType), t)
          }
        case True | False => Bool
      }
    }

  def derivation(table: Map[String, Type], term: LambdaExpr): Derivation =
    term match {
      case Var(x, t) if table.contains(x) =>
        Derivation("T-Var", TypedExpr(table, term, table(x)), Nil)
      case Abs(Var(x, t), body) =>
        body -> tpe(table + (x -> t)) match {
          case UnknownType | _: ErrorType =>
            Derivation("T-Error", TypedExpr(table, term, UnknownType), Nil)
          case tpe =>
            Derivation("T-Abs", TypedExpr(table, term, tpe),
              List(derivation(table + (x -> t), body)))
        }

      case App(f, p) =>
        (f -> tpe(table), p -> tpe(table)) match {
          case (Function(tfp, tfr), tp) if (tfp == tp) =>
            Derivation("T-App", TypedExpr(table, term, tfr),
                List(derivation(table, f), derivation(table, p)))
          case _ => Derivation("T-Error", TypedExpr(table, term, UnknownType), Nil)
        }
      case True => Derivation("T-True", TypedExpr(Map(), term, Bool), Nil)
      case False => Derivation("T-False", TypedExpr(Map(), term, Bool), Nil)
      case _ => Derivation("T-Error", TypedExpr(table, term, UnknownType), Nil)
    }

}