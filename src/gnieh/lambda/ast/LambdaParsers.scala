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
package ast

import scala.collection.mutable.Map
import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }
import java.io.File

import util.environment
import types._

trait LambdaParsers extends RegexParsers with PackratParsers
                    with CommandParsers {

  /** 
   * Line ::= `:quit'
   *        | `:help'
   *        | `:normal-order'
   *        | `:call-by-name'
   *        | `:call-by-value'
   *        | `:show-steps'
   *        | `:hide-steps'
   *        | `:env'
   *        | `:save' Path
   *        | `:load' Path
   *        | `:show-aliases'
   *        | `:hide-aliases'
   *        | `:rm' Ident+
   *        | `:de-bruijn' Expr
   *        | Assign
   *        | Expr
   */
  lazy val line: Parser[Node] =
    quit | help | normalOrder | callByName | callByValue | showSteps | hideSteps | showAliases | 
    hideAliases | env | load | save | assign | rm | deBruijn | expr

  /**
   * Expr ::= Expr Factor
   *        | `\u03BB' Variable `.' Expr
   *        | `\' Variable `.' Expr
   *        | Factor
   *        | error
   */
  lazy val expr: PackratParser[LambdaExpr] =
    expr ~ factor ^^ { case f ~ p => App(f, p) } |
    lambda |
    factor |
    failure("expression expected")

  /**
   * Factor ::= Ident
   *          | Variable
   *          | `(' Expr `)'
   */
  lazy val factor: PackratParser[LambdaExpr] = (
      ident ^^ { name => parsedIdent(name) }
    | variable
    | "(" ~> expr <~ ")"
  )

  lazy val lambda: Parser[Abs] =
    (("\\" | "\u03BB") ~> variable) ~ ("." ~> expr) ^^ { case v ~ b => Abs(v, b) }
      
  lazy val variable: Parser[Var] = "[a-z]".r~opt(":"~>tpe) ^^ {
    case name~Some(tpe) => Var(name, tpe)
    case name~None => Var(name)
  }
  
  lazy val tpe: PackratParser[Type] = 
    "Bool" ^^^ Bool |
    "Nat" ^^^ Nat |
    (tpe<~("->" | "\u2192"))~tpe ^^ {case t1~t2 => Function(t1,t2)}
    
  /**
   * Ident ::= [A-Z][a-z0-9_]*
   */
  lazy val ident = "[A-Z][A-Za-z0-9_]*".r
    
  private def parsedIdent(id: String): LambdaExpr =
    environment.getExpr(id) match {
      case Some(expr) => expr
      case None => LambdaError(id + " not found in the environment\n")
    }
}