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
   *        | `:enable-typing'
   *        | `:disable-typing'
   *        | `:type' Expr
   *        | `:derivation' Expr
   *        | Assign
   *        | Expr
   */
  lazy val line: Parser[Node] =
    quit |
      help |
      normalOrder |
      callByName |
      callByValue |
      showSteps |
      hideSteps |
      showAliases |
      hideAliases |
      env |
      load |
      save |
      rm |
      deBruijn |
      enableTyping |
      disableTyping |
      showType |
      derivation |
      assign |
      expr

  /**
   * Expr ::= Expr Factor
   *        | Lambda
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
   *          | Boolean
   *          | Variable
   *          | Number
   *          | `(' Expr `)'
   */
  lazy val factor: PackratParser[LambdaExpr] = (ident ^^ { name => parsedIdent(name) }
    | boolean
    | variable
    | number
    | "(" ~> expr <~ ")")

  /**
   * Lambda ::= `\u03BB' Variable [`:' Type] `.' Expr
   *          | `\' Variable [`:' Type] `.' Expr
   */
  lazy val lambda: Parser[Abs] =
    (("\\" | "\u03BB") ~> variable ~ opt(":" ~> tpe)) ~ ("." ~> expr) ^^
      {
        case v ~ None ~ b => Abs(v, b)
        case Var(v, _) ~ Some(t) ~ b => Abs(Var(v, t), b)
      }

  lazy val variable: Parser[Var] = "[a-z]".r ^^ { name => Var(name) }

  /**
   * Boolean ::= `true'
   *           | `false'
   */
  lazy val boolean: Parser[LambdaExpr] =
    "true" ^^^ True |
      "false" ^^^ False

  /**
   * Number ::= [0-9]+
   */
  lazy val number: Parser[Number] =
    "[0-9]+".r ^^ { n => Number(n.toInt) }

  /**
   * Type ::= Type `->' Type
   *        | Type `\u2192' Type
   *        | `Bool'
   *        | `Nat'
   *        | `(' Type `)'
   */
  lazy val tpe: PackratParser[Type] =
    (tpe <~ ("->" | "\u2192")) ~ tpe ^^ { case t1 ~ t2 => Function(t1, t2) } |
      "Bool" ^^^ Bool |
      "Nat" ^^^ Nat |
      "(" ~> tpe <~ ")"

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