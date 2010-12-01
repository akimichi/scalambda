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

trait LambdaParsers extends RegexParsers with PackratParsers {

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
      
  /**
   * Variable ::= char
   */
  def variable: Parser[Var] =
    "[a-z]".r ^^ Var

  /**
   * Assign ::= Ident `=' Expr
   */
  lazy val assign: Parser[Assign] =
    (ident <~ "=") ~ expr ^^ { case name ~ expr => Assign(name, expr) }

  /**
   * Ident ::= [A-Z][a-z0-9_]*
   */
  lazy val ident = "[A-Z][A-Za-z0-9_]*".r

  lazy val file: Parser[List[Assign]] = opt("#.*\n".r) ~> rep(assign <~ ";")

  /**
   * Path ::= [A-Za-z0-9_\-]+
   */
  lazy val path: Parser[String] = "[A-Za-z0-9_\\-]+(\\.lbd)?".r
  
  lazy val rm: Parser[RemoveEnv] = ":rm"~>rep1(ident) ^^ RemoveEnv

  lazy val load: Parser[LoadLib] = ":load" ~> path ^^ LoadLib

  lazy val save: Parser[SaveLib] = ":save" ~> path ^^ SaveLib

  lazy val quit: Parser[Command] = ":quit" ^^^ Quit

  lazy val help: Parser[Command] = ":help" ^^^ Help

  lazy val normalOrder: Parser[Command] = ":normal-order" ^^^ NormalOrder
  
  lazy val callByName: Parser[Command] = ":call-by-name" ^^^ CallByName
  
  lazy val callByValue: Parser[Command] = ":call-by-value" ^^^ CallByValue

  lazy val showSteps: Parser[Command] = ":show-steps" ^^^ ShowSteps

  lazy val hideSteps: Parser[Command] = ":hide-steps" ^^^ HideSteps

  lazy val showAliases: Parser[Command] = ":show-aliases" ^^^ ShowAliases

  lazy val hideAliases: Parser[Command] = ":hide-aliases" ^^^ HideAliases

  lazy val env: Parser[Command] = ":env" ^^^ Env

  lazy val deBruijn: Parser[DeBruijnCommand] = ":de-bruijn"~>expr ^^ DeBruijnCommand
  
  private def parsedIdent(id: String): LambdaExpr =
    environment.getExpr(id) match {
      case Some(expr) => expr
      case None => LambdaError(id + " not found in the environment\n")
    }
}