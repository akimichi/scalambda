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
package gnieh.lambda.ast

import scala.collection.mutable.Map
import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }
import java.io.File

trait LambdaParsers extends RegexParsers with PackratParsers {
  
  val environment = Map.empty[String, LambdaExpr]

  /** 
   * Line ::= `:quit'
   *        | `:help'
   *        | `:normal-order'
   *        | `:show-steps'
   *        | `:hide-steps'
   *        | `:env'
   *        | `:save' Path
   *        | `:load' Path
   *        | Assign
   *        | Expr
   */
  lazy val line: Parser[Node] = 
    quit | help | normalOrder | showSteps | hideSteps | env | load | save | assign | expr
  
  /**
   * Expr ::= Expr Factor
   *        | `\u03BB' Variable `.' Expr
   *        | `\' Variable `.' Expr
   *        | Factor
   *        | error
   */
  lazy val expr: PackratParser[LambdaExpr] =
    expr~factor ^^ {case f~p => App(f, p)} |
    (("\\" | "\u03BB")~>variable)~("."~>expr) ^^ {case v ~ b => Abs(v, b)} |
    factor |
    failure("expression expected")

  /**
   * Factor ::= Ident
   *          | Variable
   *          | `(' Expr `)'
   */
  lazy val factor: PackratParser[LambdaExpr] =
    ident ^^ {name => parsedIdent(name)} |
    variable | "("~>expr<~")"

  /**
   * Variable ::= char
   */
  lazy val variable: Parser[Var] =
    "[a-z]".r ^^ { case c => Var(c.charAt(0)) }
    
  /**
   * Assign ::= Ident `=' Expr
   */
  lazy val assign: Parser[Assign] =
    (ident<~"=")~expr ^^ {case name~expr => Assign(name, expr)}
  
  /**
   * Ident ::= [A-Z][a-z0-9_]*
   */
  lazy val ident = "[A-Z][A-Za-z0-9_]*".r
  
  lazy val file: Parser[List[Assign]] = opt("#.*\n".r)~>rep(assign<~";")
  
  /**
   * Path ::= [A-Za-z0-9_\-]+
   */
  lazy val path: Parser[String] = "[A-Za-z0-9_\\-]+".r
  
  lazy val load: Parser[LoadLib] = ":load"~>path ^^ LoadLib
  
  lazy val save: Parser[SaveLib] = ":save"~>path ^^ SaveLib

  lazy val quit: Parser[Command] = ":quit" ^^ { _ => Quit }

  lazy val help: Parser[Command] = ":help" ^^ { _ => Help }

  lazy val normalOrder: Parser[Command] = ":normal-order" ^^ { _ => NormalOrder }

  lazy val showSteps: Parser[Command] = ":show-steps" ^^ { _ => ShowSteps }

  lazy val hideSteps: Parser[Command] = ":hide-steps" ^^ { _ => HideSteps }
  
  lazy val env: Parser[Command] = ":env" ^^ { _ => Env }
  
  private def parsedIdent(id: String): LambdaExpr =
    environment.get(id) match {
      case Some(expr) => expr
      case None => LambdaError(id + " not found in the environment")
  }
}