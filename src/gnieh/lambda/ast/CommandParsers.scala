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

import scala.util.parsing.combinator.{ RegexParsers, PackratParsers }

/**
 * @author Lucas Satabin
 *
 */
trait CommandParsers extends RegexParsers {
  
  this: LambdaParsers =>
  
  /**
   * Assign ::= Ident `=' Expr
   */
  lazy val assign: Parser[Assign] =
    (ident <~ "=") ~ expr ^^ { case name ~ expr => Assign(name, expr) }

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
  
  lazy val enableTyping: Parser[Command] = ":enable-typing" ^^^ EnableTyping
  
  lazy val disableTyping: Parser[Command] = ":disable-typing" ^^^ DisableTyping

}