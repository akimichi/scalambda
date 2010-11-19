package gnieh.lambda
package strategy

import ast._

/**
 * This trait describes the interface of an interpretation strategy
 * 
 * @author Lucas Satabin
 *
 */
trait InterpretationStrategy {
  
  val name: String
  
  def init
  
  /**
   * Reduces the given expression by one step
   * @param expr expression to reduce
   */
  def apply(expr: LambdaExpr): Option[LambdaExpr]

}