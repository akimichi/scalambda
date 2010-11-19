package gnieh.lambda

import ast._
import strategy._

import org.kiama.util.ParsingREPL

object Lambda extends ParsingREPL[Node] with LambdaParsers {

  var strategy: InterpretationStrategy = NormalOrderStrategy
  
  var showReductionSteps = true
  
  override def setup(args: Array[String]) = {
    strategy.init
    println(
"""λ Interpreter © 2010 Lucas Satabin
type :help for help and :quit to quit""")
    true
  }
  
  override def prompt = "λ > "
  
  def process(n: Node) = n match {
    case Assign(name, expr) =>
      environment += (name -> expr)
      println(name + " added to the environment.")
    case le: LambdaExpr =>
      println(le)
      steps(le)
    case NormalOrder => switchTo(NormalOrderStrategy)
    case ShowSteps => showReductionSteps = true
    case HideSteps => showReductionSteps = false
    case Quit => exit
    case Help => println("""Available commands:
 :help            Display this help
 :quit            Quit the λ Interpreter
 :normal-order    Use normal order strategy to reduce the terms (default)
 :show-steps      Show the steps when reducing (enabled by default)
 :hide-steps      Do not show steps when reducing""")
  }

  def start = line
  
  private def switchTo(st: InterpretationStrategy) {
    strategy = st
    strategy.init
  }
  
  private def steps(le: LambdaExpr) {
    var current = le
    var last: LambdaExpr = null
    while(true) {
      last = current
      strategy(current) match {
        case Some(e) if last == e =>
          println("The term '" + last + "' diverges using the " + strategy.name + " strategy.")
          return
        case Some(e) =>
          current = e
          if(showReductionSteps)
            println(" => " + current)
        case None =>
          if(!showReductionSteps)
            println(" => " + current)
          println("Normal form reached with the " + strategy.name + " strategy.")
          return
      } 
    }
  }
  
}