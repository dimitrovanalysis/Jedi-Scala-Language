package expression
import value._
import context._

trait Expression{
  def execute(environment:Environment): Value
}
