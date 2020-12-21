package expression
import context.Environment
import value._

case class Identifier(val name:String) extends Expression {
 def execute(environment: Environment) = {
  val execute =  environment(this)
  
  execute match
  {
  case thunk: Thunk => thunk.apply(List(execute))
  case text: Text =>text.body.execute(environment)
  case _=> execute
  }
}  
 override def toString:String = name
} 