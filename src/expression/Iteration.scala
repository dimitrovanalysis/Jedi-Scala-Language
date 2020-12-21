package expression
import context._
import value._

case class Iteration(val condition:Expression,val body:Expression) extends SpecialForm{
def execute(environment: Environment) ={
 while(condition.execute(environment)==Boole(true))
  body.execute(environment)
  
  Notification.DONE
 }
}