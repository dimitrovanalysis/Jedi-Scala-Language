package expression
import value._
import context._

case class Assignment(val v: Identifier, val newV: Expression) extends SpecialForm{
def execute(environment: Environment) = {    
 val variable = v.execute(environment)
 
 if(variable.isInstanceOf[Variable]){
  val result = newV.execute(environment)
  variable.asInstanceOf[Variable].content = result
 }
  else throw new JediException("must be a variable")
       
 Notification.DONE
}
}

