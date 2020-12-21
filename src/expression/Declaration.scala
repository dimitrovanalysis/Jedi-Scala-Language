package expression
import context._
import value._

case class Declaration(identifier:Identifier,expression:Expression) extends SpecialForm {
def execute(environment:Environment):Value ={
 environment.put(identifier,expression.execute(environment))
 //respond
 Notification.OK
}
}