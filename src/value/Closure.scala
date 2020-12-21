package value
import expression._
import context._

class Closure(val params:List[Identifier],val body:Expression, val definingEnv:Environment, val callingEnv:Environment=new Environment) extends Value{
def apply(arguments:List[Value]) ={
 if(flags.staticScope){
  val temporaryE = new Environment(definingEnv)
  temporaryE.bulkPut(params,arguments)
  body.execute(temporaryE)
 }
 else{
  val temporaryE = new Environment(callingEnv)
  temporaryE.bulkPut(params,arguments)
  body.execute(temporaryE)
 }
} 
}