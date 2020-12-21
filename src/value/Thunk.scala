package value
import context._
import expression._

class Thunk(body:Expression,definingEnv:Environment) extends Closure (Nil,body,definingEnv){
 override def apply(args:List[Value])={
  val temporaryE=new Environment(definingEnv)
  body.execute(temporaryE)
} 
}