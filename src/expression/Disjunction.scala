package expression
import context._
import value._

case class Disjunction(operands:List[Expression]) extends SpecialForm{
def execute(environment: Environment):Value = {
//boolean to get out of loop
//increment to control sections
var finalBoole=false
var i=0
while ((i<operands.length) && (finalBoole==false)) {
   if (operands(i).execute(environment) == Boole(true)) 
   finalBoole = true
   i=i+1
}
Boole(finalBoole) //return
}
  
}

/*

for(i <- 0 to operands.length && finalBoole==false)
* i.execute(environment) == true
* finalboole=true
*/
