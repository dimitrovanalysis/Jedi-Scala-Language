package expression
import context._
import value._

case class Conjunction(operands:List[Expression]) extends SpecialForm {
  
def execute(environment:Environment):Value = {
var finalBoole=true
var i=0
while (finalBoole==true && i < operands.length) {
 if (operands(i).execute(environment)==Boole(false))
   //set false / increment
   finalBoole=false
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
