package expression
import context._
import value._

// main caller for arguments, then goes to alu
case class FunCall(operator:Identifier, operands:List[Expression]) extends Expression{
def execute(environment:Environment):Value = {
   
 var myEnv = environment
 if(!flags.staticScope)myEnv = new Environment()
  
 var argumentList =List[Value]()
 val flagParam = flags.paramPassing
  
 flagParam match
 {
  case flags.BY_VALUE=> argumentList = operands.map(_.execute(myEnv))
  case flags.BY_NAME => argumentList = operands.map((exp:Expression)=>new Thunk(exp,myEnv))
  case flags.BY_TEXT => argumentList = operands.map((exp:Expression)=>new Text(exp))
 }
  
 if(myEnv.contains(operator)){
  val closureTest = operator.execute(myEnv) 
  if (closureTest.isInstanceOf[Closure]){
  val closure = closureTest.asInstanceOf[Closure]
  closure.apply(argumentList)
  }else{
    throw new TypeException("Functions Only")}
 }else{
   val a = operands.map(_.execute(myEnv))
   alu.execute(operator,a) // finally to alu
 }
}
}

/*
create main class to use operator and operands and execute them in the alu

* for (index <- operands) {argumentList =argumentList:+ index.execute(environment)}
   alu.execute(operator,argumentList) //finally to alu
 }
* */
