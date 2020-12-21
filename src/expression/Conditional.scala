package expression
import context._
import value._

case class Conditional(first:Expression, secondOpt:Expression, thirdOpt:Expression = null) extends SpecialForm{
  //first always goes, second might and third might after that.
def execute(environment:Environment):Value = {
val ifExp = first.execute(environment)
ifExp match
{
  case bool: Boole =>if(bool.value) secondOpt.execute(environment)
                     else{
                       if(thirdOpt!=null) thirdOpt.execute(environment)
                       else Notification.UNSPECIFIED
                     }
  case _=> throw new TypeException
}

}
}
/*
if (!ifExp.isInstanceOf[Boole]) {
   throw new TypeException("expression needs to be boole.")
} //conditional execution = lazy execution

if (ifExp==true) secondOpt.execute(environment)
 else {if(thirdOpt !=null) thirdOpt.execute(environment) else Notification.UNSPECIFIED}

*/