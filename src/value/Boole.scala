package value
import expression._

case class Boole(val value:Boolean) extends Literal {
  
def &&(that:Boole) = if (this.value&&that.value) Boole(true) else Boole(false)
def ||(that:Boole) = if (this.value||that.value) Boole(true) else Boole(false)
def unary_! = if (this.value) Boole(false) else Boole(true)
  //overrides 
override def toString = value.toString
override def hashCode = this.toString.##
override def equals(that:Any):Boolean = 
   that match {
    case that:Boole => this.canEqual(that) && (that.value==this.value)
    case _ => false }
}