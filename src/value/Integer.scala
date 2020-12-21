package value
import context._
import expression._

case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals {
override def toString = value.toString
override def canEqual(that: Any) =  that.isInstanceOf[Integer]
override def hashCode = this.toString.##
override def equals(that:Any):Boolean = 
 that match {
    case that:Integer => this.canEqual(that) &&(that.value==this.value)
    case _ => false}

def +(that:Integer) =Integer(this.value +that.value)
def -(that:Integer) =Integer(this.value -that.value)
def *(that:Integer) =Integer(this.value *that.value)
def /(that:Integer) ={
  if (that.value ==0) throw new JediException("Can't divide by 0")
  else Integer(this.value/that.value)
}
def unary_- = Integer(-this.value)
def compare(that:Integer):Int = if (this.value < that.value) -1 
else if (that.value < this.value) 1 else 0


}

object Integer {
  implicit def intToReal(nvalue:Integer):Real =Real(nvalue.value.toDouble)
}