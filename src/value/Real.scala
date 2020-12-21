package value
import context._
import expression._

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {

override def hashCode =this.toString.##  
override def toString = value.toString
override def canEqual(that:Any) = that.isInstanceOf[Real]
override def equals(that:Any): Boolean = 
  that match{
     case that:Real => this.canEqual(that) && (that.value==this.value)
     case _ => false
  }

def +(that:Real) =Real(this.value +that.value)
def -(that:Real) =Real(this.value -that.value)
def *(that:Real) =Real(this.value *that.value)
def /(that:Real) ={if (that.value == 0.0) throw new JediException("Can't Divide by 0")
 else Real(this.value/that.value)
}

def unary_- :Real =Real(-this.value)
def compare(that: Real):Int = if (this.value < that.value) -1 
else if (that.value < this.value) 1 else 0
}