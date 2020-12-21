package value
import expression._

case class Chars(val value:String) extends Literal with Ordered[Chars] with Equals {

def ==(that:Chars) = if(this.equals(that)) Boole(true) else Boole(false)
def substring(i:Integer, k:Integer) =Chars(this.value.substring(i.value,k.value))
def +(that:Chars):Chars ={ //in his order
 println(Notification.OK)
 Chars(this.value +that.value)}

def compare(that:Chars):Int = if (this.value<that.value) -1 
else if (that.value < this.value) 1 else 0

override def toString = value.toString
override def hashCode = this.toString.##
override def equals(that:Any):Boolean = 
  that match {
   case that: Chars => this.canEqual(that) && (that.value==this.value)
   case _=> false
 }
  



}