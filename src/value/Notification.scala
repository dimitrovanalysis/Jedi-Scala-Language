package value

//pops up in console after an action warrents a response
class Notification(val msg: String) extends Value {
override def toString:String = msg
}
object Notification {
def apply(msg: String) = new Notification(msg)
val OK = Notification("ok")
val UNSPECIFIED = Notification("unspecified")
val DONE = Notification("done")
} 