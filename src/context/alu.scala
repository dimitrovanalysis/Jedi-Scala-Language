package context
import expression._
import value._
import collection.mutable._


/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, list: List[Value]): Value = {
    opcode.name match {
      case "add" => add(list)
      case "mul" => mul(list)
      case "sub" => sub(list)             
      case "div" => div(list)
      case "less" => less(list) //binary
      case "more" => more(list) // binary
      case "equals" => equals(list) // note: equals(7, true) = false, not error
      case "unequals" => unequals(list) // binary, = not(equals(list))?
      case "not" => not(list) // unary
      // primitive I/O ops:
      case "write" => write(list)
      case "prompt" => prompt(list)
      case "read" => read(list)
      // variables
      case "dereference" => dereference(list)
      case "var" => makeVar(list)
      // store ops
      case "store" => store(list)
      case "put" => put(list)
      case "rem" => rem(list)
      case "contains" => contains(list)
      case "map" => map(list)
      case "filter" => filter(list)
      case "get" => get(list)
      case "addLast" => addLast(list)
      case "size" => size(list)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
    private def toInt(arg: Value): Option[Integer] =
      if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
      
    private def toReal(arg: Value): Option[Real] =
      if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
      else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
      else None
      
    private def toChars(arg: Value): Option[Chars] =
      if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None
      
    private def add(list: List[Value]) = {
      val args2 = list.map(toInt).filter(_ != None)
      if (args2.size == list.size) args2.flatten.reduce(_+_)
      else {
        val args3 = list.map(toReal).filter(_ != None)
        if (args3.size == list.size) args3.flatten.reduce(_+_)
        else {
          val args4 = list.map(toChars).filter(_ != None)
          if (args4.size == list.size) args4.flatten.reduce(_+_)
          else {
            throw new TypeException("Inputs to + must be numbers or texts")
          }
        }
      }
    }
  
  def less(list: List[Value]): Value = {
      if (list.length  != 2) throw new TypeException("less expects two inputs")
      val args2 = list.map(toInt).filter(_ != None)
      if (args2.size == list.size) Boole(args2(0) < args2(1))
      else {
        val args3 = list.map(toReal).filter(_ != None)
        if (args3.size == list.size) Boole(args3(0) < args3(1))
        else {
          val args4 = list.map(toChars).filter(_ != None)
          if (args4.size == list.size) Boole(args4(0) < args4(1))
          else throw new TypeException("Inputs to < must be numbers or texts")
        }
      }
   }
  
def div(list: List[Value]) = {
   val args2 = list.map(toInt).filter(_ != None)
   if (args2.size == list.size) args2.flatten.reduce(_/_)
   else {val args3 = list.map(toReal).filter(_ != None)
     if (args3.size == list.size) args3.flatten.reduce(_/_)
      else throw new TypeException("only numbers")
    }
}   
  
def more(list: List[Value]): Value = {
   if (list.length != 2) throw new TypeException("input only 2")
   val args2 = list.map(toInt).filter(_ != None)
   if (args2.size == list.size) Boole(args2(0) > args2(1))
   else {val args3 = list.map(toReal).filter(_ != None)
     if (args3.size == list.size) Boole(args3(0) > args3(1))
     else {val args4 = list.map(toChars).filter(_ != None)
      if (args4.size == list.size) Boole(args4(0) > args4(1))
      else throw new TypeException("numbers + text only")
     }
   }
}
  
def sub(list: List[Value]) = {
 val args2 = list.map(toInt).filter(_ != None)
 if (args2.size == list.size) args2.flatten.reduce(_-_)
 else {
  val args3 = list.map(toReal).filter(_ != None)
  if (args3.size == list.size) args3.flatten.reduce(_-_)
  else throw new TypeException("only numbers")
 }
}  

def mul(list: List[Value]) = {
  val args2 = list.map(toInt).filter(_ != None)
  if (args2.size == list.size) args2.flatten.reduce(_*_)
  else {
   val args3 = list.map(toReal).filter(_ != None)
   if (args3.size == list.size) args3.flatten.reduce(_*_)
   else throw new TypeException("only numbers")
  }
}
   
def unequals(list: List[Value]): Value = {
  if (list.length != 2) throw new TypeException("input only 2")
 if (not(List(equals(list))) == Boole(true)) Boole(true) else Boole(false)
}  
  
def not(list: List[Value]): Value = {
 if (list.length != 1) throw new TypeException("input only 1 ")
 if (!list(0).isInstanceOf[Boole]) throw new TypeException("input only boole")
 !(list(0).asInstanceOf[Boole])
} 

def equals(list:List[Value]):Value = {
if (list.length <2) throw new TypeException("input only 2")
   var fin =true
   var location =0
   while (fin ==true && location < list.length) {
    if (list(0) !=list(location)) {
      fin =false
    }
    location=location + 1
   }
 Boole(fin)
 }
   
    
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

  
  // jedi 1.0 done , 2.0 starts-----------------------------------
   
   // variable ops
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {args(0).asInstanceOf[Variable].content}
   
   // creates a new variable cobtaining args(0)
   private def makeVar(args: List[Value]) = {new Variable(args(0))}
   
   // store ops
   
   // returns a new store containing args
   private def store(args: List[Value]) = {new Store(args.to[ArrayBuffer])}
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = {
     val p = args(0).asInstanceOf[Integer]
     val s = args(1).asInstanceOf[Store]
     s.rem(p)
     Notification.DONE}
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = {
     val p = args(0).asInstanceOf[Integer]
     val s = args(1).asInstanceOf[Store]
     s.get(p)}
   
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = {
     val f = args(0).asInstanceOf[Closure]
     val s = args(1).asInstanceOf[Store]
     s.map(f)} 
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = {
     val f = args(0).asInstanceOf[Closure]
     val s = args(1).asInstanceOf[Store]
     s.filter(f)} 
   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = {
     val v = args(0).asInstanceOf[Value]
     val s = args(1).asInstanceOf[Store]
     s.contains(v)}
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = {
     val v = args(0).asInstanceOf[Value]
     val s = args(1).asInstanceOf[Store]
     s.add(v)
     Notification.DONE}
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = {
     val s = args(0).asInstanceOf[Store]
     s.size}
   
   //etc.
}