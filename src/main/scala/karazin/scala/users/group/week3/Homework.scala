package karazin.scala.users.group.week3

import scala.annotation.targetName

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)

    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(int: Int) = ???
  
    override def toString: String = s"Nat($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    infix def +(that: Nat): Nat = that
    
    infix def -(that: Nat): Nat = throw new Exception("A nat can't be negative")
    
    // Optional task
    def toInt: Int = ???

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean =
      if (obj.isInstanceOf[Zero] || (obj.asInstanceOf[Nat].isZero)) true else false

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat = if(that.isZero) this else n + that.successor
    
    infix def -(that: Nat): Nat = if(that.isZero) this else n - that.predecessor
    
    // Optional task
    def toInt: Int = ???

    override def equals(obj: Any): Boolean =
      obj match {
        case nat: Nat if nat.isZero == this.isZero => nat.successor == nat.successor
        case _ => false
      }

