package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean = if (b) false else true // here is my greatest solution

    def and(left: Boolean, right: Boolean): Boolean = if (left) (if (right) true else false) else false

    def or(left: Boolean, right: Boolean): Boolean = if (left) true else (if (right) true else false)

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) =>
      @tailrec
      def multiplicationReq(a: BigInt, b: BigInt, cur: BigInt): BigInt =
        if ((a <= 0) || (b <= 0)) then cur
        else multiplicationReq(a, b - 1, cur + a)

      multiplicationReq(a, b, cur = 0)

    val power: (BigInt, BigInt) => BigInt = (a, b) =>
      @tailrec
      def powerReq(a: BigInt, b: BigInt, cur: BigInt): BigInt =
        if b == 0 then cur
        else powerReq(a, b - 1, multiplication(cur, a))

      powerReq(a, b, cur = 1)

    val fermatNumber: Int => BigInt = v => power(2, power(2, v)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = n =>
      @tailrec
      def loop(n: Int, num: String): BigInt = {
        if n <= 1 then BigInt(num)
        else loop(n - 1, lookAndSayNext(num))
      }

      def lookAndSayNext(number: String): String = {
        val result = new StringBuilder

        @tailrec
        def loop(numberString: String, repeat: Char, times: Int): String =
          if (numberString.isEmpty) result.toString()
          else if (numberString.head != repeat) {
            result.append(times).append(repeat)
            loop(numberString.tail, numberString.head, 1)
          } else loop(numberString.tail, numberString.head, times + 1)

        loop(number.tail + " ", number.head, 1)
      }

      loop(n, "1")

  end `Look-and-say Sequence`

end Homework