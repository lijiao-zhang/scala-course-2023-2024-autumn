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

    val int = 42

    def not(b: Boolean): Boolean =  if (b) false else true   // here is my greatest solution

    def and(left: Boolean, right: Boolean): Boolean =  if (left) right else false

    def or(left: Boolean, right: Boolean): Boolean =    if (left) true else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = (a, b) =>

      @tailrec
      def multiply(acc: BigInt, a: BigInt, b: BigInt): BigInt =
        if b == 0 then acc
        else if b == 1 then acc + a
        else multiply(acc + a, a, b - 1)

      multiply(0, a, b)


    val power: (BigInt, BigInt) => BigInt = (v, exp) =>

      @tailrec
      def pow(acc: BigInt, v: BigInt, exp: BigInt): BigInt =
        if exp == 0 then acc
        else if exp == 1 then multiplication(acc, v)
        else pow(multiplication(acc, v), v, exp - 1)

      if exp == 0 then 1
      else if v == 0 then 0
      else pow(1, v, exp)

    val fermatNumber: Int => BigInt = n => {
      val p = power(2, power(2, n))
      p + 1
    }

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    val lookAndSaySequenceElement: Int => BigInt = n => {
      if (n == 1) BigInt(1)
      else {
        def describe(b: BigInt): BigInt = {
          val s = b.toString

          def count(c: Char, s: String): Int = {
            if (s.isEmpty || s.head != c) 0
            else 1 + count(c, s.tail)
          }

          if (s.isEmpty) BigInt(0)
          else {
            val c = s.head
            val k = count(c, s)
            BigInt(k.toString + c) + describe(BigInt(s.substring(k).toInt))
          }
        }

        describe(lookAndSaySequenceElement(n - 1))
      }
    }
  end `Look-and-say Sequence`

end Homework
