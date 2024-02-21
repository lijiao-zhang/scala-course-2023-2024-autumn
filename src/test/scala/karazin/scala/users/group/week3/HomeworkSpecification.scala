package karazin.scala.users.group.week3

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import Homework._

object HomeworkSpecification extends Properties("Homework"):

  property("addition") = forAll { (nat1: Nat, nat2: Nat) =>
    (nat1 + nat2).toInt == nat1.toInt + nat2.toInt
  }

property("subtraction") = forAll { (nat1: Nat, nat2: Nat) =>
  if (nat1.toInt >= nat2.toInt) (nat1 - nat2).toInt == nat1.toInt - nat2.toInt
  else throws(classOf[Exception]) { nat1 - nat2 }
}

property("toInt") = forAll { (nat: Nat) =>
  nat.toInt == (0 max nat.toInt)
}





end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):

  property("isZero") = forAll { (nat: Zero.type) =>
    nat.isZero
  }

  property("predecessor") = throws(classOf[Exception]) { Zero.predecessor }

  property("addition") = forAll { (nat: Nat) =>
    (Zero + nat) == nat
  }

  property("subtraction") = forAll { (nat: Nat) =>
    (Zero - nat) == Zero
  }

  property("toInt") = forAll { (nat: Zero.type) =>
    nat.toInt == 0
  }



end ZeroSpecification


object SuccSpecification extends Properties("Succ"):

  property("isZero") = forAll { (nat: Succ) =>
    !nat.isZero
  }

property("predecessor") = forAll { (nat: Succ) =>
  nat.predecessor.toInt == (nat.toInt - 1)
}

property("addition") = forAll { (nat1: Nat, nat2: Nat) =>
  val succ1 = Succ(nat1)
  val succ2 = Succ(nat2)
  (succ1 + succ2).toInt == succ1.toInt + succ2.toInt
}

property("subtraction") = forAll { (nat1: Nat, nat2: Nat) =>
  val succ1 = Succ(nat1)
  val succ2 = Succ(nat2)
  if (succ1.toInt >= succ2.toInt) (succ1 - succ2).toInt == succ1.toInt - succ2.toInt
  else throws(classOf[Exception]) { succ1 - succ2 }
}

property("toInt") = forAll { (nat: Succ) =>
  nat.toInt == (nat.predecessor.toInt + 1)
}


end SuccSpecification

object NatSpecification extends Properties("Nat"):
 
  // Nat1 + Nat2 => ... 
property("addition associativity") = forAll { (nat1: Nat, nat2: Nat, nat3: Nat) =>
  ((nat1 + nat2) + nat3) == (nat1 + (nat2 + nat3))
}

property("addition commutativity") = forAll { (nat1: Nat, nat2: Nat) =>
  (nat1 + nat2) == (nat2 + nat1)
}

property("subtraction with negative result") = forAll { (nat1: Nat, nat2: Nat) =>
  if (nat1.toInt < nat2.toInt) throws(classOf[Exception]) { nat1 - nat2 }
  else true
}


end NatSpecification
  
