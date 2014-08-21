package fpinscala.monoids

import org.specs2.mutable._

class MonoidsSpec extends Specification {
  "intAddition" should {
  import fpinscala.monoids.Monoid.intAddition._
    "same value when add zero" in {
      val a = 33
      Monoid.intAddition.op(zero, a) must_== Monoid.intAddition.op(a, Monoid.intAddition.zero)
    }
    "same value when add order" in {
      val a = 99
      val b = 8
      val c = 31
      Monoid.intAddition.op(Monoid.intAddition.op(a, b), c) must_== Monoid.intAddition.op(a, Monoid.intAddition.op(b, c))
    }
  }

  "intMultiplication" should {
    import fpinscala.monoids.Monoid.intMultiplication._
    "same value when add zero" in {
      val a = 33
      op(zero, a) must_== op(a, zero)
    }
    "same value when add order" in {
      val a = 99
      val b = 8
      val c = 31
      op(op(a, b), c) must_== op(a, op(b, c))
    }
  }

  "booleanOr" should {
    import fpinscala.monoids.Monoid.booleanOr._
    "same value when add zero" in {
      val a = true
      op(zero, a) must_== op(a, zero)
    }
    "same value when add order" in {
      val a = true
      val b = false
      val c = true
      op(op(a, b), c) must_== op(a, op(b, c))
    }
  }

  "booleanAnd" should {
    import fpinscala.monoids.Monoid.booleanAnd._
    "same value when add zero" in {
      val a = true
      op(zero, a) must_== op(a, zero)
    }
    "same value when add order" in {
      val a = true
      val b = false
      val c = true
      op(op(a, b), c) must_== op(a, op(b, c))
    }
  }
  "foldLeft" should {
    "same value when add zero" in {
      Monoid.foldLeft(List(1, 2, 3))(List[Int]())((acc, a) => a + 1 :: acc) must_== List(2, 3, 4)
    }
  }
  "foldMap" should {
    import fpinscala.monoids.Monoid.intAddition._
    "_" in {
      Monoid.foldMap(IndexedSeq(1, 2, 3), Monoid.intAddition)(_ * 2) must_== 12
      Monoid.foldMap(IndexedSeq(1, 2, 3), Monoid.intAddition)(_ + 2) must_== 12
    }
  }
  "ordered" should {
    "Return True When Ordered Correctly" in {
      Monoid.ordered(IndexedSeq(1, 2, 3, 4)) must_== true
    }
    "Return False When Ordered Reversely" in {
      Monoid.ordered(IndexedSeq(4, 3, 2, 1)) must_== false
    }
    "Return False When Ordered Alternately" in {
      // 分割の最小をとってもtrue
      // 分割の最大をとってもtrue
      // 最小・最大それぞれとればfalse
      Monoid.ordered(IndexedSeq(1, 3, 2, 4)) must_== false
    }
  }
}
