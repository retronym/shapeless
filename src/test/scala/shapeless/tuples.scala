/*
 * Copyright (c) 2011 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless

import org.junit.Test
import org.junit.Assert._

class TupleTests {
  import Tuples._
  import HList._
  import Poly._

  trait Fruit
  case class Apple() extends Fruit
  case class Pear() extends Fruit
  case class Banana() extends Fruit
  
  val a : Apple = Apple()
  val p : Pear = Pear()
  val b : Banana = Banana()

  def typed[T](t : => T) {}
  
  @Test
  def testTuples {
    val t1 = (23, "foo", 2.0, true)
    
    val h1 = t1.hlisted
    typed[Int :: String :: Double :: Boolean :: HNil](h1)
    assertEquals(23 :: "foo" :: 2.0 :: true :: HNil, h1)
    
    val h2 = hlisted(t1)
    typed[Int :: String :: Double :: Boolean :: HNil](h2)
    assertEquals(23 :: "foo" :: 2.0 :: true :: HNil, h2)
    
    val l2 = 23 :: "foo" :: 2.0 :: true :: HNil
    
    val t3 = l2.tupled
    typed[(Int, String, Double, Boolean)](t3)
    assertEquals((23, "foo", 2.0, true), t3)
    
    val t4 = tupled(l2)
    typed[(Int, String, Double, Boolean)](t4)
    assertEquals((23, "foo", 2.0, true), t4)
    
    val t5 = (23, "foo")
    val t6 = (false, 3.0)
    
    val t7 = (t5.hlisted ::: t6.hlisted).tupled
    typed[(Int, String, Boolean, Double)](t7)
    assertEquals((23, "foo", false, 3.0), t7)
    
    val t8 = (Set(2), Set("foo"))
    val t8b = (t8.hlisted map choose).tupled
    typed[(Option[Int], Option[String])](t8b)
    assertEquals((Option(2), Option("foo")), t8b)
  }
  
  @Test
  def testTupleOps {
    import Nat._
    
    val t1 = (23, "foo", 2.0, true)
    val t2 = ('c', ())
    
    val h = t1.head
    val t = t1.tail
    
    val t3 = "bar" :: t2
    
    val t4 = t1 ::: t2
    
    val t5 = t1 reverse_::: t2
    
    val a1 = t1[_0]
    val a2 = t1[_1]
    val a3 = t1[_2]
    val a4 = t1[_3]

    val av1 = t1(_0)
    val av2 = t1(_1)
    val av3 = t1(_2)
    val av4 = t1(_3)
    
    val l = t1.last
    val t6 = t1.init
    
    val s = t1.select[String]
    
    val tv7 = t1 take _2
    val tv8 = t1 drop _2
    
    val (t9, t10) = t1 split _2
    
    val t11 = t1.reverse

    val t12 = (Set(2), Set("foo"))
    val t13 = t12 map choose
    typed[(Option[Int], Option[String])](t13)
    
    val t14 = t1 map option
    val t15 = t14.foldLeft(true)(isDefined)(_ && _)
    
    val t16 = (a, p, a, p)
    val t17 = t16.unify
    
    val t18 = (a, p, b, p)
    val t19 = t18.unify
  }
  
  @Test
  def testCaseClasses {
    case class Foo(a : Int, b : String, c : Double)
    
    val f1 = Foo(23, "foo", 2.3)
    val t1 = Foo.unapply(f1).get
    val hf = t1.hlisted
    val f2 = Foo.tupled(hf.tupled)
    assertEquals(f1, f2)
  }
}