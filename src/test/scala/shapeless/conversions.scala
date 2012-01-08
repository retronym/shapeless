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

class ConversionTests {
  import Functions._

  def typed[T](t : => T) {}
  
  @Test
  def testFunctions {
    val sum : (Int, Int) => Int = _+_
    val prd : (Int, Int, Int) => Int = _*_*_
    
    val hlsum = sum.hlisted
    typed[(Int :: Int :: HNil) => Int](hlsum)
    
    val hlprd = prd.hlisted
    typed[(Int :: Int :: Int :: HNil) => Int](hlprd)
    
    trait A
    trait B extends A
    trait C extends A
    
    val a = new A {}
    val b = new B {}
    
    val ab : A => B = (a : A) => b
    
    val hlab = ab.hlisted
    typed[(A :: HNil) => B](hlab)
    
    def foo[F, L <: HList, R, HF](f : F, l : L)(implicit hl : FnHListerAux[F, HF], ev : HF <:< (L => R)) = hl(f)(l)
    val s2 = foo(sum, 2 :: 3 :: HNil)
    val ab2 = foo(ab, b :: HNil)
  }
}
