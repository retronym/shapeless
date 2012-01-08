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

final class TupleOps[T <: Product, L <: HList](l : L) {
  import HList._
  
  def hlisted : L = l
  
  /**
   * Returns the head of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def head(implicit c : IsHCons[L]) : c.H = c.head(l) 

  /**
   * Returns that tail of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def tail[H, M <: HList](implicit c : IsHConsAux[L, H, M], tupler : Tupler[M]) : tupler.Out = c.tail(l).tupled
  
  /**
   * Prepend the argument element to this `HList`.
   */
  def ::[H](h : H)(implicit tupler : Tupler[H :: L]) : tupler.Out = (h :: l).tupled
  
  /**
   * Prepend the argument `HList` to this `HList`.
   */
  def :::[P <: Product, M <: HList, ML <: HList](prefix : P)
    (implicit
      hlister : HListerAux[P, M],
      prepend : PrependAux[M, L, ML],
      tupler : Tupler[ML]) : tupler.Out = prepend(hlister(prefix), l).tupled
  
  /**
   * Prepend the reverse of the argument `HList` to this `HList`.
   */
  def reverse_:::[P <: Product, M <: HList, ML <: HList](prefix : P)
    (implicit
      hlister : HListerAux[P, M],
      prepend : ReversePrependAux[M, L, ML],
      tupler : Tupler[ML]) : tupler.Out = prepend(hlister(prefix), l).tupled

  /**
   * Returns the ''nth'' of this `HList`. An explicit type argument must be provided. Available only if there is
   * evidence that this `HList` has at least ''n'' elements.
   */
  def apply[N <: Nat](implicit at : At[L, N]) : at.Out = at(l)

  /**
   * Returns the ''nth'' of this `HList`. Available only if there is evidence that this `HList` has at least ''n''
   * elements.
   */
  def apply[N <: Nat](n : N)(implicit at : At[L, N]) : at.Out = at(l)
  
  /**
   * Returns the last element of this `HList`. Available only if there is evidence that this `HList` is composite.
   */
  def last(implicit last : Last[L]) : last.Out = last(l)

  /**
   * Returns an `HList` consisting of all the elements of this `HList` except the last. Available only if there is
   * evidence that this `HList` is composite.
   */
  def init[I <: HList](implicit init : InitAux[L, I], tupler : Tupler[I]) : tupler.Out = init(l).tupled
  
  /**
   * Returns the first element of type `U` of this `HList`. An explicit type argument must be provided. Available only
   * if there is evidence that this `HList` has an element of type `U`.
   */
  def select[U](implicit selector : Selector[L, U]) : U = selector(l)
  
  /**
   * Returns the first ''n'' elements of this `HList`. Available only if there is evidence that this `HList` has at
   * least ''n'' elements.
   */
  def take[N <: Nat, M <: HList](n : N)(implicit take : TakeAux[L, N, M], tupler : Tupler[M]) : tupler.Out = take(l).tupled
  
  /**
   * Returns all but the  first ''n'' elements of this `HList`. Available only if there is evidence that this `HList`
   * has at least ''n'' elements.
   */
  def drop[N <: Nat, M <: HList](n : N)(implicit drop : DropAux[L, N, M], tupler : Tupler[M]) : tupler.Out = drop(l).tupled
  
  /**
   * Splits this `HList` at the ''nth'' element, returning the prefix and suffix as a pair. An explicit type argument
   * must be provided. Available only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def split[N <: Nat, P <: HList, S <: HList]
    (implicit
      split : SplitAux[L, N, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = split(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the ''nth'' element, returning the prefix and suffix as a pair. Available only if there is
   * evidence that this `HList` has at least ''n'' elements.
   */
  def split[N <: Nat, P <: HList, S <: HList](n : N)
    (implicit
      split : SplitAux[L, N, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = split(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the ''nth'' element, returning the reverse of the prefix and suffix as a pair. An explicit
   * type argument must be provided. Available only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def reverse_split[N <: Nat, P <: HList, S <: HList]
    (implicit
      split : ReverseSplitAux[L, N, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = split(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the ''nth'' element, returning the reverse of the prefix and suffix as a pair. Available
   * only if there is evidence that this `HList` has at least ''n'' elements.
   */
  def reverse_split[N <: Nat, P <: HList, S <: HList](n : N)
    (implicit
      split : ReverseSplitAux[L, N, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = split(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  def splitLeft[U, P <: HList, S <: HList]
    (implicit
      splitLeft : SplitLeftAux[L, U, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = splitLeft(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the first occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  def reverse_splitLeft[U, P <: HList, S <: HList]
    (implicit
      splitLeft : ReverseSplitLeftAux[L, U, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = splitLeft(HNil, l) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning the prefix and suffix as a pair.
   * An explicit type argument must be provided. Available only if there is evidence that this `HList` has an element
   * of type `U`.
   */
  def splitRight[U, P <: HList, S <: HList]
    (implicit
      splitRight : SplitRightAux[L, U, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = splitRight(l, HNil, HNil) ; (p.tupled, s.tupled) }

  /**
   * Splits this `HList` at the last occurrence of an element of type `U`, returning reverse of the prefix and suffix
   * as a pair. An explicit type argument must be provided. Available only if there is evidence that this `HList` has
   * an element of type `U`.
   */
  def reverse_splitRight[U, P <: HList, S <: HList]
    (implicit
      splitRight : ReverseSplitRightAux[L, U, P, S],
      tuplerP : Tupler[P],
      tuplerS : Tupler[S]) : (tuplerP.Out, tuplerS.Out) = { val (p, s) = splitRight(l, HNil, HNil) ; (p.tupled, s.tupled) }

  /**
   * Reverses this `HList`.
   */
  def reverse[M <: HList](implicit reverse : ReverseAux[L, M], tupler : Tupler[M]) : tupler.Out = reverse(HNil, l).tupled

  /**
   * Maps a higher rank function across this `HList`.
   */
  def map[HF, M <: HList](f : HF)(implicit mapper : MapperAux[HF, L, M], tupler : Tupler[M]) : tupler.Out = mapper(l).tupled

  /**
   * Replaces each element of this `HList` with a constant value.
   */
  def mapConst[C, M <: HList](c : C)(implicit mapper : ConstMapperAux[C, L, M], tupler : Tupler[M]) : tupler.Out = mapper(c, l).tupled
  
  /**
   * Maps a higher rank function ''f'' across this `HList` and folds the result using monomorphic combining operator
   * ''op''. Available only if there is evidence that the result type of `f` at each element conforms to the argument
   * type of ''op''.
   */
  def foldLeft[R, HF](z : R)(f : HF)(op : (R, R) => R)(implicit folder : LeftFolder[L, R, HF]) : R = folder(l, z, op)
  
  /**
   * Returns an `HList` typed as a repetition of the least upper bound of the types of the elements of this `HList`.
   */
  def unify[M <: HList](implicit unifier : UnifierAux[L, M], tupler : Tupler[M]) : tupler.Out = unifier(l).tupled

  /**
   * Converts this `HList` to an ordinary List of elements typed as the least upper bound of the types of the elements
   * of this `HList`.
   */
  def toList[Lub](implicit toList : ToList[L, Lub]) : List[Lub] = toList(l)
}

/**
 * Conversions between `Tuples` and `HLists`.
 * 
 * The implicit defined by this object enhances `Tuples` (currently up to arity 4) with an `hlisted` method which
 * constructs an equivalently typed [[shapeless.HList]]. This object also provides higher ranked functions for
 * conversion between `Tuples` and `HLists`.
 * 
 * @author Miles Sabin
 */
object Tuples {
  
  implicit def tupleOps[T <: Product](t : T)(implicit hlister : HLister[T]) = new TupleOps[T, hlister.Out](hlister(t))
  
  /**
   * Higher ranked function which converts `Tuples` to `HLists`. 
   */
  object hlisted {
    def apply[T <: Product](t : T)(implicit hlister : HLister[T]) : hlister.Out = hlister(t)
  }
  implicit def hlisted1[T <: Product](implicit hlister : HLister[T]) =
    new Case[hlisted.type, T => hlister.Out](hlister.apply(_))

  /**
   * Monomorphic instantiator for [[shapeless.Tuples.hlisted]].
   */
  implicit def univInstHListed[F, G](h : hlisted.type)(implicit c : Case[hlisted.type, F => G]) : F => G = c.value
}

/**
 * Type class supporting conversion of `Tuples` to `HLists`.
 * 
 * @author Miles Sabin
 */
trait HLister[-T <: Product] {
  type Out <: HList
  def apply(t : T) : Out
}
  
trait HListerAux[-T <: Product, Out <: HList] {
  def apply(t : T) : Out
}

/**
 * `HLister` type class instances.
 * 
 * @author Miles Sabin
 */
object HLister {
  implicit def hlister[T <: Product, Out0 <: HList](implicit hlister : HListerAux[T, Out0]) = new HLister[T] {
    type Out = Out0
    def apply(t : T) : Out = hlister(t)
  }
}

object HListerAux extends HListerAuxInstances

/**
 * Conversions between ordinary functions and `HList` functions.
 * 
 * The implicits defined by this object enhance ordinary functions (resp. HList functions) with an `hlisted` (resp.
 * `unhlisted`) method which creates an equivalently typed `HList` function (resp. ordinary function).
 * 
 * @author Miles Sabin
 */
object Functions {
  trait FnHListOps[HLFn] {
    def hlisted : HLFn
  }
  
  implicit def fnHListOps[F](t : F)(implicit fnHLister : FnHLister[F]) = new FnHListOps[fnHLister.Out] {
    def hlisted = fnHLister(t)
  }

  trait FnUnHListOps[F] {
    def unhlisted : F
  }

  implicit def fnUnHListOps[F](t : F)(implicit fnUnHLister : FnUnHLister[F]) = new FnUnHListOps[fnUnHLister.Out] {
    def unhlisted = fnUnHLister(t)
  }
}

/**
 * Type class supporting conversion of arbitrary functions (currently up to arity 4) to functions of a single `HList`
 * argument. 
 * 
 * @author Miles Sabin
 */
trait FnHLister[F] {
  type Out
  def apply(f : F) : Out
}
  
trait FnHListerAux[F, Out] {
  def apply(f : F) : Out
}
  
/**
 * `FnHLister` type class instances.
 * 
 * @author Miles Sabin
 */
object FnHLister {
  implicit def fnHLister[F, Out0](implicit fnHLister : FnHListerAux[F, Out0]) = new FnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnHLister(f)
  }
}

object FnHListerAux extends FnHListerAuxInstances

/**
 * Type class supporting conversion of functions of a single `HList` argument to ordinary functions (currently up to
 * arity 4). 
 * 
 * @author Miles Sabin
 */
trait FnUnHLister[F] {
  type Out
  def apply(f : F) : Out
}
  
trait FnUnHListerAux[F, Out] {
  def apply(f : F) : Out
}
  
/**
 * `FnUnHLister` type class instances.
 * 
 * @author Miles Sabin
 */
object FnUnHLister {
  implicit def fnUnHLister[F, Out0](implicit fnUnHLister : FnUnHListerAux[F, Out0]) = new FnUnHLister[F] {
    type Out = Out0
    def apply(f : F) : Out = fnUnHLister(f)
  }
}

object FnUnHListerAux extends FnUnHListerAuxInstances

/**
 * Conversions between `Traversables` and `HLists`.
 * 
 * The implicit defined by this object enhances `Traversables` with a `toHList` method which constructs an equivalently
 * typed [[shapeless.HList]] if possible. 
 * 
 * @author Miles Sabin
 */
object Traversables {
  trait TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) : Option[L]
  }
  
  implicit def traversableOps[T](l : Traversable[T]) = new TraversableOps[T] {
    def toHList[L <: HList](implicit fl : FromTraversable[T, L]) = fl(l) 
  }
}

/**
 * Type class supporting type safe conversion of `Traversables` to `HLists`. 
 * 
 * @author Miles Sabin
 */
trait FromTraversable[T, Out <: HList] {
  def apply(l : Traversable[T]) : Option[Out]
}
  
/**
 * `FromTraversable` type class instances.
 * 
 * @author Miles Sabin
 */
object FromTraversable {
  import Typeable._

  implicit def hnilFromTraversable[T] = new FromTraversable[T, HNil] {
    def apply(l : Traversable[T]) = l match {
      case Nil => Some(HNil)
      case _ => None
    }
  }
  
  implicit def hlistFromTraversable[T, OutH, OutT <: HList]
    (implicit flt : FromTraversable[T, OutT], oc : Typeable[OutH]) = new FromTraversable[T, OutH :: OutT] {
      def apply(l : Traversable[T]) : Option[OutH :: OutT] =
        for(e <- l.headOption; h <- e.cast[OutH]; t <- flt(l.tail)) yield h :: t
  }
}