package dhg.util

import scala.annotation.tailrec
import scala.collection.GenSeqLike
import scala.collection.GenTraversable
import scala.collection.GenTraversableLike
import scala.collection.GenTraversableOnce
import scala.collection.Parallel
import scala.collection.Parallelizable
import scala.collection.SeqLike
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.Builder
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**
 * Enhancement methods for collections
 *
 * @author Dan Garrette (dhgarrette@gmail.com)
 */
object CollectionUtil {

  //////////////////////////////////////////////////////
  // toTuple2: (T,T)
  // toTuple3: (T,T,T)
  // toTuple4: (T,T,T,T)
  // toTuple5: (T,T,T,T,T)
  //   - Convert this sequence to a tuple
  //////////////////////////////////////////////////////

  final implicit class Enriched_toTuple_Seq[A](val seq: Seq[A]) extends AnyVal {
    def toTuple2 = seq match { case Seq(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple2: $x") }
    def toTuple3 = seq match { case Seq(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple3: $x") }
    def toTuple4 = seq match { case Seq(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple4: $x") }
    def toTuple5 = seq match { case Seq(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert sequence of length ${seq.size} into Tuple5: $x") }
  }

  final implicit class Enriched_toTuple_Array[A](val seq: Array[A]) extends AnyVal {
    def toTuple2 = seq match { case Array(a, b) => (a, b); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple2: Array(${x.mkString(", ")})") }
    def toTuple3 = seq match { case Array(a, b, c) => (a, b, c); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple3: Array(${x.mkString(", ")})") }
    def toTuple4 = seq match { case Array(a, b, c, d) => (a, b, c, d); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple4: Array(${x.mkString(", ")})") }
    def toTuple5 = seq match { case Array(a, b, c, d, e) => (a, b, c, d, e); case x => throw new AssertionError(s"Cannot convert array of length ${seq.size} into Tuple5: Array(${x.mkString(", ")})") }
  }

  //////////////////////////////////////////////////////
  // +:(elem: B): Iterator[B]
  //   - Prepend an element to the iterator
  // :+(elem: B): Iterator[B]
  //   - Append an element to the end of the iterator
  //////////////////////////////////////////////////////

  final implicit class Enriched_prependAppend_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Prepend an item to the front of the iterator
     *
     * @param elem	the item to be prepended
     * @return a new iterator
     */
    def +:[B >: A](elem: B): Iterator[B] =
      Iterator(elem) ++ self

    /**
     * Append an item to the end of the iterator
     *
     * @param elem	the item to be appended
     * @return a new iterator
     */
    def :+[B >: A](elem: B): Iterator[B] =
      self ++ Iterator(elem)
  }

  //////////////////////////////////////////////////////
  // counts(): Map[A, Int]
  //   - Map each distinct item in the collection to the number of times it appears.
  //////////////////////////////////////////////////////

  final implicit class Enriched_counts_TraversableOnce[A](val self: TraversableOnce[A]) extends AnyVal {
    /**
     * Map each distinct item in the collection to the number of times it appears.
     *
     * @return Map from items to their counts
     */
    def counts(): Map[A, Int] = {
      val m = mutable.Map.empty[A, Int]
      for (item <- self) {
        val count = m.getOrElse(item, 0)
        m(item) = count + 1
      }
      m.toMap
    }
  }

  //////////////////////////////////////////////////////
  // groupBy(f: A => K): Repr[(R,U)]
  //   - Make Traversable.groupBy functionality available to Iterator
  //////////////////////////////////////////////////////

  final implicit class Enriched_groupBy_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @return Map from new keys to original items
     */
    def groupBy[K](f: A => K): Map[K, Vector[A]] =
      this.groupBy(f, Vector.newBuilder[A])

    /**
     * Same functionality as Traversable.groupBy(f)
     *
     * @param f	function mapping items to new keys
     * @param builder	a builder to construct collections of items that have been grouped
     * @return Map from new keys to original items
     */
    def groupBy[K, That <: Iterable[A]](f: A => K, builder: => Builder[A, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[A, That]]
      for (elem <- self) {
        val key = f(elem)
        val bldr = m.getOrElseUpdate(key, builder)
        bldr += elem
      }
      val b = Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // groupByKey(): Map[T,Repr[U]]
  //   - For a collection of pairs (k,v), create a map from each `k` to the  
  //     collection of `v`s with which it is associated.
  //   - Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
  //////////////////////////////////////////////////////

  final implicit class Enriched_groupByKey_Traversable[K, V, Repr](val self: TraversableLike[(K, V), Repr]) extends AnyVal {
    /**
     * For a collection of pairs (k,v), create a map from each `k` to the
     * collection of `v`s with which it is associated.
     *
     * Equivalent to self.groupBy(_._1).map { case (k, elems) => (k, elems.map(_._2)) }
     *
     * @return Map from `k`s to collections of `v`s
     */
    def groupByKey[That](implicit bf: CanBuildFrom[Repr, V, That]): Map[K, That] = {
      val m = mutable.Map.empty[K, Builder[V, That]]
      for ((key, value) <- self) {
        val bldr = m.getOrElseUpdate(key, bf(self.asInstanceOf[Repr]))
        bldr += value
      }
      val b = immutable.Map.newBuilder[K, That]
      for ((k, v) <- m)
        b += ((k, v.result))
      b.result
    }
  }

  final implicit class Enriched_groupByKey_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U](implicit ev: A <:< (T, U)): Map[T, Vector[U]] =
      groupByKey(Vector.newBuilder[U])

    /**
     * For a collection of pairs, group by the first item in the pair.
     *
     * @param builder a builder to construct collections of items that have been grouped
     * @return Map from first items of the pairs to collections of items that have been grouped
     */
    def groupByKey[T, U, That <: Iterable[U]](builder: => Builder[U, That])(implicit ev: A <:< (T, U)): Map[T, That] =
      self.groupBy(_._1).mapVals(v => (builder ++= v.map(_._2)).result)
  }

  //////////////////////////////////////////////////////
  // ungroup(): Iterator[(A, B)]
  //   - For a map with collections for values, return an iterator of pairs
  //     where each key is paired with each item in its value collection
  //   - Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  //////////////////////////////////////////////////////

  final implicit class Enriched_ungroup_GenTraversableOnce[A, B](val self: GenTraversableOnce[(A, GenTraversableOnce[B])]) extends AnyVal {
    /**
     * For a map with collections for values, return an iterator of pairs
     * where each key is paired with each item in its value collection.
     *
     * Equivalent to self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
     *
     * @return an iterator of pairs
     */
    def ungroup() = self.toIterator.flatMap { case (a, bs) => bs.toIterator.map(a -> _) }
  }

  //////////////////////////////////////////////////////
  // dropRightWhile(p: A => Boolean): Repr
  //////////////////////////////////////////////////////

  final implicit class Enriched_dropRightWhile_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
    def dropRightWhile[That](p: A => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      val buffer = mutable.Buffer[A]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }

  final implicit class Enriched_dropRightWhile_String(val self: String) extends AnyVal {
    def dropRightWhile(p: Char => Boolean): String = {
      val b = StringCanBuildFrom()
      val buffer = mutable.Buffer[Char]()
      for (x <- self) {
        buffer += x
        if (!p(x)) {
          b ++= buffer
          buffer.clear()
        }
      }
      b.result
    }
  }

  //////////////////////////////////////////////////////
  // splitAt(n: Int) 
  //   - Split this collection at the specified index
  //   - Useful since Iterator.take doesn't guarantee the state of the original Iterator
  //   - Extend Traversable.splitAt to Iterator
  //////////////////////////////////////////////////////

  private[this] class Counter(startAt: Int = 0) { private[this] var i = startAt; def get = i; def inc() = { i += 1; this }; override def toString = f"Counter($i)" }
  private[this] class Enriched_splitAt_Iterator_FirstItr[A](self: Iterator[A], n: Int, c: Counter) extends Iterator[A] {
    def next(): A = {
      assert(hasNext, "first has already been read completely")
      c.inc(); self.next
    }
    def hasNext() = c.get < n && self.hasNext
  }
  private[this] class Enriched_splitAt_Iterator_SecondItr[A](self: Iterator[A], n: Int, c: Counter) extends Iterator[A] {
    def next(): A = {
      assert(c.get >= n, "first has NOT YET been read completely")
      assert(hasNext, "second has already been read completely")
      c.inc(); self.next
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_splitAt_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Safely split this iterator at the specified index.  The 'first'
     * iterator must be exhausted completely before the items in the 'second'
     * iterator can be accessed.
     *
     * Inspired by Traversable.splitAt
     *
     * @param n	The index at which to split the collection
     * @return	a pair: the items before the split point and the items
     *          starting with the split point
     */
    def splitAt(n: Int): (Iterator[A], Iterator[A]) = {
      val c = new Counter()
      val first: Iterator[A] = new Enriched_splitAt_Iterator_FirstItr(self, n, c)
      val second: Iterator[A] = new Enriched_splitAt_Iterator_SecondItr(self, n, c)
      (first, second)
    }
  }

  /**
   * The KeepDelimiter enumeration is used to specify behavior for the `split` methods.
   */
  sealed trait KeepDelimiter
  object KeepDelimiter {
    case object DropDelimiter extends KeepDelimiter
    case object KeepDelimiterAsFirst extends KeepDelimiter
    case object KeepDelimiterAsLast extends KeepDelimiter
  }

  //////////////////////////////////////////////////////
  // split(delim: A): Iterator[Repr[A]]
  //   - Split this collection on each occurrence of the delimiter 
  //   - Inspired by String.split
  //////////////////////////////////////////////////////

  final implicit class Enriched_split_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split(delim: A, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Iterator[Vector[A]] =
      split(delim, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That]): Iterator[That] =
      self.splitWhere(_ == delim, builder, KeepDelimiter.DropDelimiter)

    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      self.splitWhere(_ == delim, builder, keepDelimiter)
  }

  final implicit class Enriched_split_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this collection on each occurrence of the delimiter.
     *
     * Inspired by String.split
     *
     * @param delim	The delimiter upon which to split.
     */
    def split[That](delim: A, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.split(delim, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // splitWhere(p: A => Boolean): Iterator[Repr[A]]
  //   - Split this on items for which the predicate is true 
  //////////////////////////////////////////////////////

  private[this] class SplitWhereIterator[A, That](self: Iterator[A], p: A => Boolean, builder: => Builder[A, That], keepDelimiter: KeepDelimiter) extends Iterator[That] {
    var queued: Option[That] = None
    val bldr = new BuilderHolder(builder)

    def next(): That = {
      assert(this.hasNext, "next on empty iterator")
      val group = queued.get
      queued = None
      group
    }

    def hasNext() = {
      if (queued.isEmpty) {
        takeUntilDelim()
      }
      if (queued.isEmpty && bldr.nonEmpty) {
        queued = Some(bldr.result)
        bldr.clear()
      }
      queued.nonEmpty
    }

    @tailrec
    private def takeUntilDelim() {
      if (self.hasNext) {
        val x = self.next
        if (p(x)) {
          if (keepDelimiter == KeepDelimiter.KeepDelimiterAsLast) {
            bldr += x
          }
          queued = Some(bldr.result)
          bldr.clear()
          if (keepDelimiter == KeepDelimiter.KeepDelimiterAsFirst) {
            bldr += x
          }
        }
        else {
          bldr += x
          takeUntilDelim()
        }
      }
    }
  }
  final implicit class Enriched_splitWhere_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere(p: A => Boolean, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter): Iterator[Vector[A]] =
      splitWhere(p, Vector.newBuilder[A], keepDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That]): Iterator[That] =
      splitWhere(p, builder, KeepDelimiter.DropDelimiter)

    /**
     * Split this on items for which the predicate is true.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, builder: => Builder[A, That], keepDelimiter: KeepDelimiter): Iterator[That] =
      new SplitWhereIterator(self, p, builder, keepDelimiter)
  }

  class BuilderHolder[A, That](builder: => Builder[A, That]) {
    private val b = builder
    private var bEmpty = true

    def +=(a: A) = {
      b += a
      bEmpty = false
      this
    }

    def result() = {
      b.result
    }

    def clear() {
      b.clear()
      bEmpty = true
    }

    def isEmpty = bEmpty
    def nonEmpty = !isEmpty

    override def toString() = s"BuilderHolder(${b.result}, isEmpty=$isEmpty)"
  }

  final implicit class Enriched_splitWhere_Traversable[A, Repr](val self: TraversableLike[A, Repr]) extends AnyVal {
    /**
     * Split this on items for which the predicate is true.  Delimiters
     * do not appear in the output.
     *
     * @param delim	The delimiter upon which to split.
     */
    def splitWhere[That](p: A => Boolean, keepDelimiter: KeepDelimiter = KeepDelimiter.DropDelimiter)(implicit bf: CanBuildFrom[Repr, A, That]): Iterator[That] =
      self.toIterator.splitWhere(p, bf(self.asInstanceOf[Repr]), keepDelimiter)
  }

  //////////////////////////////////////////////////////
  // zipSafe(that: GenTraversable[B]): Repr[(A,B)]
  //   - zip this collection with another, throwing an exception if they are
  //     not of equal length.
  //////////////////////////////////////////////////////

  private[this] class ZipSafeIterator[A, B](self: Iterator[A], thatItr: Iterator[B]) extends Iterator[(A, B)] {
    def hasNext() = {
      val hn = self.hasNext
      assert(hn == thatItr.hasNext, s"Attempting to zipSafe collections of different lengths.  ${if (hn) "Second" else "First"} ran out.")
      hn
    }
    def next() = {
      hasNext()
      (self.next, thatItr.next)
    }
  }
  final implicit class Enriched_zipSafe_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[B](that: GenTraversableOnce[B]): Iterator[(A, B)] = {
      val thatItr = that.toIterator
      new ZipSafeIterator(self, thatItr)
    }
  }

  final implicit class Enriched_zipSafe_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, B, That](that: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self.asInstanceOf[Repr])
      b ++= (self.toIterator zipSafe that)
      b.result
    }
  }

  final implicit class Enriched_zipSafe_Tuple_of_Iterator[A, B](val self: (Iterator[A], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe = self._1 zipSafe self._2
  }

  final implicit class Enriched_zipSafe_Tuple_of_GenTraversable[A, Repr, B](val self: (GenTraversableLike[A, Repr], GenTraversableOnce[B])) extends AnyVal {
    /**
     * zip this collection with another, throwing an exception if they
     * are not of equal length.
     *
     * @param that	the collection with which to zip
     * @return an iterator of pairs
     * @throws RuntimeException	thrown if collections differ in length
     */
    def zipSafe[A1 >: A, That](implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
      val b = bf(self._1.asInstanceOf[Repr])
      b ++= (self._1.toIterator zipSafe self._2)
      b.result
    }
  }

  def zipSafe[A, Repr, B, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B])(implicit bf: CanBuildFrom[Repr, (A1, B), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext) bldr += ((ai.next, bi.next))
    assert(!ai.hasNext && !bi.hasNext, s"a=${if (ai.hasNext) "empty" else "nonempty"} b=${if (bi.hasNext) "empty" else "nonempty"}")
    bldr.result
  }

  def zipSafe[A, Repr, B, C, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B], c: GenTraversableOnce[C])(implicit bf: CanBuildFrom[Repr, (A1, B, C), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val ci = c.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext && ci.hasNext) bldr += ((ai.next, bi.next, ci.next))
    assert(!ai.hasNext && !bi.hasNext && !ci.hasNext, s"a=${if (ai.hasNext) "empty" else "nonempty"} b=${if (bi.hasNext) "empty" else "nonempty"} c=${if (ci.hasNext) "empty" else "nonempty"}")
    bldr.result
  }

  def zipSafe[A, Repr, B, C, D, A1 >: A, That](a: GenTraversableLike[A, Repr], b: GenTraversableOnce[B], c: GenTraversableOnce[C], d: GenTraversableOnce[D])(implicit bf: CanBuildFrom[Repr, (A1, B, C, D), That]): That = {
    val ai = a.toIterator
    val bi = b.toIterator
    val ci = c.toIterator
    val di = d.toIterator
    val bldr = bf(a.asInstanceOf[Repr])
    while (ai.hasNext && bi.hasNext && ci.hasNext && di.hasNext) bldr += ((ai.next, bi.next, ci.next, di.next))
    assert(!ai.hasNext && !bi.hasNext && !ci.hasNext && !di.hasNext, s"a=${if (ai.hasNext) "empty" else "nonempty"} b=${if (bi.hasNext) "empty" else "nonempty"} c=${if (ci.hasNext) "empty" else "nonempty"} d=${if (di.hasNext) "empty" else "nonempty"}")
    bldr.result
  }

  //////////////////////////////////////////////////////
  // unzip 
  //   - Unzip this iterator of pairs into two iterators.
  //   - The new iterators coordinate to maintain laziness.
  //////////////////////////////////////////////////////

  final implicit class Enriched_unzip3_GenTraversable[A, B, C, D, Repr <: GenTraversable[(A, B, C, D)]](val self: GenTraversableLike[(A, B, C, D), Repr]) extends AnyVal {
    def unzip4[ThatA, ThatB, ThatC, ThatD](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD] //
      ): (ThatA, ThatB, ThatC, ThatD) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      for ((a, b, c, d) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result())
    }
  }

  final implicit class Enriched_unzip5_GenTraversable[A, B, C, D, E, Repr <: GenTraversable[(A, B, C, D, E)]](val self: GenTraversableLike[(A, B, C, D, E), Repr]) extends AnyVal {
    def unzip5[ThatA, ThatB, ThatC, ThatD, ThatE](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD],
      bfE: CanBuildFrom[Repr, E, ThatE] //
      ): (ThatA, ThatB, ThatC, ThatD, ThatE) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      val bldrE = bfE(self.asInstanceOf[Repr])
      for ((a, b, c, d, e) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
        bldrE += e
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result(), bldrE.result())
    }
  }

  final implicit class Enriched_unzip6_GenTraversable[A, B, C, D, E, F, Repr <: GenTraversable[(A, B, C, D, E, F)]](val self: GenTraversableLike[(A, B, C, D, E, F), Repr]) extends AnyVal {
    def unzip6[ThatA, ThatB, ThatC, ThatD, ThatE, ThatF](implicit // 
    bfA: CanBuildFrom[Repr, A, ThatA],
      bfB: CanBuildFrom[Repr, B, ThatB],
      bfC: CanBuildFrom[Repr, C, ThatC],
      bfD: CanBuildFrom[Repr, D, ThatD],
      bfE: CanBuildFrom[Repr, E, ThatE],
      bfF: CanBuildFrom[Repr, F, ThatF] //
      ): (ThatA, ThatB, ThatC, ThatD, ThatE, ThatF) = {
      val bldrA = bfA(self.asInstanceOf[Repr])
      val bldrB = bfB(self.asInstanceOf[Repr])
      val bldrC = bfC(self.asInstanceOf[Repr])
      val bldrD = bfD(self.asInstanceOf[Repr])
      val bldrE = bfE(self.asInstanceOf[Repr])
      val bldrF = bfF(self.asInstanceOf[Repr])
      for ((a, b, c, d, e, f) <- self.seq) {
        bldrA += a
        bldrB += b
        bldrC += c
        bldrD += d
        bldrE += e
        bldrF += f
      }
      (bldrA.result(), bldrB.result(), bldrC.result(), bldrD.result(), bldrE.result(), bldrF.result())
    }
  }

  private[this] abstract class QueuedPairIterator[A, B, T, O](self: Iterator[(A, B)], thisQueue: mutable.Queue[T], otherQueue: mutable.Queue[O]) extends Iterator[T] {
    protected[this] def swapOrNot(p: (A, B)): (T, O)
    override def hasNext = thisQueue.nonEmpty || self.hasNext
    override def next =
      if (thisQueue.nonEmpty) thisQueue.dequeue()
      else { val (t, o) = swapOrNot(self.next()); otherQueue.enqueue(o); t }
  }
  private[this] class SwappingQueuedPairIterator[A, B](self: Iterator[(A, B)], aQueue: mutable.Queue[A], bQueue: mutable.Queue[B]) extends QueuedPairIterator[A, B, A, B](self, aQueue, bQueue) { override def swapOrNot(p: (A, B)) = p }
  private[this] class NonSwpngQueuedPairIterator[A, B](self: Iterator[(A, B)], aQueue: mutable.Queue[A], bQueue: mutable.Queue[B]) extends QueuedPairIterator[A, B, B, A](self, bQueue, aQueue) { override def swapOrNot(p: (A, B)) = p.swap }
  final implicit class Enriched_unzip2_Iterator[A, B](val self: Iterator[(A, B)]) extends AnyVal {
    def unzip(): (Iterator[A], Iterator[B]) = {
      val aQueue = mutable.Queue[A]()
      val bQueue = mutable.Queue[B]()
      val aItr = new SwappingQueuedPairIterator(self, aQueue, bQueue)
      val bItr = new NonSwpngQueuedPairIterator(self, aQueue, bQueue)
      (aItr, bItr)
    }
  }

  //////////////////////////////////////////////////////
  // mapTo[B](f: A => B): Repr[(A,B)]
  //   - Map a function over the collection, returning a set of pairs consisting 
  //     of the original item and the result of the function application
  //   - Functionally equivalent to:
  //         map(x => x -> f(x))
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapTo_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return the new collection
     */
    def mapTo[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(x => x -> f(x))
    }
  }

  private[this] class MapToIterator[A, B](self: Iterator[A], f: A => B) extends Iterator[(A, B)] {
    def hasNext = self.hasNext
    def next() = {
      val x = self.next
      x -> f(x)
    }
  }
  final implicit class Enriched_mapTo_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Map a function over the collection, returning a set of pairs consisting
     * of the original item and the result of the function application
     *
     * Functionally equivalent to: map(x => x -> f(x))
     *
     * @param f	the function to map
     * @return a new iterator
     */
    def mapTo[B](f: A => B): Iterator[(A, B)] = new MapToIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // mapToVal[B](v: B): Repr[(A,B)]
  //   - Map each item in the collection to a particular value
  //   - Functionally equivalent to:
  //         map(x => x -> v)
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapToVal_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return the new collection
     */
    def mapToVal[B, That](v: => B)(implicit bf: CanBuildFrom[Repr, (A, B), That]): That = {
      self.map(_ -> v)
    }
  }

  private[this] class MapToValIterator[A, B](self: Iterator[A], v: => B) extends Iterator[(A, B)] {
    def hasNext = self.hasNext
    def next() = self.next -> v
  }
  final implicit class Enriched_mapToVal_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Map each item in the collection to a particular value
     *
     * Functionally equivalent to: map(x => x -> v)
     *
     * @param v	the value to map to
     * @return a new iterator
     */
    def mapToVal[B](v: => B): Iterator[(A, B)] = new MapToValIterator(self, v)
  }

  //////////////////////////////////////////////////////
  // mapKeys(f: T => R): Repr[(R,U)]
  //   - In a collection of pairs, map a function over the first item of each pair.
  //   - Functionally equivalent to:
  //         this.map{case (k,v) => f(k) -> v}
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapKeys_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, (R, U), That]) = {
      self.map(x => f(x._1) -> x._2)
    }
  }

  private[this] class MapKeysIterator[T, U, R](self: Iterator[(T, U)], f: T => R) extends Iterator[(R, U)] {
    def hasNext = self.hasNext
    def next() = {
      val (k, v) = self.next()
      f(k) -> v
    }
  }
  final implicit class Enriched_mapKeys_Iterator[T, U](val self: Iterator[(T, U)]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the first item of each
     * pair.
     *
     * @param f	function to map over the first item of each pair
     * @return a collection of pairs
     */
    def mapKeys[R](f: T => R): Iterator[(R, U)] = new MapKeysIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // mapVals(f: U => R): Repr[(T,R)]
  //   - In a collection of pairs, map a function over the second item of each pair.
  //   - Ensures that the map is computed at call-time, and not returned as a view as `Map.mapValues` would do.
  //   - Equivalent to: this.map { case (k,v) => k -> f(v) }
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapVals_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.  Ensures that the map is computed at call-time, and not returned
     * as a view as 'Map.mapValues' would do.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R, That](f: U => R)(implicit bf: CanBuildFrom[Repr, (T, R), That]) = {
      self.map(x => x._1 -> f(x._2))
    }
  }

  private[this] class MapValsIterator[T, U, R](self: Iterator[(T, U)], f: U => R) extends Iterator[(T, R)] {
    def hasNext = self.hasNext
    def next() = {
      val (k, v) = self.next()
      k -> f(v)
    }
  }
  final implicit class Enriched_mapVals_Iterator[T, U](val self: Iterator[(T, U)]) extends AnyVal {
    /**
     * In a collection of pairs, map a function over the second item of each
     * pair.
     *
     * @param f	function to map over the second item of each pair
     * @return a collection of pairs
     */
    def mapVals[R](f: U => R): Iterator[(T, R)] = new MapValsIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // submap(f: T => R): Repr[R]
  //   - In a collection of collections, map a function over the inner collections without flattening.
  //   - Equivalent to: this.map(_.map(x => f(x)))
  //////////////////////////////////////////////////////

  final implicit class Enriched_submap_GenTraversable[T, TRepr, SRepr](val self: GenTraversableLike[GenTraversableLike[T, TRepr], SRepr]) extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f	function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, TThat, SThat](f: T => R)(implicit tbf: CanBuildFrom[TRepr, R, TThat], bf: CanBuildFrom[SRepr, TThat, SThat]) = {
      self.map(s => s.map(f)(tbf))
    }
  }

  private[this] class SubmapIterator[T, R, Repr, That](self: Iterator[GenTraversableLike[T, Repr]], f: T => R, bf: CanBuildFrom[Repr, R, That]) extends Iterator[That] {
    def hasNext = self.hasNext
    def next() = self.next().map(f)(bf)
  }
  final implicit class Enriched_submap_Iterator[T, Repr](val self: Iterator[GenTraversableLike[T, Repr]]) extends AnyVal {
    /**
     * In a collection of collections, map a function over the inner collections without flattening.
     *
     * @param f	function to map over the inner collections
     * @return a collection of collections
     */
    def submap[R, That](f: T => R)(implicit bf: CanBuildFrom[Repr, R, That]): Iterator[That] = new SubmapIterator(self, f, bf)
  }

  //////////////////////////////////////////////////////
  // mapt[A,B,R](f: (A,B) => R): Repr[R]
  //   - map over a Tuple2
  //   - same as `xs.map { case (x,y) => f(x,y) } `
  //////////////////////////////////////////////////////

  final implicit class Enriched_mapt_2_GenTraversableLike[A, B, Repr](val self: GenTraversableLike[(A, B), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2))
    }
  }

  private[this] class Mapt2Iterator[A, B, R](self: Iterator[(A, B)], f: (A, B) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_2_Iterator[A, B](val self: Iterator[(A, B)]) extends AnyVal {
    def mapt[R](f: (A, B) => R): Iterator[R] = new Mapt2Iterator(self, f)
  }

  final implicit class Enriched_mapt_3_GenTraversableLike[A, B, C, Repr](val self: GenTraversableLike[(A, B, C), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B, C) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2, x._3))
    }
  }

  private[this] class Mapt3Iterator[A, B, C, R](self: Iterator[(A, B, C)], f: (A, B, C) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2, x._3)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_3_Iterator[A, B, C](val self: Iterator[(A, B, C)]) extends AnyVal {
    def mapt[R](f: (A, B, C) => R): Iterator[R] = new Mapt3Iterator(self, f)
  }

  final implicit class Enriched_mapt_4_GenTraversableLike[A, B, C, D, Repr](val self: GenTraversableLike[(A, B, C, D), Repr]) extends AnyVal {
    def mapt[R, That](f: (A, B, C, D) => R)(implicit bf: CanBuildFrom[Repr, R, That]) = {
      self.map(x => f(x._1, x._2, x._3, x._4))
    }
  }

  private[this] class Mapt4Iterator[A, B, C, D, R](self: Iterator[(A, B, C, D)], f: (A, B, C, D) => R) extends Iterator[R] {
    def next() = {
      val x = self.next
      f(x._1, x._2, x._3, x._4)
    }
    def hasNext() = self.hasNext
  }
  final implicit class Enriched_mapt_4_Iterator[A, B, C, D](val self: Iterator[(A, B, C, D)]) extends AnyVal {
    def mapt[R](f: (A, B, C, D) => R): Iterator[R] = new Mapt4Iterator(self, f)
  }

  //////////////////////////////////////////////////////
  // foldLeftWhile[A,B](z: B)(p: (B, A) => Boolean)(op: (B, A) => B): B
  //   - Folds while the condition `p` is true.
  //   - If `p` operates on the item, then it behaves like `takeWhile`;
  //     if it operates on the accumulator, then it behaves like `while`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_foldLeftWhile_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    def foldLeftWhile[B](z: B)(p: (B, A) => Boolean)(op: (B, A) => B): B = {
      var result = z
      val it = self.toIterator
      while (it.hasNext) {
        val x = it.next
        if (!p(result, x)) return result
        result = op(result, x)
      }
      result
    }
  }

  //////////////////////////////////////////////////////
  // avg(): A
  //   - Find the average (mean) of this collection of numbers
  //////////////////////////////////////////////////////

  final implicit class Enrich_avg_GenTraversableOnce[A](val self: GenTraversableOnce[A])(implicit num: Fractional[A]) {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((num.zero, num.zero)) {
        case ((total, count), x) => (num.plus(total, x), num.plus(count, num.one))
      }
      num.div(total, count)
    }
  }

  final implicit class Enrich_avg_Int_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    /**
     * Find the average (mean) of this collection of numbers.
     *
     * @return the average (mean)
     */
    def avg = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val (total, count) = self.foldLeft((0, 0)) {
        case ((total, count), x) => (total + x, count + 1)
      }
      total.toDouble / count
    }
  }

  //////////////////////////////////////////////////////
  // proportion(p: A => Boolean): Double
  //   - Find the proportion of elements for which the predicate holds
  //////////////////////////////////////////////////////

  final implicit class Enrich_proportion_GenTraversableOnce[A](val self: GenTraversableOnce[A]) {
    /**
     * Find the proportion of elements for which the predicate holds
     *
     * @return the proportion
     */
    def proportion(p: A => Boolean) = {
      assert(self.nonEmpty, "cannot call `proportion` on an empty collection")
      val (total, count) = self.foldLeft((0, 0)) {
        case ((total, count), x) => (total + 1, if (p(x)) count + 1 else count)
      }
      count / total.toDouble
    }
  }

  //////////////////////////////////////////////////////
  // normalize(): Repr[A]
  //   - Normalize this collection of numbers by dividing each by the sum
  //////////////////////////////////////////////////////

  final implicit class Enriched_normalize_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit num: Fractional[A], bf: CanBuildFrom[Repr, A, That]) = {
      assert(self.nonEmpty, "cannot normalize an empty collection")
      val total = self.sum
      self.map(num.div(_, total))
    }
  }

  final implicit class Enriched_normalize_Int_GenTraversable[Repr](val self: GenTraversableLike[Int, Repr]) extends AnyVal {
    /**
     * Normalize this collection of numbers by dividing each by the sum
     *
     * @return normalized values
     */
    def normalize[That](implicit bf: CanBuildFrom[Repr, Double, That]) = {
      assert(self.nonEmpty, "cannot average an empty collection")
      val total = self.sum.toDouble
      self.map(_ / total)
    }
  }

  //////////////////////////////////////////////////////
  // normalizeValues(): Repr[(T,U)]
  //   - Normalize this values in this collection of pairs
  //////////////////////////////////////////////////////

  final implicit class Enriched_normalizeValues_GenTraversable[T, U, Repr](val self: GenTraversableLike[(T, U), Repr]) extends AnyVal {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit num: Fractional[U], bf: CanBuildFrom[Repr, (T, U), That]) = {
      val total = self.foldLeft(num.zero)((z, a) => num.plus(z, a._2))
      self.map(x => x._1 -> num.div(x._2, total))
    }
  }

  final implicit class Enriched_normalizeValues_Int_GenTraversable[T, Repr](val self: GenTraversableLike[(T, Int), Repr]) extends AnyVal {
    /**
     * Normalize this values in this collection of pairs
     *
     * @return a collection of pairs
     */
    def normalizeValues[That](implicit bf: CanBuildFrom[Repr, (T, Double), That]) = {
      val total = self.foldLeft(0)((z, a) => z + a._2).toDouble
      self.map(x => x._1 -> (x._2 / total))
    }
  }

  //////////////////////////////////////////////////////
  // maxByN(n: Int)(f: A => B): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).takeRight(n).reverse
  //////////////////////////////////////////////////////

  final implicit class Enriched_maxByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B, That](n: Int)(f: A => B)(implicit ord: Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
      val r = new Array[(A, B)](n min self.size)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.gteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (bf(self.asInstanceOf[Repr]) ++= r.map(_._1)).result
    }
  }

  final implicit class Enriched_maxByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N maximum entries in the collection.
     *
     * @return a collection containing the N maximum entries, sorted so the max is first
     */
    def maxByN[B](n: Int)(f: A => B)(implicit ord: Ordering[B]): Vector[A] = {
      val r = new Array[(A, B)](n)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.gteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (Vector.newBuilder ++= r.take(i min n).map(_._1)).result
    }
  }

  //////////////////////////////////////////////////////
  // minByN(n: Int)(f: A => B): Repr[A]
  //   - Equivalent to, but faster than self.sortBy(f).take(n)
  //////////////////////////////////////////////////////

  final implicit class Enriched_minByN_GenTraversable[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B, That](n: Int)(f: A => B)(implicit ord: Ordering[B], bf: CanBuildFrom[Repr, A, That]): That = {
      val r = new Array[(A, B)](n min self.size)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.lteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (bf(self.asInstanceOf[Repr]) ++= r.map(_._1)).result
    }
  }

  final implicit class Enriched_minByN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * Find the N minimum entries in the collection.
     *
     * @return a collection containing the N minimum entries, sorted so the min is first
     */
    def minByN[B](n: Int)(f: A => B)(implicit ord: Ordering[B]): Vector[A] = {
      val r = new Array[(A, B)](n)
      val it = self.toIterator
      var i = 0
      while (it.hasNext) {
        val x = it.next
        val b = f(x)
        var j = 0
        while (j < (i min n) && ord.lteq(r(j)._2, b)) { // while the thing at position j is greater than f(x)
          j += 1 // keep looking for the insertion position
        }
        if (j < n) { // if the insertion position is in the array
          for (j2 <- (i min (n - 1) until j by -1)) { // move each lower value down one array position
            r(j2) = r(j2 - 1)
          }
          r(j) = (x -> b) // insert x in the correct place
        }
        i += 1
      }
      (Vector.newBuilder ++= r.take(i min n).map(_._1)).result
    }
  }

  //////////////////////////////////////////////////////
  // distinctBy[A, B](f: A => B): Seq[A]
  //   - Remove duplicates according to some function `f`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_distinctBy_GenSeqLike[A, Repr](val self: GenSeqLike[A, Repr]) extends AnyVal {
    /**
     * Remove duplicates according to some function `f`.
     *
     * @param p	the function to determine the duplication key
     * @return the new collection
     */
    def distinctBy[B, That](f: A => B)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val builder = bf()
      val seen = mutable.HashSet[B]()
      for (a <- self) {
        val b = f(a)
        if (!seen(b)) {
          builder += a
          seen += b
        }
      }
      builder.result()
    }
  }

  //////////////////////////////////////////////////////
  // sumBy[B: Numeric](f: A => B): B
  //   - Map a numeric-producing function over each item and sum the results 
  //   - Functionally equivalent to:
  //         this.map(f).sum
  //////////////////////////////////////////////////////

  final implicit class Enriched_sumBy_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Map a numeric-producing function over each item and sum the results.
     *
     * Functionally equivalent to `this.map(f).sum`
     *
     * @param f	A function that produces a Numeric
     * @return the sum of the results after applications of f
     */
    def sumBy[B](f: A => B)(implicit num: Numeric[B]): B = {
      (num.zero /: self)((accum, x) => num.plus(accum, f(x)))
    }
  }

  //////////////////////////////////////////////////////
  // sliding2: Iterator[(A,A)]
  //   - slide over this collection to produce pairs.
  //   - Functionally equivalent to:
  //         this.sliding(2).map{Seq(a,b) => (a,b)}
  //////////////////////////////////////////////////////

  final implicit class Enriched_slidingN_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def sliding2(): Iterator[(A, A)] = self.sliding(2).map(_.toTuple2)
    def sliding3(): Iterator[(A, A, A)] = self.sliding(3).map(_.toTuple3)
    def sliding4(): Iterator[(A, A, A, A)] = self.sliding(4).map(_.toTuple4)
  }

  final implicit class Enriched_slidingN_GenTraversableLike[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def sliding2(): Iterator[(A, A)] = self.toIterator.sliding2()
    def sliding3(): Iterator[(A, A, A)] = self.toIterator.sliding3()
    def sliding4(): Iterator[(A, A, A, A)] = self.toIterator.sliding4()
  }

  //////////////////////////////////////////////////////
  // slyce
  //////////////////////////////////////////////////////

  final implicit class Enriched_slyce_GenTraversable[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def slyce[That](from: Int, until: Int)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val start = if (from >= 0) from else self.size + from
      val end = if (until >= 0) until else self.size + until
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(end - start)
      b ++= self.slice(start, end).toIterator
      b.result
    }

    def slyce[That](range: Range)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      val start = if (range.start >= 0) range.start else self.size + range.start
      val end = (if (range.end >= 0) range.end else self.size + range.end) + (if (range.isInclusive) 1 else 0)
      val b = bf(self.asInstanceOf[Repr])
      b.sizeHint(end - start)
      b ++= self.slice(start, end).toIterator
      b.result
    }
  }

  final implicit class Enriched_slyce_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def slyce(from: Int, until: Int): Iterator[A] = {
      val start = if (from >= 0) from else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      val end = if (until >= 0) until else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      self.slice(start, end)
    }

    def slyce(range: Range): Iterator[A] = {
      val start = if (range.start >= 0) range.start else throw new IllegalArgumentException("cannot slice Iterator with negative indices")
      val end = (if (range.end >= 0) range.end else throw new IllegalArgumentException("cannot slice Iterator with negative indices")) + (if (range.isInclusive) 1 else 0)
      self.slice(start, end)
    }
  }

  //////////////////////////////////////////////////////
  // countCompare(p: A => Boolean, count: Int): Int
  //   - Compares the number of items satisfying a predicate to a test value.
  //   - Functionally equivalent to (but more efficient than):
  //         this.count(p).compareTo(count)
  //////////////////////////////////////////////////////

  final implicit class Enriched_countCompare_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Compares the number of items satisfying a predicate to a test value.
     *
     *   @param p       the predicate used to test elements.
     *   @param count   the test value that gets compared with the count.
     *   @return Int value `x` where
     *   {{{
     *        x <  0       if this.count(p) <  count
     *        x == 0       if this.count(p) == count
     *        x >  0       if this.count(p) >  count
     *   }}}
     *  The method as implemented here does not call `length` directly; its running time
     *  is `O(length min count)` instead of `O(length)`.
     */
    def countCompare(p: A => Boolean, count: Int): Int = {
      val itr = self.toIterator
      var i = 0
      while (itr.hasNext && i <= count) {
        if (p(itr.next))
          i += 1
      }
      i - count
    }
  }

  //////////////////////////////////////////////////////
  // takeWhileAg
  //////////////////////////////////////////////////////

  final implicit class Enriched_takeWhileAg_Iterator[A](val self: Iterator[A]) {
    def takeWhileAg(p: Iterable[A] => Boolean): Iterator[A] = {
      if (self.isEmpty) {
        self
      }
      else {
        new Iterator[A] {
          private[this] var nextElement: A = self.next
          private[this] val z = ListBuffer[A](nextElement)
          private[this] var done = false

          override def hasNext = !done && p(z)

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next
                z += nextElement
              }
              else
                done = true
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  final implicit class Enriched_takeWhileAg_GenTraversableLike[A, Repr <: GenTraversable[A]](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def takeWhileAg[That](p: Iterable[A] => Boolean)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      (bf(self.asInstanceOf[Repr]) ++= self.toIterator.takeWhileAg(p)).result
    }
  }

  //////////////////////////////////////////////////////
  // takeSub[GenIterable[B]](n: Int): Repr[GenIterable[B]]
  //   - Take iterables from this collection until the total number of 
  //     elements in the taken items is about to exceed `n`.  The total number
  //     of elements will be less than or equal to `n`.
  //////////////////////////////////////////////////////

  final implicit class Enriched_takeSub_Iterator[A, R <: GenTraversable[A]](val self: Iterator[GenTraversableLike[A, R]]) {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub(n: Int): Iterator[R] = {
      require(n >= 0, "`n` cannot be negative")
      if (self.isEmpty) {
        self.asInstanceOf[Iterator[R]]
      }
      else {
        new Iterator[R] {
          private[this] var nextElement: R = self.next.asInstanceOf[R]
          private[this] var total: Int = nextElement.size

          override def hasNext = 0 <= total && total <= n

          override def next = {
            if (hasNext) {
              val x = nextElement
              if (self.hasNext) {
                nextElement = self.next.asInstanceOf[R]
                total += nextElement.size
              }
              else
                total = n + 1 // indicate to `hasNext` that there should be no more elements
              x
            }
            else
              throw new NoSuchElementException("next on empty iterator")
          }
        }
      }
    }
  }

  final implicit class Enriched_takeSub_GenTraversableLike[A, R <: GenTraversable[A], Repr <: GenTraversable[GenTraversable[A]]](val self: GenTraversableLike[GenTraversableLike[A, R], Repr]) extends AnyVal {
    /**
     * Take iterables from this collection until the total number of elements
     * in the taken items is about to exceed `n`.  The total number of
     * elements will be less than or equal to `n`.
     *
     * @param n	the maximum number of sub-elements to take
     * @return the new collection
     */
    def takeSub[That](n: Int)(implicit bf: CanBuildFrom[Repr, R, That]): That = {
      (bf(self.asInstanceOf[Repr]) ++= self.toIterator.takeSub(n)).result
    }
  }

  //////////////////////////////////////////////////////
  // asc/desc
  //////////////////////////////////////////////////////

  final implicit class Enriched_AscDesc_GenTraversableOnce[K, V](val self: GenTraversableOnce[(K, V)])(implicit ord: Ordering[V]) /*extends AnyVal */ {
    def asc: Vector[(K, V)] = self.toVector.sortBy(t => t._2)
    def desc: Vector[(K, V)] = self.toVector.sorted((ord on ((t: (K, V)) => t._2)).reverse)
  }

  //////////////////////////////////////////////////////
  // Iterator.last
  // Iterator.takeRight
  // Iterator.dropRight
  //////////////////////////////////////////////////////

  final implicit class Enriched_last_Iterator[A](val self: Iterator[A]) extends AnyVal {
    /**
     * @return The last item in the iterator.  Note that the iterator will be consumed after calling.
     */
    def last(): A = {
      if (!self.hasNext) throw new AssertionError("cannot call Iterator.last on an empty iterator")
      var a = self.next()
      while (self.hasNext) a = self.next()
      a
    }

    /**
     * @return The last n items of the iterator.  Note that the iterator will be consumed after calling.
     */
    def takeRight(n: Int): Vector[A] = self.sliding(n).last.toVector

    /**
     * @return The all but the last n items of the iterator.  Note that the iterator will be consumed after calling.
     */
    def dropRight(n: Int): Vector[A] = self.toVector.dropRight(n)
  }

  //////////////////////////////////////////////////////
  // GenTraversableOnce.only
  //////////////////////////////////////////////////////

  final implicit class Enriched_only_GenTraversableOnce[A](val self: GenTraversableOnce[A]) extends AnyVal {
    /**
     * Return the only element in the collection, or error if there is not exactly one element.
     *
     * @return the only element
     */
    def only(): A = {
      val itr = self.toIterator
      assert(itr.hasNext, "cannot call `only` on empty collection.")
      val a = itr.next
      assert(!itr.hasNext, f"cannot call `only` on collection with ${itr.size + 1} elements.")
      a
    }
  }

  //////////////////////////////////////////////////////
  // No-Op
  //////////////////////////////////////////////////////
  final implicit class Enriched_noop_GenTraversableLike[A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def noop[That](f: A => _)(implicit bf: CanBuildFrom[Repr, A, That]): That = {
      self.map { x => f(x); x }
    }
  }

  private[this] class NoOpIterator[A](self: Iterator[A], f: A => _) extends Iterator[A] {
    def hasNext = self.hasNext
    def next() = {
      val x = self.next
      f(x)
      x
    }
  }
  final implicit class Enriched_noop_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def noop(f: A => _): Iterator[A] = new NoOpIterator(self, f)
  }

  //////////////////////////////////////////////////////
  // shuffle
  //////////////////////////////////////////////////////

  final implicit class Enriched_shuffle_Seq[A, Repr](val self: SeqLike[A, Repr]) extends AnyVal {
    def shuffle[That](implicit bf: CanBuildFrom[Repr, A, That]): That =
      (bf(self.asInstanceOf[Repr]) ++= Random.shuffle(self)).result
  }

  final implicit class Enriched_shuffle_Iterator[A](val self: Iterator[A]) extends AnyVal {
    def shuffle: Iterator[A] = Random.shuffle(self)
  }

  //////////////////////////////////////////////////////
  // toBitSet
  //////////////////////////////////////////////////////

  final implicit class Enriched_toBitSet_GenTraversableOnce(val self: GenTraversableOnce[Int]) extends AnyVal {
    def toBitSet: BitSet = BitSet() ++ self
  }

  //////////////////////////////////////////////////////
  // mutable.Map.updateWith, mutable.Map.updateOrElseWith
  //////////////////////////////////////////////////////

  final implicit class Enriched_updateWith_MutableMap[K, V](val self: mutable.Map[K, V]) extends AnyVal {
    def updateWith(key: K)(f: (V => V)): mutable.Map[K, V] = {
      self(key) = f(self(key))
      self
    }

    def updateOrElseWith(key: K, default: V)(f: (V => V)): mutable.Map[K, V] = {
      self(key) = f(self.getOrElse(key, default))
      self
    }
  }

  //////////////////////////////////////////////////////
  // PARALLEL / SEQUENTIAL
  //   - obnoxious versions of the .par and .seq methods 
  //////////////////////////////////////////////////////

  final implicit class Enriched_PARALLEL_Parallelizable[+A, +ParRepr <: Parallel](val self: Parallelizable[A, ParRepr]) extends AnyVal {
    def PARALLEL = self.par
  }
  final implicit class Enriched_SEQUENTIAL_Iterator[+A](val self: Iterator[A]) extends AnyVal {
    def SEQUENTIAL = self.seq
  }
  final implicit class Enriched_SEQUENTIAL_GenTraversableLike[+A, Repr](val self: GenTraversableLike[A, Repr]) extends AnyVal {
    def SEQUENTIAL[That](implicit bf: CanBuildFrom[Repr, A, That]): That = (bf(self.asInstanceOf[Repr]) ++= self.seq).result
  }

}
