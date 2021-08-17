package objsets

import TweetReader.*

import java.util.NoSuchElementException

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int):
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet extends TweetSetInterface:

  def isEmpty: Boolean
  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, Empty())

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet =
    if that.isEmpty then this
    else if (this.isEmpty) then that
    else that.filterAcc(p => true, this)

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

class Empty extends TweetSet:

  override def isEmpty: Boolean = true

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    println("Empty: acc-" + acc.foreach(t => println))
    acc
  }

  def mostRetweeted: Tweet = throw NoSuchElementException()

  override def descendingByRetweet: TweetList = Nil


  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = NonEmpty(tweet, Empty(), Empty())

  def remove(tweet: Tweet): TweetSet = throw NoSuchElementException()

  def foreach(f: Tweet => Unit): Unit = ()

//  def union(that: TweetSet): TweetSet = that

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet:

  override def isEmpty: Boolean = false

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    println("NonEmpty: elem-" + elem + ", acc-" + acc.foreach(t => println))
    if (p(elem)) left.filterAcc(p, right.filterAcc(p, acc)).incl(elem)
    else left.filterAcc(p, right.filterAcc(p, acc))
  }

  def mostRetweeted: Tweet = {
    def getMax(t1: Tweet, t2: Tweet): Tweet = if t1.retweets > t2.retweets then t1 else t2
    val maxLeft = if left.isEmpty then elem else getMax(elem, left.mostRetweeted)
    val maxRight = if right.isEmpty then elem else  getMax(elem, right.mostRetweeted)
    getMax(maxLeft, maxRight)
  }

  override def descendingByRetweet: TweetList = {
    def go(tweetSet: TweetSet, tweetList: TweetList): TweetList = {
      if (tweetSet.isEmpty) then tweetList
      else
        val mostR = tweetSet.mostRetweeted
        println("Contains? " + tweetSet.contains(mostR))
        go(tweetSet.remove(mostR), Cons(mostR, tweetList))
      }
    def reverse(init: TweetList, result: TweetList): TweetList = {
      if (init.isEmpty) result
      else  reverse(init.tail, Cons(init.head, result))
    }
    reverse(go(this, Nil), Nil)
  }

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if x.text < elem.text then
      left.contains(x)
    else if elem.text < x.text then
      right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if x.text < elem.text then
      NonEmpty(elem, left.incl(x), right)
    else if elem.text < x.text then
      NonEmpty(elem, left, right.incl(x))
    else
      this

  def remove(tw: Tweet): TweetSet =
    if tw.text < elem.text then
      NonEmpty(elem, left.remove(tw), right)
    else if elem.text < tw.text then
      NonEmpty(elem, left, right.remove(tw))
    else
      left.union(right)

  def foreach(f: Tweet => Unit): Unit =
    f(elem)
    left.foreach(f)
    right.foreach(f)


trait TweetList:
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if !isEmpty then
      f(head)
      tail.foreach(f)

object Nil extends TweetList:
  def head = throw java.util.NoSuchElementException("head of EmptyList")
  def tail = throw java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true

class Cons(val head: Tweet, val tail: TweetList) extends TweetList:
  def isEmpty = false


object GoogleVsApple:
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = allTweets.filter(t => google.map(w => t.text.contains(w)).contains(true))
  lazy val appleTweets: TweetSet = allTweets.filter(t => apple.map(w => t.text.contains(w)).contains(true))

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = (googleTweets.union(appleTweets)).descendingByRetweet

object Main extends App:
  // Print the trending tweets
  GoogleVsApple.trending foreach println
