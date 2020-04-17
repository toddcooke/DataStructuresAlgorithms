import java.util

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {

  val min = 0
  val max = 1_000_000
  val array = (min until max).toArray.sorted
  val map = scala.collection.mutable.HashMap[Int, Int]()
  val tuples = scala.collection.mutable.ListBuffer[(Int, Int)]()
  (min until max).map(i => map.put(i, i))
  (min until max).map(i => tuples.addOne((i, i)))


  def binarySearch(key: Int): Unit = {
    util.Arrays.binarySearch(array, key)
  }

  def linearSearch(key: Int): Unit = {
    var i = min
    while (i < max) {
      if (array(i) == key) i = max
      else i += 1
    }
  }

  def hashMapGet(key: Int): Unit = {
    map.get(key)
  }

  def tuplesGet(key: Int): Unit = {
    tuples.find(i => i._1 == key)
  }

  def runner(op: Int => Unit, opName: String): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val f = Future.sequence {
      (0 until 100).map(_ => Future {
        val randomInt = Math.abs(scala.util.Random.nextInt()) % max
        time(op(randomInt))
      })
    }.map(times => {
      println(s"$opName median is ${median(times)} seconds")
    })

    Await.result(f, Duration.Inf)
  }

  // http://biercoff.com/easily-measuring-code-execution-time-in-scala/
  def time[R](block: => R): Double = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val elapsed = (t1 - t0).toDouble / 1_000_000_000
    elapsed
  }

  def median[T](seq: Seq[T]): T = seq(seq.length / 2)

  runner(binarySearch, "binary search")
  runner(linearSearch, "linear search")

  runner(hashMapGet, "hashmap get")
  runner(tuplesGet, "tuples get")
}
