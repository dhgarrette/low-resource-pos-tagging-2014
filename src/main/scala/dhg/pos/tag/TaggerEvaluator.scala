package dhg.pos.tag

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.StringUtil._
import dhg.util.Time
import dhg.util.CommandLineUtil
import math.{ log, exp }
import scalaz._
import Scalaz._
import dhg.pos.prob._

object TaggerEvaluator {
  type Word = String
  def apply[Tag](model: Tagger[Tag], testData: Vector[Vector[(Word, Tag)]]): Double = {
    var correct = 0
    var total = 0
    val errors = collection.mutable.Map[(Tag, Tag), Int]() // (gold, model) -> count
    var totalTime = 0L
    Time.time("testing", {
      for ((goldTaggedSentence, i) <- testData.zipWithIndex) {
        val tokens = goldTaggedSentence.map(_._1)
        val startTime = System.currentTimeMillis()
        val modelTagged = model.tag(tokens)
        totalTime += (System.currentTimeMillis() - startTime)
        //if ((i + 1) % 100 == 0) println(f"${i + 1}%5s, total time: ${totalTime / 1000.0}%.3f sec, avg time: ${totalTime / 1000.0 / (i + 1)}%.4f sec")
        //println(tokens zipSafe modelTagged)
        //logger.debug((tokens zipSafe modelTagged).map { case (w, t) => f"$w|$t" }.mkString(" "))
        for (((gw, gt), mt) <- goldTaggedSentence zipSafe modelTagged) {
          if (gt == mt) {
            correct += 1
          }
          else {
            // Error breakdown
            errors((gt, mt)) = errors.getOrElse((gt, mt), 0) + 1
          }
          total += 1
        }
      }
    })
    val accuracy = correct * 100.0 / total
    println(f"Accuracy: ${accuracy}%.2f  ($correct/$total)")
    val errorsToShow = errors.toVector.sortBy(-_._2).take(10).map { case ((gt, mt), count) => (count.toString, gt.toString, mt.toString) }
    val maxCountWidth = errorsToShow.map(_._1.length).max max 5
    val maxTagWidth = errorsToShow.map { case (_, gt, mt) => gt.size }.max
    println(f" ${"count".padLeft(maxCountWidth)}  ${"gold".padRight(maxTagWidth)}  ${"model"}")
    for ((count, gt, mt) <- errorsToShow) {
      println(f" ${count.padLeft(maxCountWidth)}  ${gt.padRight(maxTagWidth)}  ${mt}")
    }
    println(f"avg tagging: ${totalTime / 1000.0 / testData.size}%.4f sec")
    accuracy
  }
}
