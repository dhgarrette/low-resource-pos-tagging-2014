package dhg.pos.run

import dhg.util.CollectionUtil._
import dhg.util.Time._
import dhg.util.FileUtil._
import dhg.util.Pattern._
import dhg.util.StringUtil._
import math.{ log, exp, abs }
import scala.collection.immutable.BitSet
import scala.collection.breakOut
import scala.util.Random
import annotation.tailrec
import dhg.util.CommandLineUtil
import scala.util.Try
import scalaz._
import Scalaz._
import dhg.pos.tag.learn._
import dhg.pos.prob._
import dhg.pos.tag.learn._
import dhg.pos.tag._
import dhg.pos.tag.learn._
import dhg.pos.tagdict.SimpleTagDictionaryFactory
import dhg.pos.gen._
import dhg.pos.tagdict.TagDictionary
import dhg.pos.tagdict.SimpleTagDictionary
import dhg.pos.data._

object Run {

  def main(args: Array[String]): Unit = {
    val (arguments, options_) = CommandLineUtil.parseArgs(args)
    val options = new CommandLineUtil.CommandLineOptions(options_)

    val autotagger = new Naacl2013Autotagger[String](
      labelPropIterations = options.i("labelPropIterations", 200),
      emIterations = options.i("emIterations", 50),
      tagToString = identity,
      tagFromString = identity,
      baseline = options.s("baseline", "false"))

    val autotaggedTrainer = new Naacl2013SupervisedTrainer[String](
      memmIterations = options.i("memmIterations", 100),
      memmCutoff = options.i("memmCutoff", 100),
      tagToString = identity,
      tagFromString = identity)

    val trainer = new Naacl2013Trainer[String](
      autotagger,
      autotaggedTrainer)

    val isTrain = options.contains("rawFile") || options.contains("autotaggedFile")
    assert(options.contains("rawFile") == (options.contains("toksupFile") || options.contains("typesupFile")), "If `rawFile` is given, `toksupFile` or `typesupFile` (or both) must be given.")
    assert((options.contains("rawFile") && options.contains("autotaggedFile")) || options.contains("modelFile") || options.contains("inputFile") || options.contains("evalFile"), "Need to either train (`modelFile`) or tag (`inputFile` or `evalFile`)")

    val tagger: Tagger[String] = {
      if (isTrain) {
        println("Training mode")

        val (autotaggedCorpus, generalizedTagdict) =
          if (options.contains("rawFile")) {
            val rawSentences = new FileRawDataReader().readRaw(options("rawFile")).takeSub(options.i("numRawTokens", Int.MaxValue)).toVector
            val tokenSupSentences = options.get("toksupFile").map(f => new FileTaggedDataReader().readTagged(f).toVector).getOrElse(Vector.empty)
            val typeSupData = options.get("typesupFile").map(f => new FileTaggedDataReader().readTagged(f).toVector).getOrElse(Vector.empty)
            val initialTagdict = new SimpleTagDictionaryFactory(options.get("tdCutoff").map(_.toDouble))
              .apply(typeSupData.toVector, "<S>", "<S>", "<E>", "<E>")
            val (autotaggedCorpus, generalizedTagdict) = autotagger.induceRawCorpusTagging(rawSentences, tokenSupSentences, initialTagdict)

            if (options.contains("autotaggedFile")) {
              println("Writing auto-tagged data to " + options("autotaggedFile"))
              writeUsing(File(options("autotaggedFile"))) { f =>
                for (sentence <- autotaggedCorpus) {
                  f.writeLine(sentence.map { case (w, t) => f"$w|$t" }.mkString(" "))
                }
              }
            }

            (autotaggedCorpus, generalizedTagdict)
          }
          else if (options.contains("autotaggedFile")) {
            println("Reading auto-tagged data from " + options("autotaggedFile"))
            val autotaggedCorpus = new FileTaggedDataReader().readTagged(options("autotaggedFile")).toVector
            val generalizedTagdict = new SimpleTagDictionaryFactory(options.get("tdCutoff").map(_.toDouble))
              .apply(autotaggedCorpus.toVector, "<S>", "<S>", "<E>", "<E>")
            (autotaggedCorpus, generalizedTagdict)
          }
          else {
            ??? // can't happen
          }

        val model = autotaggedTrainer.trainFromAutoTagged(autotaggedCorpus, generalizedTagdict)
        options.get("modelFile").foreach { modelFile =>
          println(f"Writing tagger model to ${options("modelFile")}")
          MemmTagger.persistToFile(model, modelFile)
        }
        model
      }
      else {
        assert(options.contains("modelFile"), "`modelFile` required if training data (`rawFile` or `autotaggedFile`) is not given.")
        println(f"Loading tagger model from ${options("modelFile")}")
        MemmTagger.fromFile(options("modelFile"))
      }
    }

    assert(options.contains("inputFile") == options.contains("outputFile"), "If `inputFile` is given, `outputFile` must be too.")
    for (inputFile <- options.get("inputFile")) {
      println(f"Tagging data in ${options("inputFile")} and writing to ${options("outputFile")}")
      writeUsing(File(options("outputFile"))) { f =>
        for (sentence <- new FileRawDataReader().readRaw(inputFile)) {
          f.writeLine((for ((word, tag) <- sentence zipSafe tagger.tag(sentence)) yield f"$word|$tag").mkString(" "))
        }
      }
    }

    for (evalFile <- options.get("evalFile")) {
      println(f"Evaluating on ${options("evalFile")}")
      TaggerEvaluator.apply(tagger, new FileTaggedDataReader().readTagged(evalFile).toVector)
    }

  }

}
