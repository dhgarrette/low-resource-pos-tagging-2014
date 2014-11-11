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

    val trainer = new Naacl2013Trainer[String](
      labelPropIterations = options.i("labelPropIterations", 200),
      emIterations = options.i("emIterations", 50),
      memmIterations = options.i("memmIterations", 100),
      memmCutoff = options.i("memmCutoff", 100),
      tagToString = identity,
      tagFromString = identity,
      baseline = options.s("baseline", "false"))

    assert(options.contains("rawFile") == (options.contains("toksupFile") || options.contains("typesupFile")), "If `rawFile` is given, `toksupFile` or `typesupFile` (or both) must be given.")
    assert(options.contains("rawFile") || options.contains("modelFile"), "`modelFile` required if training data is not given.")

    val tagger: Tagger[String] =
      if (options.contains("rawFile")) {
        val rawSentences = new FileRawDataReader().readRaw(options("rawFile")).takeSub(options.i("numRawTokens", Int.MaxValue)).toVector
        val tokenSupSentences = options.get("toksupFile").map(f => new FileTaggedDataReader().readTagged(f).toVector).getOrElse(Vector.empty)
        val typeSupData = options.get("typesupFile").map(f => new FileTaggedDataReader().readTagged(f).toVector).getOrElse(Vector.empty)
        val initialTagdict = new SimpleTagDictionaryFactory(options.get("tdCutoff").map(_.toDouble))
          .apply(typeSupData.toVector, "<S>", "<S>", "<E>", "<E>")
        val model = trainer.train(rawSentences, tokenSupSentences, initialTagdict)
        options.get("modelFile").foreach { modelFile =>
          println(f"Writing tagger model to ${options("modelFile")}")
          MemmTagger.persistToFile(model, modelFile)
        }
        model
      }
      else {
        println(f"Loading tagger model from ${options("modelFile")}")
        MemmTagger.fromFile(options("modelFile"))
      }

    assert(options.contains("inputFile") == options.contains("outputFile"), "If `inputFile` is given, `outputFile` must be too.")
    for (inputFile <- options.get("inputFile")) {
      writeUsing(File(options("outputFile"))) { f =>
        for (sentence <- new FileRawDataReader().readRaw(inputFile)) {
          f.writeLine((for ((word, tag) <- sentence zipSafe tagger.tag(sentence)) yield f"$word|$tag").mkString(" "))
        }
      }
    }
    
    for(evalFile <- options.get("evalFile")){
      TaggerEvaluator.apply(tagger, new FileTaggedDataReader().readTagged(evalFile).toVector)
    }

  }

}
