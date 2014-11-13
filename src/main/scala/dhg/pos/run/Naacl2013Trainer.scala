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

class Naacl2013Trainer[Tag](
  autotagger: Naacl2013Autotagger[Tag],
  supTrainer: Naacl2013SupervisedTrainer[Tag]) {
  type Word = String

  /**
   * LP -> ModelMin -> EM -> MEMM
   */
  def train(
    rawSentences: Vector[Vector[Word]],
    labeledSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): MemmTagger[Tag] = {
    val startTime = System.currentTimeMillis()

    val (autotaggedRawCorpus, generalizedTagdict) = autotagger.induceRawCorpusTagging(rawSentences, labeledSentences, initialTagdict)

    println("learn an MEMM from auto-tagged data produced by the smoothed HMM")
    val memm = supTrainer.trainFromAutoTagged(autotaggedRawCorpus ++ labeledSentences, generalizedTagdict)

    println(f"Total NAACL Trainer training time: ${(System.currentTimeMillis() - startTime) / 1000} seconds\n\n")
    memm
  }

}

class Naacl2013SupervisedTrainer[Tag](
  memmIterations: Int = 100,
  memmCutoff: Int = 100,
  //
  tagToString: (Tag => String),
  tagFromString: (String => Tag)) {
  type Word = String

  /**
   * LP -> ModelMin -> EM -> MEMM
   */
  def trainFromAutoTagged(
    taggedCorpus: Vector[Vector[(Word, Tag)]],
    generalizedTagdict: TagDictionary[Tag]): MemmTagger[Tag] = {
    val startTime = System.currentTimeMillis()

    val supervisedMemmTrainer = new MemmTaggerTrainer(memmIterations, memmCutoff, tdRestricted = true, tagToString, tagFromString)
    supervisedMemmTrainer.train(taggedCorpus, generalizedTagdict)
  }

}

class Naacl2013Autotagger[Tag](
  labelPropIterations: Int = 200,
  emIterations: Int = 50,
  //
  tagToString: (Tag => String),
  tagFromString: (String => Tag),
  //
  baseline: String = "false") {
  type Word = String

  /**
   * LP -> ModelMin -> EM -> Tagged raw corpus
   */
  def induceRawCorpusTagging(
    rawSentences: Vector[Vector[Word]],
    labeledSentences: Vector[Vector[(Word, Tag)]],
    initialTagdict: TagDictionary[Tag]): (Vector[Vector[(Word, Tag)]], TagDictionary[Tag]) = {

    val annotationTagdict = SimpleTagDictionary(
      initialTagdict.entries |+| labeledSentences.flatten.groupByKey.mapVals(_.toSet),
      initialTagdict.startWord, initialTagdict.startTag, initialTagdict.endWord, initialTagdict.endTag,
      initialTagdict.allWords ++ rawSentences.flatten ++ labeledSentences.flatten.map(_._1), initialTagdict.allTags ++ labeledSentences.flatten.map(_._2).toSet,
      initialTagdict.excludedTags)

    val tsmooth = new AddLambdaTransitionDistributioner[Tag](0.1)
    val esmooth = new AddLambdaEmissionDistributioner[Tag](0.1)

    val emTrainer = new SoftEmHmmTaggerTrainer[Tag](emIterations, new UnsmoothedTransitionDistributioner, new UnsmoothedEmissionDistributioner, alphaT = 0.0, alphaE = 0.0, 1e-10)

    println(f"Raw tokens: ${rawSentences.flatten.size}  (${rawSentences.size} sentences)")
    println(f"Token-supervision tokens: ${labeledSentences.flatten.size}  (${labeledSentences.size} sentences)")
    println(f"Type-supervision TD-entries: ${annotationTagdict.entries.flatMap(_._2).size}  (${annotationTagdict.entries.size} word types)")
    println(f"tsmooth: ${tsmooth}")
    println(f"esmooth: ${esmooth}")
    println(f"emTrainer: ${emTrainer}")

    val (transitions, emissions, generalizedTagdict) =
      if (baseline == "false") {
        val lpSoftTagger = {
          val lpTaggingGraphBuilder =
            new JuntoAdaptingLpTaggingGraphBuilder[Tag](
              new SimpleLpTaggingGraphBuilder[Tag](
                new Type2TokenLpEdgeExtractor(),
                Vector(
                  TokenPrevLpEdgeExtractor(annotationTagdict.startWord),
                  TokenNextLpEdgeExtractor(annotationTagdict.endWord),
                  WordPrefixLpEdgeExtractor(3),
                  WordSuffixLpEdgeExtractor(3))))
          new JuntoTagger[Tag](lpTaggingGraphBuilder, labelPropIterations, threshold = 0.1, tagToString, tagFromString)
        }

        val modelMinTagger = new ModelMinTagger[Tag]()

        println("Induce a soft tagging of the raw data")
        val softTaggedRawSentences = lpSoftTagger.tagFromAnnotations(rawSentences, labeledSentences, annotationTagdict)
        println("Extract a generalized tag dictionary")
        val generalizedTagdict = SimpleTagDictionary(annotationTagdict.entries |+| softTaggedRawSentences.flatten.mapVals(_.keySet).toMap, annotationTagdict.startWord, annotationTagdict.startTag, annotationTagdict.endWord, annotationTagdict.endTag, annotationTagdict.allWords, annotationTagdict.allTags, annotationTagdict.excludedTags)
        println("Induce a hard tagging via model minimization on the soft LP output")
        val modelminTaggedRawSentences = modelMinTagger.tagFromSoftTaggedSentences(softTaggedRawSentences, labeledSentences, generalizedTagdict)
        println("learn a smoothed HMM from EM")
        val transitions = tsmooth(modelminTaggedRawSentences, generalizedTagdict)
        val emissions = esmooth(modelminTaggedRawSentences, generalizedTagdict)
        (transitions, emissions, generalizedTagdict)
      }
      else { // Skip the LP/ModelMin stuff
        val trInitializer =
          if (baseline.contains("tr")) new TrTagDictEntriesPossibilities[Tag](new AddLambdaTransitionDistributioner(0.1))
          else new TrUniform[Tag]()
        val emInitializer =
          if (baseline.contains("em")) new EmTagDictionaryEstimate[Tag](new TagDictionaryEstimateTagPriorInitializer(0.1), 0.1, 0.1)
          else new EmUniform[Tag]()
        println(f"trInitializer: ${trInitializer}")
        println(f"emInitializer: ${emInitializer}")
        val generalizedTagdict = annotationTagdict
        println("Initialize transitions")
        val transitions = trInitializer.fromRaw(rawSentences, generalizedTagdict)
        println("Initialize emissions")
        val emissions = emInitializer.fromRaw(rawSentences, generalizedTagdict)
        (transitions, emissions, generalizedTagdict)
      }

    println("learn an HMM initialized with the estimated transition and emission distributions")
    val emHmm = emTrainer.trainWithSomeGold(rawSentences, labeledSentences, generalizedTagdict, transitions, emissions)

    val autotaggedRawCorpus = rawSentences.map(s => s zipSafe emHmm.tag(s))
    (autotaggedRawCorpus, generalizedTagdict)
  }

}
