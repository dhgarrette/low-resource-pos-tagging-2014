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
import dhg.pos.tag.learn._
import dhg.util.math.LogDouble
import dhg.pos.tagdict.TagDictionary
import dhg.pos.tagdict.SimpleTagDictionary
import dhg.pos.tagdict.SimpleTagDictionaryFactory

class HmmTagger[Tag](
  val transitions: ConditionalLogProbabilityDistribution[Tag, Tag],
  val emissions: ConditionalLogProbabilityDistribution[Tag, String],
  val tagdict: TagDictionary[Tag])
  extends WeightedTagger[Tag]
  with Tagger[Tag] {

  /**
   * Compute the probability of the tagged sentence.  The result
   * should be represented as a logarithm.
   */
  override def sentenceProbWithWeights(sentence: Vector[(Word, Tag)], us: Vector[Map[Tag, LogDouble]]): LogDouble = {
    (((tagdict.startWord -> tagdict.startTag) +: sentence :+ (tagdict.endWord -> tagdict.endTag)) zipSafe (Map.empty[Tag, LogDouble] +: us :+ Map.empty[Tag, LogDouble]))
      .sliding2.foldLeft(LogDouble.one) {
        case (logProd, (((_, prevTag), _), ((currWord, currTag), u))) =>
          logProd * transitions(currTag, prevTag) * emissions(currWord, currTag) / u.getOrElse(currTag, LogDouble.zero)
      }
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    tagAndProbWithWeightsFromTagSet(sentence.mapTo(tagdict), us)
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeightsFromTagSet(sentence: Vector[(Word, Set[Tag])], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    val forwards =
      ((sentence :+ ((tagdict.endWord, Set(tagdict.endTag)))) zipSafe (us :+ Map.empty))
        .scanLeft(Map(tagdict.startTag -> (LogDouble.one, tagdict.startTag))) {
          case (prevV, ((currWord, potentialTags), u)) =>
            potentialTags.mapTo { k =>
              val scores =
                prevV.map {
                  case (kprime, (kprimeScore, _)) =>
                    val score = kprimeScore * transitions(k, kprime)
                    kprime -> score
                }
              val (bestKprime, bestKprimeScore) = scores.maxBy(_._2)
              (emissions(currWord, k) * bestKprimeScore / u.getOrElse(k, LogDouble.one), bestKprime)
            }.toMap
        }
    val tags =
      forwards.scanRight(tagdict.endTag) {
        (v, kNext) => v(kNext)._2
      }.drop(2).dropRight(1) // drop start/end tags
    val p = forwards.last(tagdict.endTag)._1
    (tags, p)
  }

}

trait SupervisedHmmTaggerTrainer[Tag] extends SupervisedTaggerTrainer[Tag] {
  def make(
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag],
    emissions: ConditionalLogProbabilityDistribution[Tag, Word],
    tagdict: TagDictionary[Tag]) = new HmmTagger(transitions, emissions, tagdict)
}

class UnsmoothedHmmTaggerTrainer[Tag]() extends SupervisedHmmTaggerTrainer[Tag]() {
  private[this] val dister = new SmoothedHmmTaggerTrainer[Tag](new UnsmoothedTransitionDistributioner(), new UnsmoothedEmissionDistributioner())
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class AddLambdaSmoothedHmmTaggerTrainer[Tag](lambda: Double) extends SupervisedHmmTaggerTrainer[Tag] {
  private[this] val dister = new SmoothedHmmTaggerTrainer[Tag](new AddLambdaTransitionDistributioner(lambda), new AddLambdaEmissionDistributioner(lambda))
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class SmoothedHmmTaggerTrainer[Tag](
  transitionDister: TransitionDistributioner[Tag], emissionDister: EmissionDistributioner[Tag])
  extends SupervisedHmmTaggerTrainer[Tag]() {

  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    val transitions = transitionDister(taggedSentences, tagdict)
    val emissions = emissionDister(taggedSentences, tagdict)

    new HmmTagger(transitions, emissions, tagdict)
  }
}

object Hmm {

  /** Returns a vector of tagged sentences */
  def taggedSentencesFile(filename: String) = {
    File(filename).readLines.zipWithIndex.map {
      case (line, lineNum) =>
        line.split("\\s+")
          .map(_.split("\\|"))
          .map {
            case Array(w, t) => (w, t)
            case x => sys.error(f"failed on line $lineNum")
          }.toVector
    }.toVector
  }

  def main(args: Array[String]): Unit = {
    val (arguments, options) = CommandLineUtil.parseArgs(args)

    val trainData = taggedSentencesFile(options("train"))

    val td = {
      val tdcutoffProvided = options.get("tdcutoff").isDefined
      val tdCutoff =
        options.get("tagdict").fold(tdcutoffProvided)(_.toBoolean).option {
          options.get("tdcutoff").fold(0.0)(_.toDouble)
        }
      new SimpleTagDictionaryFactory(tdCutoff)(trainData, "<S>", "<S>", "<E>", "<E>")
    }

    val trainer: SupervisedTaggerTrainer[String] = {
      val lambda = options.get("lambda").fold(1.0)(_.toDouble)
      if (options.contains("lambda") && !options.contains("tsmooth") && !options.contains("esmooth")) {
        new AddLambdaSmoothedHmmTaggerTrainer[String](lambda)
      }
      else if (options.contains("tsmooth") || options.contains("esmooth")) {
        val tsmooth: TransitionDistributioner[String] =
          options.getOrElse("tsmooth", "none") match {
            case "addlambda" => new AddLambdaTransitionDistributioner(lambda)
            case "onecount" => new OneCountTransitionDistributioner(lambda, lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedTransitionDistributioner()
          }
        val esmooth: EmissionDistributioner[String] =
          options.getOrElse("esmooth", "none") match {
            case "addlambda" => new AddLambdaEmissionDistributioner(lambda)
            case "onecount" => new OneCountEmissionDistributioner(lambda, lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedEmissionDistributioner()
          }
        new SmoothedHmmTaggerTrainer(tsmooth, esmooth)
      }
      else {
        new UnsmoothedHmmTaggerTrainer()
      }
    }

    val model = Time.time("training", trainer.train(trainData, td))
    TaggerEvaluator(model, taggedSentencesFile(options("test")))
  }

}
