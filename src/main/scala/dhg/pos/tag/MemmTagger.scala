package dhg.pos.tag

import java.io.BufferedOutputStream
import java.io.FileInputStream
import java.io.FileOutputStream
import dhg.util.Arm.ManagedCloseable
import dhg.util.Arm.using
import dhg.util.CollectionUtil._
import opennlp.tools.dictionary.Dictionary
import opennlp.tools.postag.POSDictionary
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSSample
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.util.ObjectStreamUtils
import opennlp.tools.util.TrainingParameters
import opennlp.tools.util.model.ModelType
import opennlp.tools.postag.OpenNlpPosDictionary._
import dhg.util.math.LogDouble
import dhg.pos.tagdict.TagDictionary
import dhg.pos.tagdict.SimpleTagDictionary
import opennlp.model.NoEventsException

/**
 * Train a MEMM from gold-labeled data.
 *
 * @param maxIterations
 * @param cutoff			"events" must occur at least this many times to be used during training
 */
class MemmTaggerTrainer[Tag](
  maxIterations: Int = 50,
  cutoff: Int = 100,
  tdRestricted: Boolean = false,
  tagToString: (Tag => String), tagFromString: (String => Tag))
  extends SupervisedTaggerTrainer[Tag] {

  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]) = {
    val (taggedSentencesWords, taggedSentencesTags) = taggedSentences.flatten.toSet.unzip
    val tagdict = initialTagdict.withWords(taggedSentencesWords).withTags(taggedSentencesTags)
    val model: POSModel = trainWithCutoff(taggedSentences, cutoff, taggedSentencesWords, taggedSentencesTags, tagdict)
    new MemmTagger[Tag](model, new POSTaggerME(model), tagToString, tagFromString)
  }

  def trainWithCutoff(taggedSentences: Vector[Vector[(Word, Tag)]], usedCutoff: Int, taggedSentencesWords: Set[Word], taggedSentencesTags: Set[Tag], tagdict: TagDictionary[Tag]): POSModel = {
    val samples = ObjectStreamUtils.createObjectStream(
      taggedSentences.map { s =>
        val (words, tags) = s.unzip
        new POSSample(words.toArray, tags.map(tagToString).toArray)
      }: _*)

    val languageCode = "Uh... any language ???"

    try {
      val params = new TrainingParameters()
      params.put(TrainingParameters.ALGORITHM_PARAM, ModelType.MAXENT.toString)
      params.put(TrainingParameters.ITERATIONS_PARAM, maxIterations.toString)
      params.put(TrainingParameters.CUTOFF_PARAM, usedCutoff.toString)

      val tagDictionary =
        if (tdRestricted) {
          taggedSentencesWords.foldLeft(new POSDictionary) { (td, w) =>
            td.updated(w, (tagdict(w) & taggedSentencesTags).map(tagToString))
          }
        }
        else null

      val ngramDictionary: Dictionary = null

      POSTaggerME.train(
        languageCode,
        samples,
        params,
        tagDictionary,
        ngramDictionary)
    }
    catch {
      case e: NoEventsException =>
        Console.err.println(e.getMessage)
        if (usedCutoff > 0)
          trainWithCutoff(taggedSentences, usedCutoff - 1, taggedSentencesWords, taggedSentencesTags, tagdict)
        else
          throw e
    }
  }

}

object MemmTaggerTrainer {
  def apply[Tag](
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false,
    tagToString: (Tag => String), tagFromString: (String => Tag)) =
    new MemmTaggerTrainer(maxIterations, cutoff, tdRestricted, tagToString, tagFromString)

  def apply2[Tag](
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false,
    tagToString: (Tag => String), tagFromString: (String => Tag)) =
    new MemmTaggerTrainer[Tag](maxIterations, cutoff, tdRestricted, tagToString, tagFromString)

  def apply1(
    maxIterations: Int = 50,
    cutoff: Int = 100,
    tdRestricted: Boolean = false) =
    new MemmTaggerTrainer[String](maxIterations, cutoff, tdRestricted, identity, identity)
}

class TypeShiftingTagDictionary[T1, Tag](
  delegate: TagDictionary[T1],
  tagToString: (T1 => Tag), tagFromString: (Tag => T1))
  extends TagDictionary[Tag] {

  def allWords: Set[Word] = delegate.allWords
  def allTags: Set[Tag] = delegate.allTags.map(tagToString)

  def startWord: Word = delegate.startWord
  def startTag: Tag = tagToString(delegate.startTag)
  def endWord: Word = delegate.endWord
  def endTag: Tag = tagToString(delegate.endTag)

  def excludedTags: Set[Tag] = delegate.excludedTags.map(tagToString)

  def apply(w: Word): Set[Tag] = delegate(w).map(tagToString)

  def reversed: Map[Tag, Set[Word]] = ???

  def entries: Map[Word, Set[Tag]] = delegate.entries.mapt((w, ts) => w -> ts.map(tagToString))
  def knownWordsForTag: Map[Tag, Set[Word]] = delegate.knownWordsForTag.mapt((t, ws) => tagToString(t) -> ws)

  def withWords(words: Set[Word]): TagDictionary[Tag] = ???
  def withTags(tags: Set[Tag]): TagDictionary[Tag] = ???
  def withExcludedTags(tags: Set[Tag]): TagDictionary[Tag] = ???
}

//
//
//

class MemmTagger[Tag](
  val model: POSModel, val meTagger: POSTaggerME,
  tagToString: (Tag => String), tagFromString: (String => Tag)) extends Tagger[Tag] {

  def sentenceProb(sentence: Vector[(Word, Tag)]): LogDouble = ???

  def tagAndProb(sentence: Vector[Word]): (Vector[Tag], LogDouble) = {
    (meTagger.tag(sentence.toArray).toVector.map(tagFromString), new LogDouble(Double.PositiveInfinity))
  }
}

object MemmTagger {

  def persistToFile(tagger: MemmTagger[String], filepath: String) {
    val model: POSModel = tagger.model
    using(new BufferedOutputStream(new FileOutputStream(filepath))) { modelOut =>
      model.serialize(modelOut)
    }
  }

  def fromFile(filepath: String): MemmTagger[String] = {
    val model =
      using(new FileInputStream(filepath)) { modelIn =>
        new POSModel(modelIn)
      }
    new MemmTagger(model, new POSTaggerME(model), identity, identity)
  }

}
