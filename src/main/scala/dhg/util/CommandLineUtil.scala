package dhg.util

import dhg.util.CollectionUtil._
import dhg.util.Pattern._

object CommandLineUtil {
  private[this] val OptionRegex = "--(.*)".r

  def parseArgs(args: Array[String]) = {
    val parsedArgs =
      ("" +: args.toVector) // prepend an empty arg so sliding will work
        .sliding(2).flatMap {
          case Seq(OptionRegex(option), argument) => Some(option, argument) // if the first thing is an option
          case Seq(_, OptionRegex(_)) => None // if the second thing is an option
          case Seq(_, argument) => Some("argument", argument) // if no options are involved, then it's a normal argument
          case Seq(_) => None
        }

    val (argumentList, optionList) =
      parsedArgs.partition { // separate normal arguments from options
        case ("argument", argument) => true
        case _ => false
      }

    val arguments = argumentList.map(_._2).toVector // arguments are a Vector
    val options = optionList.groupByKey.map { case (opt, Coll(v)) => opt -> v; case (opt, _) => sys.error(f"option --${opt} given twice") } // options are a Map
    (arguments, options)
  }

  case class CommandLineOptions(options: Map[String, String]) {
    private[this] val retrieved = collection.mutable.Set.empty[String]

    def get(key: String) = { retrieved += key; options.get(key) }
    def apply(key: String) = get(key).getOrElse(throw new NoSuchElementException(f"--$key not specified : ${options}"))
    def s(key: String) = apply(key)
    def s(key: String, default: String) = get(key).getOrElse(default)
    def i(key: String) = apply(key).toInt
    def i(key: String, default: Int) = get(key).fold(default)(_.toInt)
    def l(key: String) = apply(key).toLong
    def l(key: String, default: Long) = get(key).fold(default)(_.toLong)
    def d(key: String) = apply(key).toDouble
    def d(key: String, default: Double) = get(key).fold(default)(_.toDouble)
    def b(key: String) = apply(key).toBoolean
    def b(key: String, default: Boolean) = get(key).fold(default)(_.toBoolean)
    def contains(key: String) = options.get(key).isDefined

    def unusedOptions = options.keySet -- retrieved

    def toVector = options.toVector
    def toMap = options
  }

}
