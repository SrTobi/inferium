package inferium

import inferium.Config.{ConfigEntry, ConfigKey, SpecificConfigEntry}

import scala.collection.generic.Growable
import scala.collection.mutable
import inferium.utils.Utils._

import scala.util.Try

class Config(defs: ConfigEntry*) extends Growable[ConfigEntry] {
    private val entries = mutable.Map.empty[String, ConfigEntry]

    set(defs: _*)

    def set(defs: ConfigEntry*): this.type = {
        defs foreach { this += _ }
        this
    }

    def apply[T](key: ConfigKey[T]): T = get(key)
    def get[T](key: ConfigKey[T]): T = {
        entries.get(key.fullName) map {
            case SpecificConfigEntry(k, value) =>
                assert(k == key)
                value.asInstanceOf[T]
        } getOrElse { key.default }
    }

    def section(name: String): Config = Config(entries.values.filter(_.section == name).toSeq: _*)

    def <+=(other: Config): this.type = {
        entries ++= other.entries
        this
    }

    override def +=(entry: ConfigEntry): this.type = {
        entries += (entry.fullName -> entry)
        this
    }

    override def clear(): Unit = entries.clear()

    override def equals(obj: scala.Any): Boolean = obj match {
        case other: Config => entries == other.entries
        case _ => false
    }

    override def hashCode(): Int = entries.hashCode()
}

object Config {
    def apply(defs: ConfigEntry*): Config = new Config(defs: _*)

    private def makeAliases(name: String): Seq[String] = Seq(name, name.splitCamelCase.mkString("-")) map { _.toLowerCase }

    abstract class ConfigEntry {
        def section: String
        def name: String
        def fullName: String
        def value: Any
    }
    private case class SpecificConfigEntry[T](key: ConfigKey[T], value: T) extends ConfigEntry {
        override def section: String = key.section
        override def name: String = key.name
        override def fullName: String = key.fullName

        override def toString: String = s"$fullName := $value"
    }

    object Default

    protected trait ConfigValueParsers {
        abstract class Parser[T](val typeName: String) {
            def parse(value: String): Option[T]
        }

        implicit object StringParser extends Parser[String]("string") {
            override def parse(value: String): Option[String] = Some(value)
        }

        implicit object BoolParser extends Parser[Boolean]("boolean") {
            override def parse(value: String): Option[Boolean] = Try(value.toBoolean).toOption
        }

        implicit object IntParser extends Parser[Int]("number") {
            override def parse(value: String): Option[Int] = Try(value.toInt).toOption
        }
    }
    object ConfigValueParsers extends ConfigValueParsers

    class ConfigKey[T] private (val default: T, val section: String, val name: String)(private val _parser: ConfigValueParsers.Parser[T]) {
        val fullName: String = s"$section.$name"
        val aliases: Seq[String] = makeAliases(name)

        def :=(value: T): ConfigEntry = SpecificConfigEntry[T](this, value)
        def :=(defaultType: Default.type): ConfigEntry = this := default

        def parse(source: String): ConfigEntry = parseOption(source) getOrElse(throw new IllegalArgumentException(s"'$source' can not be converted into ${_parser.typeName}"))
        def parseOption(source: String): Option[ConfigEntry] = {
            _parser.parse(source) map { new SpecificConfigEntry[T](this, _) }
        }
    }

    abstract class Section(val name: String) {
        protected implicit def self: Section = this

        private val _keys = mutable.Buffer.empty[ConfigKey[_]]

        val aliases: Seq[String] = makeAliases(name)
        def keys: Seq[ConfigKey[_]] = _keys
        def registerKey(key: ConfigKey[_]): this.type = {
            assert(!(_keys contains key))
            _keys += key
            this
        }

        def key(name: String): ConfigKey[_] = {
            val normalizedName = name.trim.toLowerCase
            keys find { _.aliases contains normalizedName } getOrElse(throw new IllegalArgumentException(s"$name is not a known key name in section ${this.name}"))
        }

        def parse(entries: Seq[(String, String)]): Config = {
            val defs = for ((key, value) <- entries) yield this.key(key).parse(value)
            Config(defs: _*)
        }
    }

    object ConfigKey {
        def apply[T](default: T)(implicit section: Section, parser: ConfigValueParsers.Parser[T], name: sourcecode.Name): ConfigKey[T] = {
            val key = new ConfigKey[T](default, section.name, name.value)(parser)
            section.registerKey(key)
            key
        }
    }

    abstract class Definition {
        def sections: Seq[Section]
        lazy val keys: Seq[ConfigKey[_]] = sections flatMap { _.keys }

        def section(name: String): Section = {
            val normalizedName = name.trim.toLowerCase
            sections find { _.aliases contains normalizedName } getOrElse(throw new IllegalArgumentException(s"$name is not a known section name"))
        }
        def key(name: String): ConfigKey[_] = {
            name.split('.') match {
                case Array(sectionName, key) =>
                    section(sectionName).key(key)
                case _ =>
                    throw new IllegalArgumentException("Expected key to be combination of one section name and one key name")
            }

        }

        def parse(entries: Seq[(String, String)]): Config = {
            val defs = for ((key, value) <- entries) yield Definition.this.key(key).parse(value)
            Config(defs: _*)
        }
    }
}