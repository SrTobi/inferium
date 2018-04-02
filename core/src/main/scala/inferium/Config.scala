package inferium

import inferium.Config.{ConfigEntry, ConfigKey, SpecificConfigEntry}

import scala.collection.generic.Growable
import scala.collection.mutable


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

    abstract class ConfigEntry {
        def section: String
        def name: String
        def fullName: String
    }
    private case class SpecificConfigEntry[T](key: ConfigKey[T], value: T) extends ConfigEntry {
        override def section: String = key.section
        override def name: String = key.name
        override def fullName: String = key.fullName

        override def toString: String = s"$fullName := $value"
    }

    object Default

    class ConfigKey[T](val default: T, val section: String, val name: String) {
        val fullName: String = s"$section.$name"

        def :=(value: T): ConfigEntry = SpecificConfigEntry[T](this, value)
        def :=(default: Default.type): ConfigEntry = this := default
    }

    object ConfigKey {
        def apply[T](default: T)(implicit fullName: sourcecode.FullName): ConfigKey[T] = {
            val Array(section, name) = fullName.value.split('.').takeRight(2)
            new ConfigKey[T](default, section, name)
        }
    }
}