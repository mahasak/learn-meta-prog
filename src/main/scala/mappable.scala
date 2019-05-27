package testmeta
import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._

class mappable extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    val q"..$mods class $tName (..$params) extends $template" = defn

    val keyValues: Seq[Term] = params.map { param =>
      val memberName = Term.Name(param.name.value)
      q"${param.name.value} -> $memberName"
    }

    q"""
      ..$mods class $tName(..$params) {
        def asMap(): Map[String, Any] = Map[String, Any](..$keyValues)
      }
    """
  }
}