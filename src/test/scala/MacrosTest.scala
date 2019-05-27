package testmeta
import org.scalatest
import org.scalatest.{Matchers, WordSpec}

object TestEntities {
  @mappable case class SimpleCaseClass(i: Int, s: String)
}

class MappableTest extends WordSpec with Matchers {
  import TestEntities._

  "simple case class" should {
    "deserialize to map" in {
      val testInstance = SimpleCaseClass(i = 42, s = "something")
      testInstance.asMap shouldBe Map("i" -> 42, "s" -> "something")

      val testInstance2 = SimpleCaseClass(i = 99, s = "somewhere")
      testInstance2.asMap shouldBe Map("i" -> 99, "s" -> "somewhere")
    }
  }
}