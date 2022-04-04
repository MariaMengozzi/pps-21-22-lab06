package u06lab.solution

import org.junit.Assert.*
import org.junit.Test

class CombinerTests {
  val f: Functions = FunctionsImpl

  @Test
  def testFunctions() = {
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    println((0.0, f.sum(List()))) // 0.0
    println(("abc", f.concat(Seq("a", "b", "c"))))
    println(("", f.concat(Seq())))
    println((3, f.max(List(-10, 3, -5, 0))))
    println((Integer.MIN_VALUE, f.max(List())))
  }

  @Test
  def testSumCombiner() =
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001)

  @Test
  def testSumCombinerNil() =
    assertEquals(0.0, f.sum(List()), 0.001)

  @Test
  def testConcat() =
    assertEquals("abc", f.concat(Seq("a", "b", "c")))

  @Test
  def testConcatNil() =
    assertEquals("", f.concat(Seq()))

  @Test
  def testMax() =
    assertEquals(3, f.max(List(-10, 3, -5, 0)))

  @Test
  def testMazNil() =
    assertEquals(Integer.MIN_VALUE, f.max(List()))
}
