package coronamodel.graph

import scala.util.Random

case class RelationshipEdge(
    first: PersonNode,
    second: PersonNode,
    chanceToSee: Double,
    chanceToTransmit: Double,
    isTestProxy: Boolean = false,
    chanceToTransmitWithTest: Double = 0.0
  ) {

  /**
   * @return whether or not a test proxy was resolved
   */
  def resolveTestProxy(): Boolean = {
    if (isTestProxy) {
      if (first.checkTest && !second.checkTest) {
        second.setTestTrue()
        true
      } else if (!first.checkTest && second.checkTest) {
        first.setTestTrue()
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  def willTransmit(random: Random): Boolean = {
    if (!first.checkTest && !second.checkTest) {
      (random.nextDouble() < chanceToSee) && (random.nextDouble() < chanceToTransmit)
    } else {
      /*
      if (second.name == "kevin") {
        println("Chose not to hang because tested")
      }
       */
      random.nextDouble() < chanceToTransmitWithTest
    }
  }
}
