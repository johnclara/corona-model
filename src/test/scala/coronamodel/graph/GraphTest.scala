package coronamodel.graph

import org.specs2.Specification

import scala.collection.mutable

class GraphTest extends Specification {
  def is =
    s2"""
            GraphTest should
            ${Kevin().run}
      """
  /*
            ${Kevin().run}
            ${Jeanette().run}
            ${Justine().run}
            ${JustineAndJeanette().run}
   */

  def runTest(graph: Graph): Unit = {
    val timeWhenContracted = mutable.HashMap.empty[PersonNode, Double]
    val numRuns = 10000
    (0 until numRuns).foreach { _ =>
      graph.clear()
      var i = 0
      while (!graph.allHaveCorona) {
        graph.tick(i)
        i += 1
      }
      graph.nodes.foreach { node =>
        val curr = timeWhenContracted.getOrElse(node, 0.0)
        timeWhenContracted.put(node, curr + node.tickReceived.get)
      }
    }
    timeWhenContracted.mapValuesInPlace { case (key, value) =>
      value / numRuns
    }

    println(s"${graph.name} : $timeWhenContracted")
  }
  case class Kevin() {
    val name = "Kevin"
    //val jeanette = PersonNode("jeanette")
    val kevin = PersonNode("kevin")
    val nodes = Set(kevin)
    val edges = Set.empty[RelationshipEdge]
    val graph = Graph(name, nodes, edges)
    /*
      Alone, kevin will get it in 1003 days
      Kevin : HashMap(PersonNode(kevin,0.001,0.0) -> 1003.5)
     */
    def run = {
      runTest(graph)
      ok
    }
  }
  case class Jeanette() {
    val name = "Jeanette"
    val jeanettesChanceToTest = 1.0 / 14.0
    //val jeanettesChanceToTest = 0.0
    val jeanette = PersonNode("jeanette", chanceToTest = jeanettesChanceToTest)
    val kevin = PersonNode("kevin")
    val jeanetteKevin = RelationshipEdge(jeanette, kevin, chanceToSee = 2.0 / 7.0, chanceToTransmit = 1.0)
    val nodes = Set(jeanette, kevin)
    val edges = Set(jeanetteKevin)
    val graph = Graph(name, nodes, edges)
    /*
    With just Kevin & Jeanette:
    with no testing, kevin gets it in 504 days
      Jeanette : HashMap(PersonNode(kevin,0.001,0.0) -> 504.5569, PersonNode(jeanette,0.001,0.0) -> 504.4925)
    with testing once every 2 weeks, kevin gets it in 504 days
      Jeanette : HashMap(PersonNode(kevin,0.001,0.0) -> 614.4635, PersonNode(jeanette,0.001,0.07142857142857142) -> 503.1096)
     */
    def run = {
      runTest(graph)
      ok
    }
  }

  case class Justine() {
    val name = "Justine"
    val justinesChanceToTest = 1.0 / 14.0
    //val justinesChanceToTest = 0.0 / 14.0
    val justine = PersonNode("justine", chanceToTest = justinesChanceToTest)
    val neil = PersonNode("neil")
    val anthony = PersonNode("anthony")
    val irene = PersonNode("irene",
      externalChanceToContract=2*PersonNode.defaultExternalChanceToContract,
      chanceToTest = 1.0 / 14.0)
    val alan = PersonNode("alan", chanceToTest = 1.0 / 21)
    val steph = PersonNode("steph", chanceToTest = 1.0 / 21)

    val justineNeil = RelationshipEdge(justine, neil, chanceToSee = 1.0 / 14.0, chanceToTransmit = 0.25)
    val justineAnthony = RelationshipEdge(justine, anthony, 1.0 / 7.0, 1.0)
    val justineIrene = RelationshipEdge(justine, irene, 1.0 / 14.0, .25)
    val justineAlan = RelationshipEdge(justine, alan, 1.0 / 14.0, .25)
    val justineSteph = RelationshipEdge(justine, steph, 1.0 / 14.0, .25)

    val stephAlan = RelationshipEdge(alan, steph, 7.0 / 7.0, 1.0, isTestProxy = true, chanceToTransmitWithTest = 1.0)

    val nodes = Set(
      justine,
      neil,
      anthony,
      irene,
      alan,
      steph
    )

    val edges = Set(
      justineNeil,
      justineAnthony,
      justineIrene,
      justineAlan,
      justineSteph,
      stephAlan
    )

    val graph = Graph(name, nodes, edges)
    def run = {
      runTest(graph)
      ok
    }
  }

  case class JustineAndJeanette() {
    val justJustine = Justine()
    val justJeanette = Jeanette()
    val justineJeanette = new RelationshipEdge(justJustine.justine, justJeanette.jeanette, 1.0, .75, isTestProxy = true, chanceToTransmitWithTest = 0.1)
    val nodes = justJustine.nodes ++ justJeanette.nodes
    val edges = justJustine.edges ++ justJeanette.edges + justineJeanette
    val graph = Graph("Justine And Jeanette", nodes, edges)
    /*
     With Justine & Jeanette living together,

     without testing, kevin gets in 188 days
     Justine And Jeanette : HashMap(PersonNode(anthony,0.001,0.0) -> 190.5047, PersonNode(kevin,0.001,0.0) -> 188.6193, PersonNode(jeanette,0.001,0.0) -> 186.996, PersonNode(alan,0.001,0.047619047619047616) -> 163.394, PersonNode(irene,0.002,0.07142857142857142) -> 177.7405, PersonNode(neil,0.001,0.0) -> 221.6439, PersonNode(justine,0.001,0.0) -> 186.6309, PersonNode(steph,0.001,0.047619047619047616) -> 163.3819)

     with jeanette testing once every two weeks, kevin gets in 364 days
     Justine And Jeanette : HashMap(PersonNode(anthony,0.001,0.0) -> 430.1112, PersonNode(kevin,0.001,0.0) -> 364.2657, PersonNode(alan,0.001,0.047619047619047616) -> 390.522, PersonNode(irene,0.002,0.07142857142857142) -> 422.8871, PersonNode(neil,0.001,0.0) -> 854.6056, PersonNode(jeanette,0.001,0.07142857142857142) -> 185.519, PersonNode(justine,0.001,0.0) -> 185.2102, PersonNode(steph,0.001,0.047619047619047616) -> 390.5184)

     with jeanette & justine testing once every two weeks, kevin gets it in 445 days
     Justine And Jeanette : HashMap(PersonNode(anthony,0.001,0.0) -> 593.5832, PersonNode(kevin,0.001,0.0) -> 445.5879, PersonNode(alan,0.001,0.047619047619047616) -> 427.8077, PersonNode(irene,0.002,0.07142857142857142) -> 468.9015, PersonNode(justine,0.001,0.07142857142857142) -> 188.997, PersonNode(neil,0.001,0.0) -> 914.5144, PersonNode(jeanette,0.001,0.07142857142857142) -> 189.3141, PersonNode(steph,0.001,0.047619047619047616) -> 427.7355)

     */
    def run = {
      runTest(graph)
      ok
    }
  }
}
