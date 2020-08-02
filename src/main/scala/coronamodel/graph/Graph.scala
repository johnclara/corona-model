package coronamodel.graph

import scala.collection.mutable
import scala.util.Random

case class Graph(name: String, nodes: Set[PersonNode], edges: Set[RelationshipEdge], r: Random = Random) {
  private val remainingNodes = mutable.Set.from(nodes)
  def tick(i: Int): Unit = {
    nodes.foreach { node =>
      if (r.nextDouble() < node.externalChanceToContract) {
        remainingNodes.remove(node)
        node.giveCorona(i)
      }
      if (r.nextDouble() < node.chanceToTest) {
        node.getTest()
      }
    }
    var updatedProxies = true
    while (updatedProxies) {
      updatedProxies = false
      edges.foreach { edge =>
        if (edge.resolveTestProxy()) {
          updatedProxies = true
        }
      }
    }
    edges.foreach { edge =>
      if (
        edge.willTransmit(r)
      ) {
        if (edge.first.hasCorona()) {
          remainingNodes.remove(edge.second)
          edge.second.giveCorona(i)
        }
        if (edge.second.hasCorona()) {
          remainingNodes.remove(edge.first)
          edge.first.giveCorona(i)
        }
      }
    }
  }

  def clear(): Unit = {
    nodes.foreach { node =>
      node.clear()
    }
    remainingNodes.addAll(nodes)
  }

  def allHaveCorona: Boolean =
    remainingNodes.isEmpty
}
