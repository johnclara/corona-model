package coronamodel.graph

object PersonNode {
  val defaultExternalChanceToContract = 0.001
  val defaultChanceToTest = 0.0
}

case class PersonNode(
    name: String,
    externalChanceToContract: Double = PersonNode.defaultExternalChanceToContract,
    chanceToTest: Double = PersonNode.defaultChanceToTest
  ) {
  private var _hasCorona: Boolean = false
  private var tickReceivedCorona: Option[Int] = None
  //private val edges  = new mutable.HashSet[RelationshipEdge]
  private var testResult: Boolean = false

  def hasCorona(): Boolean = {
    _hasCorona
  }

  def giveCorona(i: Int): Unit = {
    _hasCorona = true
    if (tickReceivedCorona.isEmpty) {
      tickReceivedCorona = Some(i)
    }
  }

  def tickReceived: Option[Int] = {
    tickReceivedCorona
  }

  def checkTest: Boolean = {
    return testResult
  }

  def clear(): Unit = {
    _hasCorona = false
    tickReceivedCorona = None
    testResult = false
  }

  def getTest(): Unit = {
    /*
    if (name == "jeanette") {
      println(s"Got tested and found: ${_hasCorona}")
    }
     */
    if (_hasCorona) {
      testResult = _hasCorona
    }
  }
  def setTestTrue(): Unit = {
    testResult = true
  }
  /*
  def addEdge(edge: RelationshipEdge): Unit = {
    edges.add(edge)
  }
   */
}
