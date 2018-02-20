val rootValueTab = "+--"
val rootDirectChildTab = "\t+--"
def freeDepthChildTab(depth: Int) = "\t|" + "\t" * depth + "+--"

// sample result of processing
// (root(будланула(nsubj(куздра(amod=Глокая)))(advmod=штеко)(dobj=бокра)(cc=и)(conj(кудрячит(dobj=бокрёнка)))))
def processRawTree(rawTree: String) = rawTree
  // filter new line, tab and whitespace
  .filter(c => c != '\n' && c != '\t' && c != ' ')
  // replace all " on leaf nodes and add artificial = to be used as leaf indicator
  .replaceAll("(\\p{L}+)(\")(\\p{L}+)", "$1=$3")
  // replace rest of "
  .replaceAll("\"", "")

// remove excessive | in last element
def cleanupFinalTree(finalTree: String) = {
  val finalTreeArray = finalTree.split("\\n")
  val lastTerm = finalTreeArray.reverse.takeWhile(_.contains('|')).map(_.replaceAll("\\|", "")).reverse
  (finalTreeArray.dropRight(lastTerm.length) ++ lastTerm) mkString "\n"
}


// the fact that input tree is always balanced with proper quantity of parentheses
def processCleanTree(currentTree: String, processedTree: String, collectedParenthesesList: List[String]): String = {
  // terminate
  if (currentTree.isEmpty) cleanupFinalTree(processedTree)
  // drop opening parentheses, no string accumulator growth, add opening parentheses to balancing array
  else if (currentTree.head == '(') processCleanTree(currentTree.tail, processedTree, collectedParenthesesList :+ currentTree.head.toString)
  // drop closing parentheses, no string accumulator growth, remove closing parentheses from balancing array - rebalancing those
  else if (currentTree.head == ')') processCleanTree(currentTree.tail, processedTree, collectedParenthesesList.init)
  // actual textual data processing
  else {
    // collect chars until next parentheses
    val meaningfulMember = currentTree.takeWhile(c => c != ')' && c != '(')
    processCleanTree(
      // remove collected from rest
      currentTree.drop(meaningfulMember.length),
      // process collected
      processedTree + transformMember(currentTree, meaningfulMember.split("="), collectedParenthesesList),
      // no rebalancing needed
      collectedParenthesesList)
  }
}

def transformMember(currentTree: String, memberSplit: Array[String], currentCollectedParenthesesList: List[String]): String = {

  // allows fetching leaf value if present by using = as token in transformMember invocation
  def potentialLeafValueProcessor(probablyLeaf: Array[String], tabPrefix: String) =
    probablyLeaf.tail.headOption match {
      case Some(s) => tabPrefix + s + "\n"
      case None => ""
    }

  // we met root
  if (currentCollectedParenthesesList.length == 1) {
    memberSplit.head + "\n" + potentialLeafValueProcessor(memberSplit, rootValueTab)
  }
  // we met root value
  else if (currentCollectedParenthesesList.length == 2) {
    rootValueTab + memberSplit.head + "\n" + potentialLeafValueProcessor(memberSplit, "|" + rootDirectChildTab)
  }
  // we met direct root child
  else if (currentCollectedParenthesesList.length == 3) {
    rootDirectChildTab + memberSplit.head + "\n" +
      potentialLeafValueProcessor(memberSplit, freeDepthChildTab(currentCollectedParenthesesList.length - 2))
  }
  // we met none of the above
  else {
    freeDepthChildTab(currentCollectedParenthesesList.length - 3) + memberSplit.head + "\n" +
      potentialLeafValueProcessor(memberSplit, freeDepthChildTab(currentCollectedParenthesesList.length - 2))
  }
}


// processing samples here
val stringComplex =
  """
  (root ("будланула"
    (nsubj ("куздра"
      (amod "Глокая")))
    (advmod "штеко")
    (dobj "бокра")
    (cc "и")
    (conj ("кудрячит"
      (dobj "бокрёнка")))
    )
  )
  """

println(processCleanTree(processRawTree(stringComplex), "", List()))

val stringSimple =
  """
  (root "будланула")
  """

println(processCleanTree(processRawTree(stringSimple), "", List()))

val stringCommon =
  """
  (root
    ("будланула"
      (nsubj "куздра")
      (conj "кудрячит")
    )
  )
  """

println(processCleanTree(processRawTree(stringCommon), "", List()))

val stringDeep =
  """
  (root
    ("воробьиная"
      (nsubj
        ("истошная"
          (amod "оскаленная")
          (advmod
            ("хищная"
              (dobj "неистовая")
              (nsubj "стая")
              (conj "голосит")
            )
          )
        )
      )
      (nsubj
        ("вечная"
          (advmod "весна")
          (advmod
            ("в"
              (conj "одиночной")
              (cc "камере")
            )
          )
        )
      )
      (cc "во")
      (cc "мне")
    )
  )
  """

println(processCleanTree(processRawTree(stringDeep), "", List()))