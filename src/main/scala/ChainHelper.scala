
class ChainHelper(thisTitle: String) {

  /**
   * This is a chain on the RIGHT of this node only operation
   * we are checking for
   */
  def canChain(thatTitle: String): Boolean = {
    if (thisTitle == thatTitle) false
    else {
      val thisList: List[String] = thisTitle.split(" ").toList
      val thatList: List[String] = thatTitle.split(" ").toList

      def canChain(list: List[String], acc: Boolean): Boolean = {
        if (list.isEmpty) acc
        else if (thatList.startsWith(list)) true else canChain(list.tail, acc = false)
      }

      canChain(thisList.tail, acc = false)
    }
  }

}
