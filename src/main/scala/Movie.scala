
case class Movie(title: String) {

  def canChain(that: Movie): Boolean = {
    if (this == that) false
    else {
      val thisList: List[String] = title.split(" ").toList
      val thatList: List[String] = that.title.split(" ").toList

      def canChain(list: List[String], acc: Boolean): Boolean = {
        if (list.isEmpty) acc
        else if (thatList.startsWith(list)) true else canChain(list.tail, acc = false)
      }

      canChain(thisList, acc = false)
    }
  }

}
