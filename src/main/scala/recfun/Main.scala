package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c==0 || c==r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty) false else findBalance(chars.head, chars.tail)
  }

  def findBalance(head: Char, tail: List[Char]): Boolean = {
    if(tail.isEmpty) false
    else {
      if(head == ')') false
      else {
        if(head == '(') {
          findStartEnd(tail)
        }
        else findBalance(tail.head, tail.tail)
      }
    }
  }


  def findStartEnd(list: List[Char]): Boolean = {
    var startCount: Int = 0
    var endCounts: Int = 0

    if(!list.isEmpty) {
      if(list.head == ')' || list.head == '('){
        if(list.head == ')') {
          endCounts=endCounts+1
          if(startCount-endCounts-1 == 0){
            findStartEnd(list.tail)
          }
          else false
        } else {
          if(list.head == '(') {
            startCount=startCount+1
            findStartEnd(list.tail)
          }
          if(startCount-endCounts-1 == 0) true else false
        }
      } else findStartEnd(list.tail)

    } else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countCoins(m: Int, c: List[Int]) : Int = {
      if (c.isEmpty) 0
      else if (m - c.head == 0) 1
      else if (m - c.head < 0) 0
      else countChange(m - c.head, c) + countChange(m, c.tail)
    }
    countCoins(money, coins.sorted)
  }
}
