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

        if (c == 0 || c == r) 1 // edge values, column == row, always 1
        else pascal(c-1, r-1) + pascal(c, r-1) // prev row, sum of 1 column back and same column

    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def loop(chars: List[Char], stack: Int): Boolean = {
        if (chars.isEmpty)  stack == 0 // '(' and ')' should result in 0 if balanced
        else if (chars.head == '(') loop(chars.tail, stack + 1)
        else if (chars.head == ')') loop(chars.tail, stack - 1)
        else loop(chars.tail, stack)

      }

      if (chars.isEmpty) true // if empty, debatable as balanced
      else if (chars.head == chars.last) false // if head and last are equal brackets not balanced
      else loop(chars, 0)

    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
