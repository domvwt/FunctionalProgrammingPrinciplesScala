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
      def factorial (x: Int): Int = {
        def accumulator (y: Int, acc: Int): Int =
          if (c == 0 || y == 0) acc
          else accumulator(y - 1, y * acc)
        accumulator(x, 1)
      }
      factorial(r) / (factorial(c) * factorial(r-c))
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def bal_aux (chars: List[Char], open: Int): Boolean = {
        if (open < 0) false
        else if (chars.isEmpty) {
          open == 0
        }
        else if (chars.head == '(') bal_aux(chars.tail, open+1)
        else if (chars.head == ')') bal_aux(chars.tail, open-1)
        else bal_aux(chars.tail, open)
      }
      bal_aux(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def tender (money: Int, coins: List[Int], combos: Int): Int =
        if (money < 0 || coins.isEmpty) 0
        else if (money == 0) combos + 1
        else tender(money - coins.head, coins, combos) + tender(money, coins.tail, combos)
      tender(money, coins, 0)
    }
  }
