import scala.annotation.tailrec

object Main {
  var count = 0
  var min = Integer.MAX_VALUE
  var max = Integer.MIN_VALUE

  // 数字そのもの
  def id(n : Int) : Option[Int] = {
    Some(n)
  }
  // 階乗
  def fact(n : Int) : Option[Int] = {
    // 無限に呼ばれてしまうので1禁止
    if (n == 1)
      None
    else if (n < 0)
      None
    else if (n == 0 || n == 1)
      Some(1)
    else
      Some(List.range(2, n + 1).product)
  }
  // 平方根
  def sqrt(n : Int) : Option[Int] = {
    // 無限に呼ばれてしまうので1以下禁止
    if (n <= 1)
      None
    else {
      val _n = Math.sqrt(n.toDouble).toInt
      if (Math.pow(_n, 2).toInt == n)
        Some(_n)
      else
        None
    }
  }
  // 自乗
  def square(n : Int) : Option[Int] = {
    // 無限に呼ばれてしまうので1以下禁止
    if (n <= 1)
      None
    else
      Some(Math.pow(n, 2).toInt)
  }

  // 加算
  def add(n : Int, m : Int) : Option[Int] = {
    Some(n + m)
  }
  // 減算
  def sub(n : Int, m : Int) : Option[Int] = {
    Some(n - m)
  }
  // 乗算
  def mul(n : Int, m : Int) : Option[Int] = {
    Some(n * m)
  }
  // 除算
  def div(n : Int, m : Int) : Option[Int] = {
    if (m == 0 || n % m != 0)
      None
    else
      Some(n / m)
  }
  // 剰余
  def mod(n : Int, m : Int) : Option[Int] = {
    if (m != 0)
      Some(n % m)
    else
      None
  }
  // 累乗
  def power(n : Int, m : Int) : Option[Int] = {
    Some(Math.pow(n.toDouble, m.toDouble).toInt)
  }
//  def route(n : Int, m : Int) : Option[Int] = {
//    Some(Math.route(n.toDouble, m.toDouble).toInt)
//  }

  // サポートする演算・関数を定義
  // 1引数の関数群
  def oneArgFuncs = List((id _, "")/*, (sqrt _, "sqrt"), (square _, "square")*/)
  // 2引数の関数群
  def twoArgsFuncs = List((add _, "+"), (sub _, "-"), (mul _, "*"),
    (div _, "/")/*, (mod _, "mod"), (power _, "pow"), (route, "route")*/)

  // 10になるかチェックする
  def check(xs : List[Int]) : Unit = {
    //@tailrec
    def __check(xs : List[Int], r : Int, ms : String) : List[(Option[Int], String)] = {
      xs match {
        case (x :: xss) => {
          (for ((f, sf) <- oneArgFuncs; (g, sg) <- oneArgFuncs; (h, sh) <- twoArgsFuncs;
                fx <- f(x); gx <- g(r); hx <- h(gx, fx))
             yield __check(xss, hx, "(" + (if (sg == "") ms + " " else sg + "(" + ms + ") ") + sh + " " + (if (sf == "") x  else sf + "(" + x + ")") + ")")).flatten
        }
        case _ => if (r == 10) List((Some(r), ms)) else Nil
      }
    }
    def _check(xs : List[Int]) : Unit = {
      // 式構造を組み立てる
      val x = xs.head
      val xss = xs.tail
      (for ((f, sf) <- oneArgFuncs; fx <- f(x))
         yield __check(xss, fx, fx.toString)).foreach {
        _.map {
          case (Some(n), ms) if n == 10 => {
            count += 1
            max = Math.max(max, xs.sum)
            min = Math.min(min, xs.sum)
            println("xs = " + xs + ", ms = " + ms + ", n = " + n)
          }
          case c                        => println(c)//println("xs = " + xs)
        }
      }
    }
    _check(xs)
  }
  def main (args: Array[String]): Unit = {
    // 1〜10の数字4通りの全ての数字を列挙
    for (a <- 1 until 10; b <- 1 until 10; c <- 1 until 10; d <- 1 until 10)
      yield check(List(a, b, c, d))
//    for (xs <- List(1, 2, 3, 4).permutations)
//      check(xs)
    println("count = " + count + ", max = " + max + ", min = " + min)
  }
}
