package rsa

import scala.io.StdIn
import util.control.Breaks._
import scala.util.Random

trait PrimeTester {
  def isPrime(n: BigInt, k: Int): Boolean
}

object MillerRabin {
  def isPrime(n: BigInt, k: Int): Boolean = {
    if (n % 2 == 0) return false
    var t = (n - 1) / 2
    var s = 1
    while (t % 2 == 0) {
      t/=2
      s+=1
    }
    var a, x: BigInt = null
    val random = new Random()
    for ( _ <- 1 until k ) {
      breakable {
        a = 2 + (BigInt(n.bitCount + 1, random) % (n - 3))
        x = a modPow(t,n)
        if (x == 1 || x == n - 1) break()
        for (_ <- 1 until s) {
          x = x modPow(2,n)
          if (x == 1) return false
          if (x == (n - 1)) break()
        }
        return false
      }
    }
    true
  }
}

object Main extends App {
  import MillerRabin.isPrime
  import Console._
  val res = for {
    number <- StdIn.readLine("Number to check.\n").toIntOption.toRight("Input is not valid integer.")
    rounds <- StdIn.readLine("Amount of rounds.\n").toIntOption.toRight("Input is not valid integer.")
  } yield {
    val res = isPrime(number, rounds)
    println("#"*10 + "RESULT" + "#"*10)
    println(s"number of rounds: $rounds")
    println(s"$BOLD$RED$number$BLACK is ${if(res) "prime" else "not prime"}")
  }
  res.left.foreach(println)
}
