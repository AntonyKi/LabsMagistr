package rsa

object BigIntUtils {
  implicit class BigIntOps(i: BigInt) {
    def concat(j: BigInt, jBits: Int): BigInt = (i << jBits) ^ j
  }
}
