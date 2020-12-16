package rsa

import scala.BigInt._
import scala.util.Random

case class Secret(d: BigInt)

case class Public(n: BigInt, e: BigInt)

case class AKey[P, S](public: P, secret: S)

class RSA(bitLengthC: Int) extends AsymmetricProtocol[Public, Secret] {
  override val nBits: Int = bitLengthC

  def gen(): Key = {
    val random = new Random()
    val p = BigInt.probablePrime(nBits, random)
    val q = BigInt.probablePrime(nBits, random)
    val n = p * q
    val lambda = (p - 1) * (q - 1) / (p gcd q)
    val e = 65537
    val d = e.modInverse(lambda)
    println(s"Public key = $n")
    println(s"Secret key = $d")
    AKey(Public(n, e), Secret(d))
  }

  def encrypt(plain: BigInt, public: Public): BigInt = plain modPow(public.e, public.n)

  def decrypt(cipher: BigInt, key: Key): BigInt = cipher modPow(key.secret.d, key.public.n)
}

object MainRSA {

  private def checkRandomString[T, U](asymmetricProtocol: AsymmetricProtocol[T, U], key: AKey[T, U]): Unit = {
    val message = BigInt(256, new Random())
    println(s"Message is: $message")
    val encrypted = asymmetricProtocol.encrypt(message, key.public)
    println(s"Encrypted message is: $encrypted")
    val decrypted = asymmetricProtocol.decrypt(encrypted, key)
    println(s"Decrypted message is: $decrypted")
    if (message != decrypted) {
      throw new Exception("Encrypt-decrypt error - messages are not equal")
    }
  }
  import rsa.OAEP._

  def main(args: Array[String]): Unit = {

    val rsa = new RSA(1024)
    val oaep = rsa.withOaep

    var start, finish: Long = 0

    start = System.currentTimeMillis()

    val keyRsa = rsa.gen()
    checkRandomString(rsa, keyRsa)


    finish = System.currentTimeMillis()
    println(s"RSA IS: ${(finish - start).toDouble/1000} seconds")

    start = System.currentTimeMillis()

    val keyOaep = oaep.gen()
    checkRandomString(oaep, keyOaep)

    finish = System.currentTimeMillis()
    println(s"RSA-OAEP time is ${(finish - start).toDouble/1000} seconds")
  }
}

