package rsa

import rsa.BigIntUtils.BigIntOps

import scala.util.Random

object OAEP {
  self =>

  def withOAEP[P, S](orig: AsymmetricProtocol[P, S]): AsymmetricProtocol[P, S] = new AsymmetricProtocol[P, S] {
    val nBits: Int = orig.nBits
    val k0 = 80
    val k1 = 80
    val random = new Random()
    val mgf1 = new MGF1()
    val G: BigInt => BigInt = mgf1.generateMask(_, nBits - k0)
    val H: BigInt => BigInt = mgf1.generateMask(_, k0)

    override def gen(): Key = orig.gen()

    override def encrypt(plain: BigInt, p: PublicPart): BigInt = {
      val a1 = plain << k1
      val r = BigInt(k0, random)
      val X = a1 ^ G(r)
      val Y = r ^ H(X)
      val XY = X.concat(Y, k0)
      orig.encrypt(XY, p)
    }

    override def decrypt(cipher: BigInt, k: Key): BigInt = {
      val XY = orig.decrypt(cipher, k)
      val X = XY >> k0
      val Y = XY ^ (X << k0)
      val r = Y ^ H(X)
      val a1 = X ^ G(r)
      if (a1.mod(1 << k0) != 0) {
        throw new Exception("Message corrupted.")
      }
      a1 >> k1
    }
  }

  implicit class OAEPOps[P, S](asymProtocol: AsymmetricProtocol[P, S]) {
    def withOaep: AsymmetricProtocol[P, S] = self.withOAEP(asymProtocol)
  }

}

class MGF1() {
  def generateMask(mgfSeedBigInt: BigInt, maskLenBits: Int): BigInt = {
    val digest = java.security.MessageDigest.getInstance("SHA-1")
    val maskLen = maskLenBits / 8
    val mgfSeed = mgfSeedBigInt.toByteArray
    val hashCount = (maskLen + digest.getDigestLength - 1) / digest.getDigestLength
    var mask = new Array[Byte](0)
    for (i <- 0 until hashCount) {
      digest.update(mgfSeed)
      digest.update(new Array[Byte](3))
      digest.update(i.toByte)
      val hash = digest.digest
      val c: Array[Byte] = new Array[Byte](mask.length + hash.length)
      System.arraycopy(mask, 0, c, 0, mask.length)
      System.arraycopy(hash, 0, c, mask.length, hash.length)
      mask = c
    }
    val output = new Array[Byte](maskLen)
    System.arraycopy(mask, 0, output, 0, output.length)
    BigInt(1, output)
  }
}



