package rsa

trait AsymmetricProtocol[P, S] {
  type PublicPart = P
  type Key = AKey[P, S]
  val nBits: Int

  def gen(): Key

  def encrypt(plain: BigInt, p: PublicPart): BigInt

  def decrypt(cipher: BigInt, k: Key): BigInt
}
