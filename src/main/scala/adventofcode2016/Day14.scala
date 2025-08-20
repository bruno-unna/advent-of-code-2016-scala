package adventofcode2016

import zio.*

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.matching.compat.Regex

object Day14 extends ZIOAppDefault:

  private type OtpEntry = (Int, String, Char)

  object Hasher:
    trait Service:
      def md5(s: String, stretched: Boolean): UIO[String]

      def nextKeyCandidate(salt: String, stretched: Boolean): UIO[OtpEntry]

      def isOtpKeyValid(salt: String, otpEntry: OtpEntry, stretched: Boolean): UIO[Boolean]

    private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

    private val tripleDigitRE: Regex = """^.*(.)\1\1.*$""".r

    val live: ZLayer[Any, Nothing, Hasher.Service] = ZLayer.fromZIO:
      for
        index <- Ref.make(0)
        cache <- Ref.make(Map.empty[String, String])
        service =
          new Service:
            private def computeMd5(s: String, stretched: Boolean): String =
              @tailrec
              def loop(s: String, n: Int): String =
                if n == 0 then s
                else
                  val md = newMd5()
                  val hashBytes = md.digest(s.getBytes(StandardCharsets.UTF_8))
                  val newHash = hashBytes.map("%02x".format(_)).mkString
                  loop(newHash, n - 1)

              loop(s, if stretched then 2017 else 1)

            override def md5(s: String, stretched: Boolean): UIO[String] =
              cache.get.flatMap: currentCache =>
                currentCache.get(s) match
                  case Some(hash) =>
                    ZIO.succeed(hash)
                  case None =>
                    val newHash = computeMd5(s, stretched)
                    cache.update(_ + (s -> newHash)).as(newHash)

            val batchSize = 64

            override def nextKeyCandidate(salt: String, stretched: Boolean): UIO[OtpEntry] =
              def batchFindCandidates(from: Int): UIO[Seq[(Int, String)]] =
                ZIO
                  .foreach(from until from + batchSize)(n => md5(salt + n, stretched).map(h => (n, h)).fork)
                  .flatMap(f => Fiber.collectAll(f).join)
                  .map(_.filter(h => tripleDigitRE.matches(h._2)))

              for
                firstI <- index.get
                (newIndex, hash, _) <- ZIO.iterate(firstI, "", false)(!_._3):
                  case (offset, _, _) =>
                    for
                      moreHashes <- batchFindCandidates(offset)
                      result =
                        if moreHashes.isEmpty then (offset + batchSize, "", false)
                        else (moreHashes.head._1, moreHashes.head._2, true)
                    yield result
                _ <- index.set(newIndex + 1)
                repeatedChar = hash match
                  case tripleDigitRE(cs) => cs.charAt(0)
              yield (newIndex, hash, repeatedChar)

            override def isOtpKeyValid(salt: String, otpEntry: OtpEntry, stretched: Boolean): UIO[Boolean] = {
              val batchSize = 100

              def batchFindNextHashes(from: Int, char: Char): UIO[Seq[String]] =
                ZIO
                  .foreach(from until from + batchSize)(n => md5(salt + n, stretched).fork)
                  .flatMap(f => Fiber.collectAll(f).join)
                  .map(_.filter(_.contains(char.toString * 5)))

              for
                currentIdx <- index.get
                top = currentIdx + 1000
                (idx, valid) <- ZIO.iterate((currentIdx + 1, false))(t => t._1 <= top && !t._2):
                  case (idx, valid) =>
                    for
                      batch <- batchFindNextHashes(idx, otpEntry._3)
                      result =
                        if batch.isEmpty then false
                        else true
                    yield (idx + batchSize, result)
              yield valid
            }
      yield service

  def calculateOTP(salt: String, stretched: Boolean = false): URIO[Hasher.Service, Vector[OtpEntry]] = {
    for
      hasher <- ZIO.service[Hasher.Service]
      firstOtp <- ZIO.succeed(Vector.empty[OtpEntry])
      firstEntry <- hasher.nextKeyCandidate(salt, stretched)
      otpTracker <- ZIO.iterate((firstOtp, firstEntry))(_._1.length <= 64):
        case (otp, otpEntry) =>
          for
            valid <- hasher.isOtpKeyValid(salt, otpEntry, stretched)
            newOtp = if valid then otp :+ otpEntry else otp
            newEntry <- hasher.nextKeyCandidate(salt, stretched)
          yield (newOtp, newEntry)
    yield otpTracker._1
  }

  private def program =
    val salt = "ihaygndm"

    for
      otp <- calculateOTP(salt)
      _ <- Console.printLine(s"index for 64th key (normal MD5): ${otp(64)}")

      stretchedOtp <- calculateOTP(salt, stretched = true)
      _ <- Console.printLine(s"index for 64th key (stretched MD5): ${stretchedOtp(64)}")
    yield ()

  override def run: ZIO[Any, IOException, Unit] = program.provide(Hasher.live)
