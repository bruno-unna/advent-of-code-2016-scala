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

            override def nextKeyCandidate(salt: String, stretched: Boolean): UIO[OtpEntry] =
              for
                firstI <- index.getAndIncrement
                firstHash <- md5(salt + firstI, stretched)
                (newI, newHash) <- ZIO.iterate(firstI -> firstHash)(t => !tripleDigitRE.matches(t._2)):
                  case (i, hash) =>
                    for
                      newI <- index.getAndIncrement
                      newHash <- md5(salt + newI, stretched)
                    yield newI -> newHash
                repeatedChar = newHash match
                  case tripleDigitRE(cs) => cs.charAt(0)
              yield (newI, newHash, repeatedChar)

            override def isOtpKeyValid(salt: String, otpEntry: OtpEntry, stretched: Boolean): UIO[Boolean] =
              for
                i <- index.get
                loopResult <- ZIO.iterate((i + 1, false))(t => t._1 <= i + 1000 && !t._2):
                  case (i, valid) =>
                    for
                      hash <- md5(salt + i, stretched)
                      valid = hash.contains(otpEntry._3.toString * 5)
                    yield (i + 1, valid)
              yield loopResult._2
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
