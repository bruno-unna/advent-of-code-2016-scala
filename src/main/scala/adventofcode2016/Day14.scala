package adventofcode2016

import zio.*
import zio.stream.{ZSink, ZStream}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.matching.compat.Regex

object Day14 extends ZIOAppDefault:

  private type OtpEntry = (Int, String, Char)

  object Hasher:
    trait Service:
      def init(stretched: Boolean): UIO[Unit]

      def md5(s: String): UIO[String]

      def nextKeyCandidate(salt: String): UIO[(OtpEntry, Boolean)]

    private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

    private val tripleDigitRE: Regex = """^.*?(.)\1\1.*$""".r

    val live: ZLayer[Any, Nothing, Hasher.Service] = ZLayer.fromZIO:
      for
        index <- Ref.make(0)
        cache <- Ref.make(Map.empty[String, String])
        stretched <- Ref.make(false)
        service =
          new Service:
            override def init(newStretched: Boolean): UIO[Unit] =
              for
                _ <- index.set(0)
                _ <- cache.set(Map.empty[String, String])
                _ <- stretched.set(newStretched)
              yield ()

            override def md5(s: String): UIO[String] =
              def computeMd5(s: String, stretched: Boolean): String =
                @tailrec
                def loop(s: String, n: Int): String =
                  if n == 0 then s
                  else
                    val md = newMd5()
                    val hashBytes = md.digest(s.getBytes(StandardCharsets.UTF_8))
                    val newHash = hashBytes.map("%02x".format(_)).mkString
                    loop(newHash, n - 1)

                loop(s, if stretched then 2017 else 1)

              for
                currentCache <- cache.get
                isStretched <- stretched.get
                hash <- currentCache.get(s) match
                  case Some(h) =>
                    ZIO.succeed(h)
                  case None =>
                    val h = computeMd5(s, isStretched)
                    cache.update(_ + (s -> h)).as(h)
              yield hash

            override def nextKeyCandidate(salt: String): UIO[(OtpEntry, Boolean)] =
              val batchSize = 64

              def isOtpKeyValid(salt: String, otpEntry: OtpEntry): UIO[Boolean] =
                val (candidateIndex, _, char) = otpEntry
                val start = candidateIndex + 1
                val end = start + 1000

                ZIO.foreachPar(start until end): n =>
                  md5(salt + n).map(_.contains(char.toString * 5))
                .map(_.contains(true))

              def batchFindCandidates(from: Int): UIO[Seq[(Int, String)]] =
                ZIO
                  .foreach(from until from + batchSize)(n => md5(salt + n).map(h => (n, h)).fork)
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
                otpEntry = (newIndex, hash, repeatedChar)
                isValid <- isOtpKeyValid(salt, otpEntry)
              yield (otpEntry, isValid)

      yield service

  def calculateOTP(salt: String, stretched: Boolean = false): URIO[Hasher.Service, Vector[OtpEntry]] =
    for
      hasher <- ZIO.service[Hasher.Service]
      _ <- hasher.init(stretched)

      otp <- ZStream
        .repeatZIO(hasher.nextKeyCandidate(salt))
        .filter(_._2)
        .map(_._1)
        .run(ZSink.take(64))
    yield otp.toVector

  private def program =
    val salt = "ihaygndm"

    for
      otp <- calculateOTP(salt)
      _ <- Console.printLine(s"index for key 64 (normal MD5): ${otp(63)}")

      stretchedOtp <- calculateOTP(salt, stretched = true)
      _ <- Console.printLine(s"index for key 64 (stretched MD5): ${stretchedOtp(63)}")
    yield ()

  override def run: ZIO[Any, IOException, Unit] = program.provide(Hasher.live)
