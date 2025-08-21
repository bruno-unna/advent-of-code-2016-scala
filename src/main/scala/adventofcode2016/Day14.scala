package adventofcode2016

import zio.*
import zio.stream.{ZSink, ZStream}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.matching.compat.Regex

/**
 * Solves the Day 14 puzzle from Advent of Code 2016, which involves generating a one-time pad (OTP)
 * of keys based on MD5 hashing with optional "stretching".
 */
object Day14 extends ZIOAppDefault:

  /**
   * Represents a valid OTP key entry, containing the index, the hash, and the character
   * that forms the triple and quintuplet.
   */
  private type OtpEntry = (Int, String, Char)

  /**
   * The core service for generating and validating hashes for the OTP.
   * It manages a memoization cache and an index to track progress.
   */
  object Hasher:
    /**
     * Defines the interface for the hashing service.
     */
    trait Service:
      /**
       * Initializes the hasher's state, resetting the index and cache.
       *
       * @param stretched A flag indicating whether to use stretched hashing for subsequent operations.
       * @return An effect that completes when the state is initialized.
       */
      def init(stretched: Boolean): UIO[Unit]

      /**
       * Computes the MD5 hash of a given string.
       * The hashing process is either normal (1 iteration) or stretched (2017 iterations),
       * based on the service's internal state. The result is memoized.
       *
       * @param s The string to hash.
       * @return An effect that, when executed, produces the MD5 hash as a hex string.
       */
      def md5(s: String): UIO[String]

      /**
       * Finds the next valid OTP key candidate by iterating through hashes.
       * A key candidate is a hash containing a triple-digit sequence.
       *
       * @param salt The salt string to prepend to the index.
       * @return An effect that, when executed, produces a tuple containing the OTP entry
       *         and a boolean indicating if it is a valid OTP key.
       */
      def nextKeyCandidate(salt: String): UIO[(OtpEntry, Boolean)]

    private def newMd5(): MessageDigest = MessageDigest.getInstance("MD5")

    private val tripleDigitRE: Regex = """^.*?(.)\1\1.*$""".r

    /**
     * A ZLayer that provides a live implementation of the Hasher service.
     * The service instance is managed by ZIO and can be shared across the application.
     */
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
              /**
               * Computes the MD5 hash of a string, applying stretching if required.
               *
               * @param s         The input string.
               * @param stretched A boolean flag to enable or disable stretching.
               * @return The resulting MD5 hash as a hex string.
               */
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

              /**
               * Validates a potential OTP key by checking for a quintuplet of the triple's character
               * in the next 1000 hashes.
               *
               * @param salt     The salt string.
               * @param otpEntry The OTP key candidate to validate.
               * @return An effect that evaluates to `true` if the key is valid, `false` otherwise.
               */
              def isOtpKeyValid(salt: String, otpEntry: OtpEntry): UIO[Boolean] =
                val (candidateIndex, _, char) = otpEntry
                val start = candidateIndex + 1
                val end = start + 1000

                ZIO.foreachPar(start until end): n =>
                  md5(salt + n).map(_.contains(char.toString * 5))
                .map(_.contains(true))

              /**
               * Finds all hashes with a triple-digit sequence within a given batch.
               *
               * @param from The starting index of the batch.
               * @return An effect that produces a sequence of (index, hash) pairs for all
               *         candidates in the batch.
               */
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

  /**
   * Calculates the full OTP by generating and filtering keys.
   * It uses a ZStream to repeatedly call `nextKeyCandidate` until 64 valid keys are found.
   *
   * @param salt      The salt string for the hashing process.
   * @param stretched A flag to enable or disable stretched hashing.
   * @return An effect that produces a Vector of 64 valid OTP keys.
   */
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

  /**
   * The main entry point for the ZIO application.
   * It runs the `program` effect, providing the `Hasher.live` layer.
   */
  override def run: ZIO[Any, IOException, Unit] = program.provide(Hasher.live)