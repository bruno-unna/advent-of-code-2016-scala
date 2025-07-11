package adventofcode2016

import zio._
import zio.nio.file.{Path, Files}
import java.io.IOException
import java.nio.charset.StandardCharsets
import zio.nio.charset.Charset

/** A utility object for common Advent of Code tasks, particularly for reading
  * input files.
  */
object Util:

  /** Reads the entire content of a resource file from the classpath as a single
    * String.
    *
    * This method expects the file to be located in the `src/main/resources`
    * directory of the project (or `src/test/resources` if called from a test
    * context that can access it). The `fileName` parameter should specify the
    * path relative to the resources root, including a leading slash if it's at
    * the root (e.g., "/01.txt").
    *
    * The operation is wrapped in a ZIO effect, handling potential
    * `IOException`s that might occur during file access (e.g., file not found,
    * permissions issues).
    *
    * @param fileName
    *   The name of the resource file to read (e.g., "/input.txt"). Must include
    *   a leading slash if the file is at the root of the resources directory.
    * @return
    *   A `ZIO` effect that, when executed, will yield the content of the file
    *   as a `String`, or fail with an `IOException` if the file cannot be read
    *   or found.
    */
  def readBigString(fileName: String): ZIO[Any, IOException, String] =

    val zioPathEffect: IO[Throwable, Path] = ZIO.attempt {
      // Attempt to get the URL of the resource from the classpath.
      // getClass.getResource will return null if the resource is not found.
      val url = getClass.getResource(fileName)

      // If the URL is null, it means the resource was not found.
      // In this case, we throw an IOException to propagate the failure
      // within the ZIO effect.
      Option(url) match {
        case Some(u) => Path.fromJava(java.nio.file.Paths.get(u.toURI))
        case None =>
          throw new IOException(s"Resource '$fileName' not found on classpath.")
      }
    }

    // Chain the effects:
    // 1. Get the ZIO Path, refining any Throwable to IOException.
    // 2. Read all bytes from the obtained path.
    // 3. Convert the byte Chunk to a String using UTF-8 encoding.
    for
      zioPath <- zioPathEffect.refineToOrDie[IOException]
      content <- Files
        .readAllBytes(zioPath)
        .map(chunk => new String(chunk.toArray, StandardCharsets.UTF_8))
    yield content

  /** Reads all lines from a resource file from the classpath into a `Seq` of
    * `String`.
    *
    * This method expects the file to be located in the `src/main/resources`
    * directory. Each element in the resulting `Seq` corresponds to a single
    * line from the file. Line terminators (e.g., '\n', '\r\n') are not included
    * in the returned strings.
    *
    * The operation is wrapped in a ZIO effect, handling potential
    * `IOException`s that might occur during file access (e.g., file not found,
    * permissions issues).
    *
    * @param fileName
    *   The name of the resource file to read (e.g., "/input.txt"). Must include
    *   a leading slash if the file is at the root of the resources directory.
    * @return
    *   A `ZIO` effect that, when executed, will yield a `Seq[String]`
    *   containing all lines of the file, or fail with an `IOException` if the
    *   file cannot be read or found.
    */
  def readStrings(fileName: String): IO[IOException, Seq[String]] =
    val zioPathEffect: IO[Throwable, Path] = ZIO.attempt {
      // Attempt to get the URL of the resource from the classpath.
      // getClass.getResource will return null if the resource is not found.
      val url = getClass.getResource(fileName)

      // If the URL is null, it means the resource was not found.
      // In this case, we throw an IOException to propagate the failure
      // within the ZIO effect.
      Option(url) match {
        case Some(u) => Path.fromJava(java.nio.file.Paths.get(u.toURI))
        case None =>
          throw new IOException(s"Resource '$fileName' not found on classpath.")
      }
    }

    for
      zioPath <- zioPathEffect.refineToOrDie[IOException]
      lines <- Files.readAllLines(zioPath, Charset.defaultCharset)
    yield lines
