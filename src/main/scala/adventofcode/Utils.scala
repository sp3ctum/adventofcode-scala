import scala.reflect.io.File

object InputReader {
  def ReadInput(path: String): String = {
    val url = getClass().getResource("/" + path)
    File(url.getPath()).slurp()
  }
}

object Timer {
  // http://biercoff.com/easily-measuring-code-execution-time-in-scala
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }
}
