import scala.reflect.io.File

object InputReader {
  def ReadInput(path: String): String = {
    val url = getClass().getResource("/" + path)
    File(url.getPath()).slurp()
  }
}
