package set2.c9

object C9 {
  def implementPKCS7Padding(message: String, desiredLength: Int): String = {
    val numBytes = message.getBytes("UTF-8")
    val diff = desiredLength - numBytes.length
    if (diff <= 0){
      message
    }
    else {
      message + "\\x04" * diff
    }
  }
}