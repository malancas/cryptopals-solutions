package c9

object C9 {
  def implementPKCS7Padding(message: String, desiredLength: Int): String = {
    val diff = desiredLength - message.length
    if (diff <= 0){
      message
    }
    else {
      message + "\\x04" * diff
    }
  }
}