package my.implementation

object FooMock {self =>

    val outerField: String = "suck"

  	case class InnerFoo(i: Int, s: String) {
      def hello(): String = s.toUpperCase
  		def world(): String =
        (for (i <- 0 until i*2)
        yield self.outerField.toUpperCase).mkString("")
  	}
}
