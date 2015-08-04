//package demo
//package unicredit
package demo.unicredit

import scala.scalajs.js.JSApp

import scala.scalajs.js

object Foo { self =>

	val outerField: String = " world"

	case class InnerFoo(i: Int, s: String) {
		def hello(): String = s
		def world(): String =
			(for (i <- 0 until i)
			yield self.outerField).mkString("")
	}

}

object Demo extends JSApp {

	def main(): Unit = {
		val fif = Foo.InnerFoo(1, "hello")
		println(fif.hello+fif.world())
		//the reslt will be "HELLOWORLDWORLD"
	}

}
