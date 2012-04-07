//
// lisp.scala
// Copyright (c) 2012 Cesar Lopez-Nataren (cesar@ciencias.unam.mx)
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to
// do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
// IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import scala.util.parsing.combinator._

class LispParser extends RegexParsers {
	def expr: Parser[Any] = symbol | "("~>rep1(expr, expr)<~")"
	def symbol: Parser[Any] = """\w+""".r | """\d+""".r
}

class LispInterpreter {
	def eval(program: List[Any], Map[String, Any]) {
		
	}
}

object LispParser extends LispParser {
	def main(args: Array[String]) {
		println("input: " + args(0))
		println(parseAll(Expr, args(0)))
	}
}