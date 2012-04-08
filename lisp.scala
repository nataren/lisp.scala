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
import scala.collection.mutable._

abstract class Expr
case class Exprs(members: List[Expr]) extends Expr
case class Symbol(name: String) extends Expr
case class Number(num: Double) extends Expr

class LispParser extends RegexParsers {
	def expr: Parser[Expr] = symbol | ("("~>rep1(expr, expr)<~")" ^^ { x => Exprs(x) })
	def symbol: Parser[Expr] = ("""\d+""".r ^^ { x => Number(x.toDouble) }) | ("""\w+""".r ^^ { x => Symbol(x) })
	def parse(program: String): Expr = parseAll(expr, program).get
}

class LispInterpreter {
	def atom(expr: Expr): Boolean = expr match {
		case Number(_) => true
		case Symbol(_) => true
		case _ => false
	}
	
	def eq(x: Expr, y: Expr): Boolean = atom(x) && atom(y) && x == y
	
	def eval(expr: Expr): Any = eval(expr, new HashMap[Symbol, Expr])
	
	def eval(expr: Expr, env: HashMap[Symbol, Expr]): Any = expr match {
		case Number(n) => n
		case _ =>
	}
}

val i = new LispInterpreter
val p = new LispParser
i.eq(p.parse("hola"), p.parse("hola"))
i.eq(p.parse("hola"), p.parse("hola1"))