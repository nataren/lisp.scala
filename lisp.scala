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
import scala.util.control.Exception._

abstract class Expr
case class Exprs(head:Expr, tail: Expr) extends Expr
case class Symbol(name: String) extends Expr
case class Number(num: Double) extends Expr

class Env(p: Env) extends HashMap[Symbol, Expr] {
	val parent = p
	def find(e: Expr) {
		getOrElse(e, parent.find(e))
	}
}
class UndefinedException extends Exception

class LispParser extends RegexParsers {
	def expr: Parser[Expr] = symbol | ("("~>rep1(expr, expr)<~")" ^^ { x => expand(x) })
	def symbol: Parser[Expr] = ("""\d+""".r ^^ { x => Number(x.toDouble) })	| ("""\w+""".r ^^ { x => Symbol(x) })
	def parse(program: String): Expr = parseAll(expr, program).get
	
	// utility functions
	def expand(l: List[Expr]): Expr = {
		if(l.length == 1)
			Exprs(l.head, Symbol("nil"))
		else Exprs(l.head, expand(l.tail))
	}
}

class LispInterpreter {
	def atom(expr: Expr): Boolean = expr match {
		case Number(_) => true
		case Symbol(_) => true
		case _ => false
	}
	def eq(x: Expr, y: Expr): Boolean = atom(x) && atom(y) && x == y
	def car(expr: Expr): Expr = expr match {
		case Exprs(head, _) => head
		case _ => throw new UndefinedException
	}
	def cdr(expr: Expr): Expr = expr match {
		case Exprs(head, tail) => tail
		case _ => throw new UndefinedException
	}
	def cons(x: Expr, y: Expr): Expr = Exprs(x, y)
	def ff(expr: Expr): Expr = expr match {
		case Exprs(head, tail) => ff(head)
		case _ => expr
	}
	def subst(x: Expr, y: Expr, z: Expr): Expr =
		if(atom(z))
		 	if(eq(z, y))
				x
			else z
		else cons(subst(x, y, car(z)), subst(x, y, cdr(z)))
	def equal(x: Expr, y: Expr): Boolean =
		((atom(x) && atom(y) && eq(x, y)) ||
		 !atom(x) && !atom(y) && equal(car(x), car(y)) && equal(cdr(x), cdr(y)))
	def nulll(x: Expr): Boolean = atom(x) && eq(x, Symbol("nil"))
	def append(x: Expr, y: Expr): Expr = if(nulll(x)) y else cons(car(x), append(cdr(x), y))
	def eval(expr: Expr): Any = eval(expr, Env(null))
	def eval(expr: Expr, env: Env): Any = expr match {
		case Number(n) => n
		case Symbol(s) => env.find(expr)
		case _ => {
			val cr = car(expr)
			if(atom(cr)) {
				if(eq(cr, Symbol('quote')))
					car(cdr(expr))
				else if(eq(cr, Symbol('atom')))
					atom(eval(car(cdr(expr)), env))
				else if(eq(cr, Symbol('eq')))
					eval(car(cdr(expr)), env) == eval(car(cdr(cdr(expr))), env)
				else if(eq(cr, Symbol('cond')))
					evalCond(cdr(expr), env)
				else if(eq(cr, Symbol('car')))
					car(eval(car(cdr(expr)), env))
				else if(eq(cr, Symbol('cdr')))
					cdr(eval(car(cdr(expr)), env))
				else if(eq(cr, Symbol('cons')))
					cons(eval(car(cdr(expr)), env), eval(car(cdr(cdr(expr))), env))
			} else if(eq(car(car(expr)), Symbol('label'))) {
				eval(cons(car(cdr(cdr(car(expr)))), cdr(expr)), env += car(cdr(car(expr))) -> car(expr)))
			} else if(eq(car(car(expr)), Symbol('lambda'))) {
				eval(car(cdr(cdr(car(expr)))), )
			}
		}
	}
	def evalCond(cond: Expr, env: Env) {
		if(eval(car(car(cond)))) {
			eval(car(cdr(car(cond))), env)
		} else {
			evalCond(cdr(cond), env)
		}
	}
	def evList(expr: Expr) {
		
	}
}