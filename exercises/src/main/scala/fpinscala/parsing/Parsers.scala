// package fpinscala.parsing

import scala.util.matching.Regex
import language.higherKinds
import language.implicitConversions

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  implicit def regex(r: Regex): Parser[String]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = {
    string(c.toString).map(_.charAt(0))
  }
  def orString(s1: String, s2: String): Parser[String] =
    string(s1) or string(s2)
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def many[A](p: Parser[A]): Parser[List[A]]
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _) or succeed(List())
  }
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    map(product(p, p2))(t => f(t._1, t._2))
  }
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def productViaFlatMap[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => p2.map(b => (a, b)))

  def map2ViaFlatMap[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(p)(a => p2.map(b => f(a, b)))

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)


  case class ParserOps[A](p: Parser[A]) {
    def | [B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or [B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: List[Parser[A]]
    def map[B](f: A => B): Parser[B]
    def ** [B](p2: Parser[B]): Parser[(A, B)] =
      self.product(this.p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] =
      self.product(this.p, p2)
  }

  object Laws {
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
  }
}
