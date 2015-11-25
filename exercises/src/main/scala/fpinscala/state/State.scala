// package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def intRng(seed: Long) = {
    Simple(seed).nextInt
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    // val max = Int.MaxValue
    val (n, rng2) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, r) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), r)
  }

  def double2(): Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def gen(c: Int)(r: RNG)(acc: List[Int]): (List[Int], RNG) = {
      if (c == 0) (acc, r)
      else {
        val (n, r1) = r.nextInt
        gen(c - 1)(r1)(n :: acc)
      }
    }
    gen(count)(rng)(List())
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def concat[A](l: List[Rand[A]], rng: RNG, acc: List[A]): (List[A], RNG) = {
    l match {
      case h :: t => {
        val (n, r) = h(rng)
        concat(t, r, (n :: acc))
      }
      case Nil => (acc, rng)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    // rng => { concat(fs, rng, List()) }
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
  }

  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (n, r1) = f(rng)
      val (m, r2) = g(n)(r1)
      (m, r2)
    }
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def _map2[A, B, C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      a => val mod = a % n
      if (a + (n - 1) - mod > 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    // State(s => {
    //   val (a, s1) = run(s)
    //   (f(a), s1)
    // })
    // flatMap(a => State(s => (f(a), s)))
    flatMap(a => State.unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    // State(s => {
    //   val (a, s1) = run(s)
    //   val (b, s2) = sb.run(s1)
    //   (f(a, b), s2)
    // })
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = {
    State(s => (a, s))
  }

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    // l.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _) )
    // l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

    def go(state: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) = {
      actions match {
        case Nil => (acc, state)
        case h :: t => h.run(state) match {
          case (a, s) => go(s, t, a :: acc)
        }
      }
    }
    State(s => go(s, l, List()))
  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    // get.flatMap(s => set(f(s)).map())
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

import State._

object Candy {
  def update: Input => (Machine => Machine) =
    (i: Input) => (s: Machine) => (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
  }


  // Candy.simulateMachine(inputs).run
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    // sequence(inputs.map(a => modify(update(a)))).
    //   flatMap(_ => get.map(s => (s.coins, s.candies)))
    for {
      _ <- sequence(inputs.map(a => modify(update(a))))
      s <- get
    } yield (s.coins, s.candies)
  }
}

val (n, r) = RNG.intRng(1)
