class SyntaxTests extends org.scalatest.FunSuite{

  import Syntax._

  test("lt - secrecy levels"){
    assert(P < S)
    assert(!(S < P))
    assert(!(S < S))
    assert(!(P < P))
  }

  test("leq - secrecy levels"){
    assert(P <= S)
    assert(!(S <= P))
    assert(S <= S)
    assert(P <= S)
  }

  test("lt - integrity levels"){
    assert(T < U)
    assert(!(U < T))
    assert(!(U < U))
    assert(!(T < T))
  }

  test("leq - integrity levels"){
    assert(T <= U)
    assert(!(U <= T))
    assert(U <= U)
    assert(T <= T)
  }

  test("leq - lattice elements"){
    assert(LatticeElt(S, U) <= LatticeElt(S, U))
    assert(!(LatticeElt(S, U) <= LatticeElt(S, T)))
    assert(!(LatticeElt(S, U) <= LatticeElt(P, U)))
    assert(!(LatticeElt(S, U) <= LatticeElt(P, T)))

    assert(LatticeElt(P, U) <= LatticeElt(S, U))
    assert(!(LatticeElt(P, U) <= LatticeElt(S, T)))
    assert(LatticeElt(P, U) <= LatticeElt(P, U))
    assert(!(LatticeElt(P, U) <= LatticeElt(P, T)))

    assert(LatticeElt(S, T) <= LatticeElt(S, U))
    assert(LatticeElt(S, T) <= LatticeElt(S, T))
    assert(!(LatticeElt(S, T) <= LatticeElt(P, U)))
    assert(!(LatticeElt(S, T) <= LatticeElt(P, T)))

    assert(LatticeElt(P, T) <= LatticeElt(S, U))
    assert(LatticeElt(P, T) <= LatticeElt(S, T))
    assert(LatticeElt(P, T) <= LatticeElt(P, U))
    assert(LatticeElt(P, T) <= LatticeElt(P, T))
  }

  test("join"){
    assert((LatticeElt(S, U) join LatticeElt(S, U)) == LatticeElt(S, U))
    assert((LatticeElt(S, U) join LatticeElt(P, U)) == LatticeElt(S, U))
    assert((LatticeElt(S, U) join LatticeElt(S, T)) == LatticeElt(S, U))
    assert((LatticeElt(S, U) join LatticeElt(P, T)) == LatticeElt(S, U))

    assert((LatticeElt(P, T) join LatticeElt(S, U)) == LatticeElt(S, U))
    assert((LatticeElt(P, T) join LatticeElt(P, U)) == LatticeElt(P, U))
    assert((LatticeElt(P, T) join LatticeElt(S, T)) == LatticeElt(S, T))
    assert((LatticeElt(P, T) join LatticeElt(P, T)) == LatticeElt(P, T))

    assert((LatticeElt(S, T) join LatticeElt(S, U)) == LatticeElt(S, U))
    assert((LatticeElt(S, T) join LatticeElt(P, U)) == LatticeElt(S, U))
    assert((LatticeElt(S, T) join LatticeElt(S, T)) == LatticeElt(S, T))
    assert((LatticeElt(S, T) join LatticeElt(P, T)) == LatticeElt(S, T))

    assert((LatticeElt(P, U) join LatticeElt(S, U)) == LatticeElt(S, U))
    assert((LatticeElt(P, U) join LatticeElt(P, U)) == LatticeElt(P, U))
    assert((LatticeElt(P, U) join LatticeElt(S, T)) == LatticeElt(S, U))
    assert((LatticeElt(P, U) join LatticeElt(P, T)) == LatticeElt(P, U))
  }

  test("meet"){
    assert((LatticeElt(S, U) meet LatticeElt(S, U)) == LatticeElt(S, U))
    assert((LatticeElt(S, U) meet LatticeElt(P, U)) == LatticeElt(P, U))
    assert((LatticeElt(S, U) meet LatticeElt(S, T)) == LatticeElt(S, T))
    assert((LatticeElt(S, U) meet LatticeElt(P, T)) == LatticeElt(P, T))

    assert((LatticeElt(P, T) meet LatticeElt(S, U)) == LatticeElt(P, T))
    assert((LatticeElt(P, T) meet LatticeElt(P, U)) == LatticeElt(P, T))
    assert((LatticeElt(P, T) meet LatticeElt(S, T)) == LatticeElt(P, T))
    assert((LatticeElt(P, T) meet LatticeElt(P, T)) == LatticeElt(P, T))

    assert((LatticeElt(S, T) meet LatticeElt(S, U)) == LatticeElt(S, T))
    assert((LatticeElt(S, T) meet LatticeElt(P, U)) == LatticeElt(P, T))
    assert((LatticeElt(S, T) meet LatticeElt(S, T)) == LatticeElt(S, T))
    assert((LatticeElt(S, T) meet LatticeElt(P, T)) == LatticeElt(P, T))

    assert((LatticeElt(P, U) meet LatticeElt(S, U)) == LatticeElt(P, U))
    assert((LatticeElt(P, U) meet LatticeElt(P, U)) == LatticeElt(P, U))
    assert((LatticeElt(P, U) meet LatticeElt(S, T)) == LatticeElt(P, T))
    assert((LatticeElt(P, U) meet LatticeElt(P, T)) == LatticeElt(P, T))
  }


}