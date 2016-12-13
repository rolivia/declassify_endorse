class TypeTests extends org.scalatest.FunSuite{

  import Syntax._
  import Types._

  test("test1"){
    val gamma = Map(("x" -> LatticeElt(S, T)), ("y" -> LatticeElt(S, T)))
    val c = CITE(EVar("x"), CAssign("y", EConst(Num(1))), CAssign("y", EConst(Num(2))))
    val pc = LatticeElt(S, T)
    assert(typeCheckComm(gamma, pc, c))
  }

  test("test2"){
    val gamma = Map(("x" -> LatticeElt(S, T)), ("y" -> LatticeElt(P, T)))
    val c = CITE(EVar("x"), CAssign("y", EConst(Num(1))), CAssign("y", EConst(Num(2))))
    val pc = LatticeElt(S, T)
    assert(!typeCheckComm(gamma, pc, c))
  }

  test("test3"){
    val gamma = Map(("x" -> LatticeElt(S, T)), ("y" -> LatticeElt(S, T)))
    val c = CSeq(CITE(EVar("x"), CAssign("y", EConst(Num(1))), CAssign("y", EConst(Num(2)))), CAssign("x", EConst(Bool(false))))
    val pc = LatticeElt(S, T)
    assert(typeCheckComm(gamma, pc, c))
  }

  test("test4"){
    val gamma = Map(("x" -> LatticeElt(P, T)), ("y" -> LatticeElt(S, T)))
    val c = CSeq(CITE(EVar("x"), CAssign("y", EConst(Num(1))), CAssign("y", EConst(Num(2)))), CAssign("x", EConst(Bool(false))))
    val pc = LatticeElt(P, T)
    assert(typeCheckComm(gamma, pc, c))
  }

  test("test5"){
    val gamma = Map(("x" -> LatticeElt(S, T)), ("y" -> LatticeElt(P, T)))
    val c = CAssign("y", EDeclassify(EVar("x")))
    val pc = LatticeElt(P, T)
    assert(typeCheckComm(gamma, pc, c))
  }

  test("test6"){
    val gamma = Map(("x" -> LatticeElt(P, U)), ("y" -> LatticeElt(P, T)))
    val c = CEndorse("y", EVar("x"))
    val pc = LatticeElt(P, T)
    assert(typeCheckComm(gamma, pc, c))
  }

}