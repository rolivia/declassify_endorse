object Semantics{
  import Syntax._

  def doUnOp(op: Op1, v: Value): Value = (op, v) match {
    case (Not, VConst(Bool(b))) => VConst(Bool(!b))
    case (Neg, VConst(Num(n))) => VConst(Num(-n))
    case _ => throw new RuntimeException("type error: got $v as argument to $op")
  }


  def doBinOp(op: Op2, v1: Value, v2: Value): Value = (op, v1, v2) match {
    case (Add, VConst(Num(n1)), VConst(Num(n2))) => VConst(Num(n1 + n2))
    case (Sub, VConst(Num(n1)), VConst(Num(n2))) => VConst(Num(n1 - n2))
    case (Mul, VConst(Num(n1)), VConst(Num(n2))) => VConst(Num(n1 * n2))
    case (Div, VConst(Num(n1)), VConst(Num(n2))) => VConst(Num(n1 / n2))
    case (Add, VConst(Bool(b1)), VConst(Bool(b2))) => VConst(Bool(b1 && b2))
    case (Or, VConst(Bool(b1)), VConst(Bool(b2))) => VConst(Bool(b1 || b2))
    case _ => throw new RuntimeException("type error: got $v1 and $v2 as arguments to $op")
  }

  def evalExpr(e: Expr, m: Memory): Value = e match {
    case EConst(c) => VConst(c)
    case EVar(x) => m.getOrElse(x, throw new RuntimeException("unbound variable: $x does not exist in memory"))
    case EOp1(op, e) => doUnOp(op, evalExpr(e, m))
    case EOp2(op, e1, e2) => doBinOp(op, evalExpr(e1, m), evalExpr(e2, m))
  }

  def evalComm(c: Comm, m: Memory, t: Trace): (Memory, Trace) = c match {
    case CSkip => (m, EmptyEvent :: t)
    case CAssign(x, e) => (m + (x -> evalExpr(e, m)), AssignEvent(x, evalExpr(e, m)) :: t)
    case CSeq(c1, c2) => {
      val (m1, t1) = evalComm(c1, m, t)
      evalComm(c2, m1, t1)
    }
    case CITE(e, c1, c2) => evalExpr(e, m) match {
      case VConst(Bool(true)) => evalComm(c1, m, t)
      case VConst(Bool(false)) => evalComm(c2, m, t)
      case x => throw new RuntimeException("type error: got $x in if condition")
    }
    case CWhile(e, c) => evalExpr(e, m) match {
      case VConst(Bool(true)) => {
        val (m1, t1) = evalComm(c, m, t)
        evalComm(CWhile(e, c), m1, t1)
      }
      case VConst(Bool(false)) => evalComm(CSkip, m, t)
      case x => throw new RuntimeException("type error: got $x in loop guard")
    }
  }


  def attackerKnowledge(c: Comm, mP: Memory, l: Trace): Set[Memory] = ???

}
