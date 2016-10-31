object Types {

  import Syntax._
  import Helpers._


  def typeCheckExpr(gamma: SecEnv, e: Expr): (LatticeElt, Set[Id]) = e match {
    case EConst(c) => (LatticeElt(P, T), Set()) //???
    case EVar(x) => (gamma.getOrElse(x, throw new RuntimeException("$x not bound in gamma")), Set())
    case EOp1(op, e) => typeCheckExpr(gamma, e)
    case EOp2(op, e1, e2) => {
      val (l1, d1) = typeCheckExpr(gamma, e1)
      val (l2, d2) = typeCheckExpr(gamma, e2)
      (l1 join l2, d1 union d2)
    }
  }

  def typeCheckComm(gamma: SecEnv, pc: LatticeElt, c: Comm): Boolean = c match {
    case CSkip => true
    case CSeq(c1, c2) => typeCheckComm(gamma, pc, c1) && typeCheckComm(gamma, pc, c2)
    case CAssign(x, e) => {
      val (l, d) = typeCheckExpr(gamma, e)

      val checkL = (l join pc) <= gamma.getOrElse(x, throw new RuntimeException("$x, not bound in gamma"))

      if(d == Set())
        checkL && d.forall(y => gamma.getOrElse(y, throw new RuntimeException("$y not bound in gamma")) <= LatticeElt(S, T))
      else
        pc <= LatticeElt(P, T) && checkL && d.forall(y => gamma.getOrElse(y, throw new RuntimeException("$y not bound in gamma")) <= LatticeElt(S, T))
    }
    case CITE(e, c1, c2) => {
      val (l, d) = typeCheckExpr(gamma, e)
      d == Set() && typeCheckComm(gamma, pc join l, c1) && typeCheckComm(gamma, pc join l, c2)
    }
    case CWhile(e, c) => {
      val (l, d) = typeCheckExpr(gamma, e)
      d == Set() && typeCheckComm(gamma, pc join l, c)
    }
  }


  // def typeCheckExprRec(): Boolean = {

  // }

}