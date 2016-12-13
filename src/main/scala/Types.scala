object Types {

  import Syntax._
  import Helpers._

  def vars(e: Expr): Set[Id] = e match {
    case EConst(_) => Set()
    case EVar(x) => Set(x)
    case EOp1(_, e) => vars(e)
    case EOp2(_, e1, e2) => vars(e1) union vars(e2)
    case EDeclassify(e) => vars(e)
  }


  def typeCheckExpr(gamma: SecEnv, e: Expr): (LatticeElt, Set[Id]) = e match {
    case EConst(c) => (LatticeElt(P, T), Set()) //???
    case EVar(x) => (gamma.getOrElse(x, throw new RuntimeException("$x not bound in gamma")), Set())
    case EOp1(op, e) => typeCheckExpr(gamma, e)
    case EOp2(op, e1, e2) => {
      val (l1, d1) = typeCheckExpr(gamma, e1)
      val (l2, d2) = typeCheckExpr(gamma, e2)
      (l1 join l2, d1 union d2)
    }
    case EDeclassify(e) => {
      val (l, d) = typeCheckExpr(gamma, e)
      (l meet LatticeElt(P, U), vars(e))
    }
  }

  def typeCheckComm(gamma: SecEnv, pc: LatticeElt, c: Comm): Boolean = c match {
    case CSkip => true
    case CSeq(c1, c2) => typeCheckComm(gamma, pc, c1) && typeCheckComm(gamma, pc, c2)
    case CAssign(x, e) => {
      val (l, d) = typeCheckExpr(gamma, e)

      val checkL = (l join pc) <= gamma.getOrElse(x, throw new RuntimeException(s"$x, not bound in gamma"))

      if(d == Set())
        checkL && d.forall(y => gamma.getOrElse(y, throw new RuntimeException(s"$y not bound in gamma")) <= LatticeElt(S, T))
      else
        pc <= LatticeElt(P, T) && checkL && d.forall(y => gamma.getOrElse(y, throw new RuntimeException(s"$y not bound in gamma")) <= LatticeElt(S, T))
    }
    case CITE(e, c1, c2) => {
      val (l, d) = typeCheckExpr(gamma, e)
      d == Set() && typeCheckComm(gamma, pc join l, c1) && typeCheckComm(gamma, pc join l, c2)
    }
    case CWhile(e, c) => {
      val (l, d) = typeCheckExpr(gamma, e)
      d == Set() && typeCheckComm(gamma, pc join l, c)
    }
    case CClass(_, body) => typeCheckComm(gamma, pc, body)
    case CNew(c, args) => {
      val (ls, ds) = args.map(typeCheckExpr(gamma, _)).unzip
      typeCheckComm(gamma, pc join ls.foldRight(latticeMin)((l, acc) => l join acc), c) && ds.forall(d => d == Set()) //??? is the join right? is it in the right order?
    }
    case CEndorse(x, e) => {
      val xLevel = gamma.getOrElse(x, throw new RuntimeException(s"$x not bound in gamma"))
      val (l, _) = typeCheckExpr(gamma, e)
      (pc <= LatticeElt(S, T)) && (pc <= xLevel) && ((l meet LatticeElt(S, T)) <= xLevel)
    }
  }

}