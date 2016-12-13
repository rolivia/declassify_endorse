object Syntax {

  type Id = String

  sealed trait Const
  case class Num(n: Int) extends Const
  case class Bool(b: Boolean) extends Const

  sealed trait Op1
  case object Not extends Op1
  case object Neg extends Op1

  sealed trait Op2
  case object Add extends Op2
  case object Sub extends Op2
  case object Mul extends Op2
  case object Div extends Op2
  case object And extends Op2
  case object Or extends Op2

  sealed trait Expr
  case class EConst(c: Const) extends Expr
  case class EVar(x: Id) extends Expr
  case class EOp1(op: Op1, e: Expr) extends Expr
  case class EOp2(op: Op2, e1: Expr, e2: Expr) extends Expr
  case class EDeclassify(e: Expr) extends Expr


  // sealed trait Attack
  sealed trait Comm
  case object CSkip extends Comm
  case class CAssign(x: Id, e: Expr) extends Comm
  case class CSeq(c1: Comm, c2: Comm) extends Comm
  case class CITE(e: Expr, c1: Comm, c2: Comm) extends Comm
  case class CWhile(e: Expr, c: Comm) extends Comm
  case class CClass(params: List[String], body: Comm) extends Comm
  case class CNew(c: CClass, args: List[Expr]) extends Comm
  case class CEndorse(x: Id, e: Expr) extends Comm

  sealed trait Value
  case class VConst(c: Const) extends Value
  case class VObj(fields: Map[String, Value]) extends Value

  sealed trait Level{
    def <[A<:Level](that: A): Boolean
    def <=[A<:Level](that: A): Boolean = (this < that) || (this == that)
  }

  sealed trait SecLevel extends Level{
    def <[SecLevel](that: SecLevel): Boolean = (this, that) match {
      case (P, S) => true
      case _ => false
    }
  }
  case object P extends SecLevel
  case object S extends SecLevel

  sealed trait IntegLevel extends Level{
    def <[IntegLevel](that: IntegLevel): Boolean = (this, that) match {
      case (T, U) => true
      case _ => false
    }
  }
  case object T extends IntegLevel
  case object U extends IntegLevel

  val latticeMax = LatticeElt(S, U)
  val latticeMin = LatticeElt(P, T)

  case class LatticeElt(sec: SecLevel, integ: IntegLevel){
    def <=(l2: LatticeElt): Boolean = (this.sec <= l2.sec) && (this.integ <= l2.integ)

    def join(l2: LatticeElt): LatticeElt =
      if(this == l2) this
      else if (this <= l2) l2
      else if (l2 <= this) this
      else latticeMax

    def meet(l2: LatticeElt): LatticeElt =
      if(this == l2) this
      else if(this <= l2) this
      else if(l2 <= this) l2
      else latticeMin
  }

  sealed trait Event
  case class AssignEvent(x: Id, v: Value) extends Event
  case object EmptyEvent extends Event
  case object TermEvent extends Event

  type Trace = List[Event]

  type SecEnv = Map[Id, LatticeElt]

  type Memory = Map[Id, Value]

}