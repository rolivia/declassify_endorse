object Attacker {
  import Syntax._

  sealed trait Attack
  case object ASkip extends Attack
  case class AAssign(x: Id, e: Expr) extends Attack
  case class ASeq(a1: Attack, a2: Attack) extends Attack
  case class AITE(e: Expr, a1: Attack, a2: Attack) extends Attack
  case class AWhile(e: Expr, a: Attack) extends Attack
}