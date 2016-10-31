object Helpers{
  import Syntax._

  def tracePrefix(t: Trace, i: Int): Trace = t.take(i+1)

  def getPublicMem(m: Memory, gamma: SecEnv): Memory = m.toList match {
    case Nil => Map()
    case (kv@(x, v)) :: rest => gamma.get(x) match {
      case None => throw new RuntimeException("unexpected: event $x in memory, but not in gamma")
      case Some(LatticeElt(P, _)) => (kv :: getPublicMem(rest.toMap, gamma).toList).toMap
      case Some(LatticeElt(S, _)) => getPublicMem(rest.toMap, gamma)
    }
  }

  def getLowEvents(t: Trace, gamma: SecEnv): Trace = t match {
    case Nil => Nil
    case (e@AssignEvent(x, v)) :: rest => gamma.get(x) match {
      case None => throw new RuntimeException("unexpected: event $x=$v recorded in trace, but not in gamma")
      case Some(LatticeElt(P, _)) => e :: getLowEvents(rest, gamma)
      case Some(LatticeElt(S, _)) => getLowEvents(rest, gamma)
    }
    case EmptyEvent :: rest => EmptyEvent :: getLowEvents(rest, gamma)
    case TermEvent :: rest => TermEvent :: getLowEvents(rest, gamma)
  }


}