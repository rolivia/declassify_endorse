I have implemented the type system for [A Semantic Framework for Declassification and Endorsement](https://www.cs.cornell.edu/andru/papers/robknowledge.pdf).

Requirements:
Must have scala/sbt installed.

To run existing tests:
1. Run sbt from the project root
2. `compile`
3. `test`

To write new tests:
Edit either TypeTests.scala, SyntaxTests.scala, or write a new test suite.

I recommend writing tests similar to those in TypeTests.scala while also messing with the `gamma` and `pc` variables
to get different results from the type checker.