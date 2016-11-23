package com.codurance.fptraining.boolean_calculator

import org.scalatest._

class ExpressionShould extends FlatSpec with Matchers {
  "T" should "evaluate to value expression True" in {
    Expression.apply("T") should be(true)
  }
}
