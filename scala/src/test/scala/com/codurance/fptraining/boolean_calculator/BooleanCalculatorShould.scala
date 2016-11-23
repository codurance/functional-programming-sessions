package com.codurance.fptraining.boolean_calculator

import com.codurance.fptraining.List._
import org.scalatest._

class BooleanCalculatorShould extends FlatSpec with Matchers {
    "NOT T" should "evaluate to False" in {
        BooleanCalculator.calculate("NOT T") should be(false)
    }

    "NOT F" should "evaluate to True" in {
        BooleanCalculator.calculate("NOT F") should be(true)
    }

    "T AND T" should "evaluate to True" in {
        BooleanCalculator.calculate("T AND T") should be(true)
    }

    "T AND F" should "evaluate to False" in {
        BooleanCalculator.calculate("T AND F") should be(false)
    }

    "F AND T" should "evaluate to False" in {
        BooleanCalculator.calculate("F AND T") should be(false)
    }

    "F AND F" should "evaluate to False" in {
        BooleanCalculator.calculate("F AND F") should be(false)
    }
}


