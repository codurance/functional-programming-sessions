package com.codurance.fptraining.boolean_calculator

object BooleanCalculator {
    def calculate(expression: String) = expression match {
        case "NOT F" => true
        case "NOT T" => false
        case "T AND T" => true
        case "T AND F" => false
        case "F AND T" => false
        case "F AND F" => false
    }
}
