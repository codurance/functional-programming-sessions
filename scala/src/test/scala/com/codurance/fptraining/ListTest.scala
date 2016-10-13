package com.codurance.fptraining

import org.scalatest._
import com.codurance.fptraining.List._

class ListTest extends FlatSpec with Matchers {
    "List with no elements" should "be Nil" in {
        List() should be(Nil)
    }

    "List with one element" should "be Cons(element, Nil)" in {
        List(1) should be(Cons(1, Nil))
    }

    "List with two elements" should "be Cons(element1, Cons(element2, Nil))" in {
        List(1, 2) should be(Cons(1, Cons(2, Nil)))
    }

    "Sum for empty numeric list" should "return 0" in {
        sum(List()) should be(0)
    }

    "Sum for numeric list" should "return the sum of the elements" in {
        sum(List(1, 2)) should be(3)
    }

    "Product for empty numeric list" should "return 1" in {
        product(List()) should be(1)
    }

    "Product for numeric list" should "return the product of the elements" in {
        product(List(5, 10)) should be(50)
    }

    "Tail of empty List" should "return Nil" in {
        tail(List()) should be(Nil)
    }

    "Tail of list of one element" should "return Nil" in {
        tail(List(1)) should be(Nil)
    }

    "Tail of list" should "return everything but the head" in {
        tail(List(1, 2)) should be(List(2))
    }

    "Drop 0 elements from list" should "return the list" in {
        drop(List(1), 0) should be(List(1))
    }

    "Drop 1 element from empty list" should "return an empty list" in {
        drop(List(), 1) should be(List())
    }

    "Drop 1 element from a list of 1 element" should "return an empty list" in {
        drop(List(54), 1) should be(List())
    }

    "Drop n elements from a list" should "return the list minus the first n elements" in {
        drop(List(54, 34, 2), 2) should be(List(2))
    }

    "Drop while of an empty list" should "return empty list" in {
        dropWhile(List())(_ => false) should be(List())
    }

    "Drop while of a list" should "remove elements until they don't match the predicate" in {
        dropWhile(List(1, 1, 1, 5, 1))(x => x == 1) should be(List(5, 1))
    }

    "Set head of an empty list" should "return a list with the head" in {
        setHead(List())(3) should be(List(3))
    }

    "Set head of a list" should "replace the head of the list" in {
        setHead(List(1, 2))(3) should be(List(3, 2))
    }
}