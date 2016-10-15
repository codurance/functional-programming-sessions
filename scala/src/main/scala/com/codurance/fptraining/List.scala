package com.codurance.fptraining

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] = {
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    }

    def sum(list: List[Int]): Int = {
        @tailrec
        def go(list: List[Int], acc: Int): Int = {
            list match {
                case Nil => acc
                case Cons(h, t) => go(t, acc + h)
            }
        }
        go(list, 0)
    }

    def product(list: List[Int]): Int = {
        @tailrec
        def go(list: List[Int], acc: Int): Int = {
            list match {
                case Nil => acc
                case Cons(h, t) => go(t, acc * h)
            }
        }
        go(list, 1)
    }

    def tail[A](list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(h, t) => t
    }

    def drop[A](list: List[A], count: Int): List[A] = count match {
        case 0 => list
        case _ => drop(tail(list), count - 1)
    }

    def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) dropWhile(t)(f) else list
    }

    def setHead[A](list: List[A])(h: A): List[A] = list match {
        case Nil => List(h)
        case Cons(_, t) => Cons(h, t)
    }

    def init[A](list: List[A]): List[A] = list match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }
}