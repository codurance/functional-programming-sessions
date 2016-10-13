module ChapterThree where

import Test.Hspec
import Text.Printf (printf)
import Data.List


data List' a = Nil
             | Cons a (List' a) deriving (Show, Eq)

head' :: List' a -> a
head' Nil = error "head of nothing???"
head' (Cons a _) = a

tail' :: List' a -> List' a
tail' Nil = error "tail of nothing???"
tail' (Cons _ xs) = xs

sum' :: List' Int -> Int
sum' Nil = 0
sum' (Cons x xs) = x + sum' xs

product' :: List' Int -> Int
product' Nil = 1
product' (Cons x xs) = x * product' xs

apply' :: [a] -> List' a
apply' [] = Nil
apply' (x:xs) = Cons x $ apply' xs


setHead' :: a -> List' a -> List' a
setHead' element Nil = Nil
setHead' newHead (Cons head tail) = (Cons newHead tail)

drop' :: Int -> List' a -> List' a
drop' 0 xs = xs
--drop' n xs = tail' (drop' (n-1) xs)
drop' n (Cons x xs) = drop' (n - 1) xs

main = hspec $ do
    describe "head" $ do
        it (printf "should return first element of list") $
            head' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 4

    describe "tail" $ do
        it (printf "should return tail of a list") $
            tail' (Cons 4 (Nil)) `shouldBe` Nil

    describe "sum" $ do
        it (printf "should sum a list") $
            sum' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 9

    describe "product" $ do
        it (printf "should multiply a list") $
            product' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 24

    describe "apply" $ do
        it (printf "should create our type of list from a list") $
            apply' [1..3] `shouldBe` (Cons 1 $ Cons 2 $ Cons 3 Nil)

    describe "setHead" $ do
        it (printf "should set the head with a new element") $
            setHead' 3 (apply' [1,2,3]) `shouldBe` (Cons 3 $ Cons 2 $ Cons 3 Nil)

    describe "drop'" $ do
        it ("should remove the first n element of our list") $
            drop' 2 (apply' [1..5]) `shouldBe` (Cons 3 $ Cons 4 $ Cons 5 Nil)