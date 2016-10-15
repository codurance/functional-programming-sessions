module ChapterThree where

import Test.Hspec
import Text.Printf (printf)
import Data.List


data List' a = Nil
             | Cons a (List' a) deriving (Show, Eq)

instance Foldable List' where
    foldr function accumulator Nil = accumulator 
    foldr function accumulator (Cons x xs) = function x (foldr function accumulator xs)

head' :: List' a -> a
head' Nil = error "head of nothing???"
head' (Cons a _) = a

tail' :: List' a -> List' a
tail' Nil = error "tail of nothing???"
tail' (Cons _ xs) = xs

sum' :: List' Int -> Int
sum' Nil = 0
sum' (Cons x xs) = x + sum' xs

sum'' :: List' Int -> Int
sum'' = foldl' (+) 0 

product' :: List' Int -> Int
product' Nil = 1
product' (Cons x xs) = x * product' xs

product'' :: List' Int -> Int
product'' xs = foldl' (*) 1 xs

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

scanr' :: (a -> b -> b) -> b -> List' a -> List' b
scanr' _ acc Nil = (Cons acc Nil)
scanr' f acc (Cons x xs) = Cons (f x (head' accumulated)) accumulated
    where accumulated = scanr' f acc xs

last' :: List' a -> a
last' Nil = error "last of nothing???"
last' (Cons x Nil) = x
last' (Cons x xs) = last' xs

appendEnd :: List' a -> a -> List' a
appendEnd Nil y = (Cons y Nil)
appendEnd (Cons x xs) y = Cons x $ appendEnd xs y

scanl'' :: (a -> b -> b) -> b -> List' a -> List' b
scanl'' f init xs =  scanl''' init (Cons init Nil) f xs  where
    scanl''' _ current f Nil = current
    scanl''' acc current f (Cons x xs) = scanl''' acc' accumulated f xs where
        accumulated = current `appendEnd` acc'
        acc' = (f x acc)

main = hspec $ do
    describe "head" $ do
        it "should return first element of list" $
            head' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 4

    describe "tail" $ do
        it "should return tail of a list" $
            tail' (Cons 4 (Nil)) `shouldBe` Nil

    describe "sum" $ do
        it "should sum a list" $ do
            (sum' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 9)
            ((sum'' $ apply' [4,3,2]) `shouldBe` 9)

    describe "product" $ do
        it "should multiply a list" $ do
            (product' (Cons 4 (Cons 3 (Cons 2 (Nil)))) `shouldBe` 24)
            ((product'' $ apply' [4,3,2]) `shouldBe` 24)

    describe "apply" $ do
        it "should create our type of list from a list" $
            apply' [1..3] `shouldBe` (Cons 1 $ Cons 2 $ Cons 3 Nil)

    describe "setHead" $ do
        it "should set the head of a list with a new value" $
            setHead' 3 (apply' [1,2,3]) `shouldBe` (Cons 3 $ Cons 2 $ Cons 3 Nil)

    describe "drop'" $ do
        it "should remove the first n element of our list" $
            drop' 2 (apply' [1..5]) `shouldBe` (Cons 3 $ Cons 4 $ Cons 5 Nil)

    describe "foldr" $ do
        it "should return the default element on the empty list" $
            foldr (-) 0 (apply' []) `shouldBe` 0

        it "should loop over the elements from the right" $
            foldr (\ele acc -> acc - ele) 0 (apply' [1,2,3]) `shouldBe` -6

    describe "foldl" $ do
        it "should return the default element on the empty list" $
            foldl (-) 0 (apply' []) `shouldBe` 0

        it "should loop over the elements from the right" $
            foldl (\ele acc -> acc + ele) 0 (apply' [1,2,3]) `shouldBe` 6

    describe "scanr" $ do
        it "cumulates the results of executing the function" $ do
            ((scanr' (-) 0 $ apply' []) `shouldBe` apply' [0])
            ((scanr' (-) 0 $ apply' [1,2,3]) `shouldBe` apply' [2,-1,3,0])

    describe "scanl" $ do
        it "with an empty, it is just the initial value" $
            (scanl'' (+) 1 $  apply' []) `shouldBe`  apply' [1]
        
        it "cumulates the result of executing the function, from the left" $
            (scanl'' (+) 1 $  apply' [1,2,3]) `shouldBe`  apply' [1,2,4,7]

    describe "last'" $ do
        it "retrieves the last element from a List'" $ do
            ((last' $ apply' [1,2,3]) `shouldBe` 3)
            ((last' $ apply' [3]) `shouldBe` 3)

    describe "appendEnd" $ do
        it "appends a single element to a List', at the end" $ do
            ((appendEnd (apply' [1,2]) 3) `shouldBe` apply' [1,2,3])
            ((appendEnd (apply' [1]) 2) `shouldBe` apply' [1,2])

