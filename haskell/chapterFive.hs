module ChapterFive where

import Test.Hspec
import Text.Printf (printf)

data Stream a = Empty
              | Cons a (Stream a) deriving (Show, Eq)

empty :: (a) -> Stream a
empty x = Empty

cons :: a -> Stream a -> Stream a
cons head tail = Cons head tail

apply' :: [a] -> Stream a
apply' [] = Empty
apply' (x:xs) = Cons x (apply' xs)

take' :: Int -> Stream a -> Stream a
take' 0 stream = Empty
take' n (Cons x xs) = Cons x (take' (n-1) xs)

takeWhile' :: (a -> Bool) -> Stream a -> Stream a
takeWhile' predicate Cons(x xs) = if predicate ( x) then takeWhile predicate
                                                  else Empty

toList :: Stream a -> [a]
toList Empty = []
toList (Cons a xs) = a:toList(xs)

main = hspec $ do
    describe "empty" $ do
        it (printf "return an empty stream") $
           empty () `shouldBe` Empty
--    describe "cons" $ do
--        it (printf "prepend a head to a stream") $
--           cons head tail `shouldBe` (Cons 1 (Cons 2 (Empty)))
--            where head = 1
--                  tail = (Cons 2 (Empty))
    describe "toList" $ do
        it (printf "return a list of a") $
           toList (Cons 1 Empty) `shouldBe` 1:[]
    describe "takeWhile'" $ do
        it (printf "return an empty stream when predicate false") $ do
           ((takeWhile' (\ x -> False) Empty)::Stream Int) `shouldBe` (Empty::Stream Int);
           ((takeWhile' (\ x -> False) (Cons 1 Empty))::Stream Int) `shouldBe` (Empty::Stream Int);