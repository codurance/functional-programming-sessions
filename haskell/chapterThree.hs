module ChapterThree where

import Test.Hspec
import Text.Printf (printf)
import Data.List


data List' a = Nil
             | Cons a (List' a) deriving (Show, Eq)

instance Foldable List' where
    foldr function accumulator Nil = accumulator 
    foldr function accumulator (Cons x xs) = function x (foldr function accumulator xs)

length' :: List' a -> Int
length' Nil = 0
length' (Cons x xs) = 1 + length' xs

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

map' :: (a -> b) -> List' a -> List' b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

map'' :: (a -> b) -> List' a -> List' b
map'' f xs = foldl (\acc x -> (acc `appendEnd` f x)) Nil xs

filter' :: (a -> Bool) -> List' a -> List' a
filter' f Nil = Nil
filter' f (Cons x xs) = if (f x) then (Cons x (filter' f xs)) else (filter' f xs)

take' :: Int -> List' a -> List' a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

flatMap' :: List' a -> (a -> List' b) -> List' b
flatMap' xs f = foldl (\acc x -> (acc `concat'` f x)) Nil xs 

concat' :: List' a -> List' a -> List' a
concat' Nil new = new
concat' (Cons x xs) new = Cons x (concat' xs new)

sumTwoLists' :: Num a => List' a -> List' a -> List' a
sumTwoLists' x Nil = x
sumTwoLists' Nil x = x
sumTwoLists' (Cons x xs) (Cons y ys) = (Cons (x + y) (sumTwoLists' xs ys))
    

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

    describe "map'" $ do
        it "applies the function to every element, keeping the order" $ do
            ((map' id (apply' [1,2])) `shouldBe` apply' [1,2])
            ((map' id (apply' [1])) `shouldBe` apply' [1])
            ((map'' id (apply' [1,2])) `shouldBe` apply' [1,2])
            ((map'' id (apply' [1])) `shouldBe` apply' [1])

    describe "filter' - keeps the elements which satisfy the predicate" $ do
        it "from a finite list" $ do
            ((filter' odd (apply' [1,2,3,4,5])) `shouldBe` apply' [1,3,5])
            ((filter' odd (apply' [])) `shouldBe` apply' [])
        
        it "from an infinite list" $ do
            (take' 3 (filter' odd (apply' [1..])) `shouldBe` apply' [1,3,5])

    describe "take' takes the specified number of elements, possibly from an infinite list" $ do
        it "from an empty list" $ do
            (length' (take' 2 $ apply' [])) `shouldBe` 0;
            (length' (take' 0 $ apply' [])) `shouldBe` 0;

        it "from a non-empty list" $ do
            ((take' 2 $ apply' [1,3,5]) `shouldBe` apply' [1,3])
            ((take' 0 $ apply' [1,3,5]) `shouldBe` apply' [])
        
        it "from an infinite list" $ do
            ((take' 2 $ apply' [1..]) `shouldBe` apply' [1,2])
            ((take' 0 $ apply' [1..]) `shouldBe` apply' [])

    describe "flatMap - flattens a list of lists into a list" $ do
        let rep x = Cons x (Cons x Nil);
        let rep' n = (apply' . replicate n)
        -- equivalent to
        -- let rep = apply' . replicate 2

        it "empty list" $ do
            length' (flatMap' Nil rep) `shouldBe` 0;

        it " list" $ do
            (flatMap' (apply' [1,2,3]) rep) `shouldBe` apply' [1,1,2,2,3,3];
            (flatMap' (apply' [1,2,3]) (rep' 3)) `shouldBe` apply' [1,1,1,2,2,2,3,3,3];

    describe "sumTwoLists - accepts two lists and sums its elements" $ do
        it "empty list" $ do
            length' (sumTwoLists' Nil Nil) `shouldBe` 0;

        it "one empty list and the other non-empty" $ do
            (sumTwoLists' (apply' [1,2,3]) Nil) `shouldBe` apply' [1,2,3];
            (sumTwoLists' Nil (apply' [1,2,3])) `shouldBe` apply' [1,2,3];

        it "both non-empty lists" $ do
            (sumTwoLists' (apply' [1,2,3]) (apply' [1,2,3])) `shouldBe` apply' [2,4..6];
