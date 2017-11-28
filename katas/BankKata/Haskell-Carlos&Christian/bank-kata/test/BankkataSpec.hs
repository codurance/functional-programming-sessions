{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module BankkataSpec  (main, spec) where

import           BankAccount
import           Control.Monad
import           Data.IORef
import           Data.Time
import           Debug.Trace
import           Test.Hspec
-- Given a client makes a deposit of 1000 on 10-01-2012
-- And a deposit of 2000 on 13-01-2012
-- And a withdrawal of 500 on 14-01-2012
-- When she prints her bank statement
-- Then she would see
-- date || credit || debit || balance
-- 14/01/2012 || || 500.00 || 2500.00
-- 13/01/2012 || 2000.00 || || 3000.00
-- 10/01/2012 || 1000.00 || || 1000.00

-- TODO: Lenses to avoid pattern matching in multiple places
-- TODO: Add Logger of transactions
-- TODO: Add storage

spec :: Spec
spec = do
  describe "Bank Account" $ do

    it "Given a positive amount and an account, when adding a deposit, then adds deposit to account" $ do
      makeDeposit' 100 emptyAccount `shouldBe` Just (BankAccount [Deposit aDay (Amount 100)])

    it "Given a positive amount and an account, when adding a withdraw, then adds withdraw to account" $ do
      makeWithdraw' 100 emptyAccount `shouldBe` Just (BankAccount [Withdraw aDay (Amount 100)])

    it "Given a negative amount and an account, when adding a deposit, then returns Nothing" $ do
      makeDeposit' (-100) emptyAccount `shouldBe` Nothing

    it "Given a negative amount and an account, when adding a withdraw, then returns Nothing" $ do
      makeWithdraw' (-100) emptyAccount `shouldBe` Nothing

    it "Given a list of transactions with invalid entries can create account discarting invalid transactions" $ do
      makeAccount [makeWithdraw' (-100), makeDeposit' 200, makeDeposit' 300, makeWithdraw' 600] `shouldBe`
        BankAccount [Deposit aDay 200, Deposit aDay 300, Withdraw aDay 600]

    it "Given a list of transactions with invalid entries can create account discarting all transactions" $ do
      makeAccount [makeWithdraw' (-100), makeDeposit' 200, makeDeposit' 300, makeWithdraw' 600] `shouldBe`
        BankAccount []

    it "Given an account creates a statement that contains the transactions and the running balance in reverse order" $ do
        account <- return $ makeAccount $ [makeDeposit' 300, makeDeposit' 200, makeWithdraw' 200, makeWithdraw' 100, makeDeposit' 100]
        makeStatement account `shouldBe`
          [(Deposit  aDay 100, RunningBalance 300),
           (Withdraw aDay 100, RunningBalance 200),
           (Withdraw aDay 200, RunningBalance 300),
           (Deposit  aDay 200, RunningBalance 500),
           (Deposit  aDay 300, RunningBalance 300)
          ]
    it "Given an statement can format it" $ do
        account <- return $ makeAccount $ [makeDeposit' 300, makeDeposit' 200, makeWithdraw' 200, makeWithdraw' 100, makeDeposit' 100]
        statementLines <- return $ printStatement . makeStatement $ account
        statementLines `shouldBe`
          ["      date | credit | debit | balance",
           "2017-01-01 | 100    |       |  300",
           "2017-01-01 |        |  100  |  200",
           "2017-01-01 |        |  200  |  300",
           "2017-01-01 | 200    |       |  500",
           "2017-01-01 | 300    |       |  300"]
    it "works" $ do
      days <- newIORef [aDay, aDay, anotherDay]
      program <- return $ program (StubCalendar days)
      console <- program [makeWithdrawFlip 200, makeWithdrawFlip 300]
      console `shouldBe`
        [SpyConsole "      date | credit | debit | balance",
         SpyConsole "2017-01-02 |        |  300  |  -500",
         SpyConsole "2017-01-01 |        |  200  |  -200"
        ]

aDay :: Day
aDay = fromGregorian 2017 01 01

anotherDay :: Day
anotherDay = fromGregorian 2017 01 02

makeDeposit' :: Int -> BankAccount -> Maybe BankAccount
makeDeposit' = makeDeposit aDay
makeWithdraw' :: Int -> BankAccount -> Maybe BankAccount
makeWithdraw' = makeWithdraw aDay

statementLines ::  [String]
statementLines = printStatement . makeStatement . makeAccount $ [makeDeposit' 100, makeWithdraw' (200)]

composing :: Maybe BankAccount
composing = makeWithdraw' 100 >=> makeDeposit' 200 >=> makeDeposit' 300 $ emptyAccount

aHardcodedProgram :: IO ()
aHardcodedProgram = do
  time <- utctDay <$> getCurrentTime
  bankAccount <- return $ makeAccount [makeWithdraw time 100, makeDeposit time (300)]
  statement <- return $ printStatement . makeStatement $ bankAccount
  mapM_ print statement

makeWithdrawFlip :: Int -> Day -> BankAccount -> Maybe BankAccount
makeWithdrawFlip = flip makeWithdraw

class Calendar a where
  day :: a -> IO Day

data RealCalendar = RealCalendar
data StubCalendar = StubCalendar (IORef [Day])

instance Calendar RealCalendar where
  day _ = utctDay <$> getCurrentTime

instance Calendar StubCalendar where
  day (StubCalendar days) = do
    modifyIORef days tail
    head <$> readIORef days

class Console a where
  print' :: String -> IO a

data RealConsole = RealConsole deriving Show
data SpyConsole = SpyConsole String deriving (Show, Eq)

instance Console RealConsole where
  print' text = do
    print text
    return RealConsole

instance Console SpyConsole where
  print' line = return $ SpyConsole $ line

program :: (Console con , Calendar cal) => cal -> [Day -> BankAccount -> Maybe BankAccount] -> IO [con]
program calendar transactions = do
--  day <- trace "1" (fst <$> day calendar)
--  bankAccount <- return $ makeAccount (transactions <*> (pure day))
  completedTransactions <- mapM (\transaction -> transaction <$> day calendar) transactions
  statement <- return $ printStatement . makeStatement . makeAccount $ completedTransactions
  mapM print' statement

main :: IO ()
main = do
--  comp
-- mapM_ print statementLines
  days <- newIORef [aDay, aDay, anotherDay]
  _ <- program (StubCalendar days) [makeWithdrawFlip 200, makeWithdrawFlip 300] :: IO [RealConsole]
  hspec spec
