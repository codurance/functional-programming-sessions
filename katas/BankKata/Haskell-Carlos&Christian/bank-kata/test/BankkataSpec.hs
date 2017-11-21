module BankkataSpec  (main, spec) where

import           BankAccount
import           Data.Time
import           Test.Hspec
import           Test.QuickCheck
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

aDeposit = Deposit aDay (Amount 100)
anotherDeposit = Deposit  aDay (Amount 200)

aWithdraw = Withdraw aDay (Amount 100)
anotherWithdraw = Withdraw aDay (Amount 200)

spec :: Spec
spec = do
  describe "Bank Account" $ do

    it "Given a positive amount and an account, when adding a deposit, then adds deposit to account" $ do
      makeDeposit' 100 <*> Just emptyAccount `shouldBe` Just (BankAccount [Deposit aDay (Amount 100)])

    it "Given a positive amount and an account, when adding a withdraw, then adds withdraw to account" $ do
      makeWithdraw' 100 <*>  Just emptyAccount `shouldBe` Just (BankAccount [Withdraw aDay (Amount 100)])

    it "Given a negative amount and an account, when adding a deposit, then returns Nothing" $ do
      makeDeposit' (-100) <*>  Just emptyAccount `shouldBe` Nothing

    it "Given a negative amount and an account, when adding a withdraw, then returns Nothing" $ do
      makeWithdraw' (-100) <*>  Just emptyAccount `shouldBe` Nothing

    it "Given a list of transactions with invalid entries can create account discarting invalid transactions" $ do
      makeAccount' [makeWithdraw' (-100), makeDeposit' 200, makeDeposit' 300, makeWithdraw' 600] `shouldBe`
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

aDay :: Day
aDay = fromGregorian 2017 01 01

makeDeposit' = makeDeposit aDay
makeWithdraw' = makeWithdraw aDay

statementLines ::  [String]
statementLines = printStatement . makeStatement . makeAccount' $ [makeWithdraw' (-100), makeDeposit' 200, makeDeposit' 300, makeWithdraw' 600]

xxx :: Maybe BankAccount
--xxx = makeDeposit' (100) <*> ((makeDeposit' 200) <*>  ((makeWithdraw' 300) <*>  pure emptyAccount))
xxx = pure (\x y z -> x . y . z $ emptyAccount) <*> (makeDeposit' (100)) <*> (makeDeposit' 200) <*>  (makeWithdraw' 300)
--xxx = pure (.) <*> (makeDeposit' (-100)) <*> (makeDeposit' 200) <*>  ((makeWithdraw' 300) <*>  pure emptyAccount)

main :: IO ()
main = do
-- mapM_ print statementLines
  hspec spec
