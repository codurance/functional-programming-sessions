module BankkataSpec  (main, spec) where

import           BankAccount
import           Data.Maybe
import           Data.Monoid
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

aDeposit = Deposit $ Amount 100
anotherDeposit = Deposit $ Amount 200

aWithdraw = Withdraw $ Amount 100
anotherWithdraw = Withdraw $ Amount 200

spec :: Spec
spec = do
  describe "Bank Account" $ do

    describe "Deposit" $ do
      it "deposits" $ do
        deposit 100 [] `shouldBe` [Deposit $ Amount 100]

    describe "Withdraw" $ do
      it "withdraws" $ do
        withdraw 100 [] `shouldBe` [Withdraw $ Amount (100)]

    describe "Statement" $ do
      it "creates statement" $ do
        (makeStatement . deposit 100 . withdraw 100 . withdraw 200 . deposit 200 . deposit 300 $ []) `shouldBe`
          [(Deposit  100, RunningBalance 300),
           (Withdraw 100, RunningBalance 200),
           (Withdraw 200, RunningBalance 300),
           (Deposit  200, RunningBalance 500),
           (Deposit  300, RunningBalance 300)
          ]

-- main :: IO ()
-- main = hspec spec

makeWithdraw :: Int -> Maybe (BankAccount -> BankAccount)
makeWithdraw x = fmap withdraw $ (toPositive x)

makeDeposit :: Int -> Maybe (BankAccount -> BankAccount)
makeDeposit x = fmap deposit $ (toPositive x)

main :: IO ()
main = do
  transactions <- return $ get $ (All' $ makeWithdraw 100) <> (All' $ makeDeposit 500) <> (All' $ makeDeposit 200)
  account <- return $ transactions <*> Just []
  statement <- return $ makeStatement <$> account
  statementLines <- return $ fromMaybe [""] $ printStatement <$> statement
  print " credit | debit | balance"
  mapM_ print statementLines
  return $ ()
