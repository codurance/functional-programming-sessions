module BankkataSpec  (main, spec) where

import           BankAccount
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

-- Given a client makes a deposit of 1000 on 10-01-2012
-- And a deposit of 2000 on 13-01-2012
-- And a withdrawal of 500 on 14-01-2012
-- When she prints her bank statement
-- Then she would see
-- date || credit || debit || balance
-- 14/01/2012 || || 500.00 || 2500.00
-- 13/01/2012 || 2000.00 || || 3000.00
-- 10/01/2012 || 1000.00 || || 1000.00

aDeposit = Transaction $ Amount 123
anotherDeposit = Transaction $ Amount 456

aWithdraw = Transaction $ Amount (-123)
anotherWithdraw = Transaction $ Amount (-456)

spec :: Spec
spec = do
  describe "Bank Account" $ do

    describe "Deposit" $ do
      it "deposits" $ do
        deposit (BankAccount []) 123 `shouldBe` BankAccount [Transaction (Amount 123)]

    describe "Withdraw" $ do
      it "withdraws" $ do
        withdraw (BankAccount []) 123 `shouldBe` BankAccount [Transaction (Amount (-123))]

    describe "Format statement" $ do
      it "Format deposits in debit colum" $ do
        printStatement (BankAccount [aDeposit, anotherDeposit]) `shouldBe` ["123 |", "456 |"]
      it "Format withdraws in credit colum" $ do
        printStatement (BankAccount [aWithdraw, anotherWithdraw]) `shouldBe` ["| 123", "| 456"]
