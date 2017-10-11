{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BankAccount where

newtype Amount = Amount Int deriving (Num, Show, Eq)
type Transactions = [Transaction]
data BankAccount = BankAccount Transactions deriving (Show, Eq)
data Transaction = Transaction Amount deriving (Show, Eq)

deposit ::  BankAccount -> Int -> BankAccount
deposit account amount = addTransaction account amount

withdraw ::  BankAccount -> Int -> BankAccount
withdraw account amount = addTransaction account (-amount)

addTransaction ::  BankAccount -> Int -> BankAccount
addTransaction (BankAccount transactions) amount = BankAccount $ Transaction (Amount amount) : transactions

printStatement ::  BankAccount -> [String]
printStatement (BankAccount transactions) = map toStatementLine transactions
  where toStatementLine (Transaction (Amount amount))
          | amount > 0 = show amount ++ " |"
          | amount < 0 = "| " ++ show  (-amount)
