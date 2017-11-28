{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}

module BankAccount where

import           Control.Monad
import           Data.Maybe
import           Data.Time

newtype Amount = Amount { unPositive :: Int } deriving (Num, Eq, Ord)
newtype RunningBalance = RunningBalance Int deriving (Eq)

data Transaction = Deposit Day Amount | Withdraw Day Amount deriving (Eq, Ord)

newtype BankAccount = BankAccount { transactions  :: [Transaction] } deriving (Show, Eq)

type Statement = [(Transaction, RunningBalance)]

instance Show RunningBalance where
  show (RunningBalance x) = show x

instance Show Transaction where
  show (Deposit _ x)  = show x
  show (Withdraw _ x) = show x

instance Show Amount where
  show (Amount x) = show x

toPositive :: Int -> Maybe Amount
toPositive x
  | x > 0     = Just $ Amount $ x
  | otherwise = Nothing

deposit :: Day -> Amount -> BankAccount -> BankAccount
deposit date amount account = addTransaction (Deposit date amount) account

withdraw :: Day -> Amount -> BankAccount -> BankAccount
withdraw date amount account = addTransaction (Withdraw date amount) account

makeWithdraw :: Day -> Int -> BankAccount -> Maybe BankAccount
makeWithdraw time x account = withdraw <$> pure time <*> toPositive x <*> pure account

makeDeposit :: Day -> Int -> BankAccount -> Maybe BankAccount
makeDeposit time x account = deposit <$> pure time <*> toPositive x <*> pure account

addTransaction ::  Transaction -> BankAccount -> BankAccount
addTransaction transaction account = BankAccount $ transactions account ++ [transaction]

printStatement ::  Statement -> [String]
printStatement statement = header ++  fmap toStatementLine statement
  where
    header = ["      date | credit | debit | balance"]
    toStatementLine :: (Transaction, RunningBalance) -> String
    toStatementLine ((Deposit date amount),  runningBalance)  = show date ++ " | " ++ show amount  ++  "    |  " ++     "   "     ++ "  |  " ++ show runningBalance
    toStatementLine ((Withdraw date amount), runningBalance)  = show date ++     " |      "     ++ "  |  " ++ show amount  ++  "  |  " ++ show runningBalance

toAmount :: Transaction -> Int
toAmount (Deposit _ x)  = unPositive x
toAmount (Withdraw _ x) = unPositive $ -x

makeStatement :: BankAccount -> Statement
makeStatement account = zip (reverse . transactions $ account) (reverse balances)
  where balances = fmap RunningBalance . scanl1 (+) . fmap toAmount $ transactions account

-- makeAccount :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
-- makeAccount fs = compose (fromMaybe [] $ sequence fs) emptyAccount
--
-- makeAccount' :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
-- makeAccount' fs = compose (catMaybes fs) emptyAccount

makeAccount :: [BankAccount -> Maybe BankAccount] -> BankAccount
makeAccount fs = fromMaybe emptyAccount account
  where account = compose fs $ return emptyAccount

emptyAccount :: BankAccount
emptyAccount = BankAccount []

compose :: (Monad m) => [a -> m a] -> m a -> m a
compose = foldl (>=>) id
