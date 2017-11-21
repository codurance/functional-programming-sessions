{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}

module BankAccount where

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

makeWithdraw :: Day -> Int -> Maybe (BankAccount -> BankAccount)
makeWithdraw time x = fmap (withdraw time) $ toPositive x

makeDeposit :: Day -> Int -> Maybe (BankAccount -> BankAccount)
makeDeposit time x = fmap (deposit time) $ toPositive x

makeAccount :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
makeAccount fs = compose (fromMaybe [] $ sequence fs) emptyAccount

makeAccount' :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
makeAccount' fs = compose (catMaybes fs) emptyAccount

emptyAccount :: BankAccount
emptyAccount = BankAccount []

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
