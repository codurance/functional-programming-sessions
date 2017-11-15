{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}

module BankAccount where

import           Data.Maybe

newtype Amount = Amount { unPositive :: Int } deriving (Num, Eq, Ord)
newtype RunningBalance = RunningBalance Int deriving (Eq)

data Transaction =
    Deposit Amount
  | Withdraw Amount
  deriving (Eq, Ord)

newtype BankAccount = BankAccount { transactions  :: [Transaction] } deriving (Show, Eq)

type Statement = [(Transaction, RunningBalance)]

instance Show RunningBalance where
  show (RunningBalance x) = show x

instance Show Transaction where
  show (Deposit x)  = show x
  show (Withdraw x) = show x

instance Show Amount where
  show (Amount x) = show x

toPositive :: Int -> Maybe Amount
toPositive x
  | x > 0     = Just $ Amount $ x
  | otherwise = Nothing

deposit ::  Amount -> BankAccount -> BankAccount
deposit amount account = addTransaction (Deposit $ amount) account

withdraw ::  Amount -> BankAccount -> BankAccount
withdraw amount account = addTransaction (Withdraw $ amount) account

addTransaction ::  Transaction -> BankAccount -> BankAccount
addTransaction transaction account = BankAccount $ transactions account ++ [transaction]

printStatement ::  Statement -> [String]
printStatement statement = header ++  fmap toStatementLine statement
  where header = [" credit | debit | balance"]
        toStatementLine :: (Transaction, RunningBalance) -> String
        toStatementLine (deposit@(Deposit _),  runningBalance)  = "   " ++ show deposit  ++  "  |  " ++     "   "     ++ "  |  " ++ show runningBalance
        toStatementLine (withdraw@(Withdraw _), runningBalance) = "   " ++     "   "     ++  "  |  " ++ show withdraw ++ "  |  " ++ show runningBalance

amount :: Transaction -> Int
amount (Deposit x)  = unPositive x
amount (Withdraw x) = unPositive $ -x

makeStatement :: BankAccount -> Statement
makeStatement account = zip (reverse . transactions $ account) (reverse balances)
  where balances = fmap RunningBalance . scanl1 (+) . fmap amount $ transactions account

makeWithdraw :: Int -> Maybe (BankAccount -> BankAccount)
makeWithdraw x = fmap withdraw $ (toPositive x)

makeDeposit :: Int -> Maybe (BankAccount -> BankAccount)
makeDeposit x = fmap deposit $ (toPositive x)

makeAccount :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
makeAccount fs = compose (fromMaybe [] $ sequence fs) emptyAccount

makeAccount' :: [Maybe (BankAccount -> BankAccount)] -> BankAccount
makeAccount' fs = compose (catMaybes fs) emptyAccount

emptyAccount :: BankAccount
emptyAccount = BankAccount []

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id
