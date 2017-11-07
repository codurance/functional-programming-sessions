{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BankAccount where

type Transactions = [Transaction]
type BankAccount = Transactions
type Statement = [(Transaction, RunningBalance)]

newtype Amount = Amount { unPositive :: Int } deriving (Num, Eq, Ord)

newtype RunningBalance = RunningBalance Int deriving (Eq)

data Transaction =
    Deposit Amount
  | Withdraw Amount
  deriving (Eq, Ord)

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
addTransaction transaction account = account ++ [transaction]

printStatement ::  Statement -> [String]
printStatement = fmap toStatementLine
  where toStatementLine :: (Transaction, RunningBalance) -> String
        toStatementLine (deposit@(Deposit _),  runningBalance) = "   "        ++ show deposit ++   "  |       |  " ++ show runningBalance
        toStatementLine (withdraw@(Withdraw _), runningBalance) = "        | " ++ show withdraw ++  "   |  "        ++ show runningBalance

amount :: Transaction -> Int
amount (Deposit x)  = unPositive x
amount (Withdraw x) = unPositive $ -x

makeStatement :: BankAccount -> Statement
makeStatement account = zip (reverse account) (reverse balances)
  where balances = fmap RunningBalance . scanl1 (+) . fmap amount $ account


newtype All' a = All' { get :: Maybe a }

instance (Monoid a) => Monoid (All' a) where
  mempty = All' Nothing
  All' (Just x) `mappend` All' (Just x') = All' $ Just $ x `mappend` x'
  _ `mappend` _                        = All' Nothing
