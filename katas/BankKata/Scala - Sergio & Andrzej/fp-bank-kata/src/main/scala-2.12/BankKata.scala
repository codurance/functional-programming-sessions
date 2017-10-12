object BankKata {

    case class Account(transactions: List[Transaction])
    case class AccountStatement(credit: Option[Amount], debit: Option[Amount], balance: Amount)
    case class Amount(dolars: Int)

    sealed trait Transaction
    case class Deposit(amount: Amount) extends Transaction
    case class Withdrawal(amount: Amount) extends Transaction

    def deposit(amount: Amount)(account: Account): Account = {
        Account(account.transactions ++ List(Deposit(amount)))
    }

    def withdrawal(amount: Amount)(account: Account): Account = {
        Account(account.transactions ++ List(Withdrawal(amount)))
    }

    def printBankStatement(account: Account): Unit = {
        println("date || credit || debit || balance")
        // TODO: Extract method here
        account.transactions
            .foldLeft(List[AccountStatement]())((statements: List[AccountStatement], transaction: Transaction) => {
                // TODO: Extract method here
                val previousBalance: Amount = if (statements.isEmpty) Amount(0) else statements.head.balance
                // TODO: Extract method here
                val accountStatement: AccountStatement = transaction match {
                    case Deposit(amount) => AccountStatement(
                        Some(amount),
                        None,
                        Amount(previousBalance.dolars + amount.dolars))
                    case Withdrawal(amount) => AccountStatement(
                        None,
                        Some(amount),
                        Amount(previousBalance.dolars - amount.dolars))
                }
                accountStatement :: statements
            })
            .foreach(accountStatement => {
               println(s"${accountStatement.credit} || ${accountStatement.debit} || ${accountStatement.balance}")
            })
    }

    def main(args: Array[String]) {
        def initialAccount() = {
            Account(List())
        }

        printBankStatement(
            withdrawal(Amount(30))(
                deposit(Amount(50))(
                    initialAccount())))
    }

}
