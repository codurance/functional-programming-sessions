object BankKata {

    case class Account(transactions: List[Transaction])
    case class AccountStatement(balance: Either[Amount, Amount], currentBalance: Amount)
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

    def createBankStatement(account: Account): List[AccountStatement] = {
        account.transactions
            .foldLeft(List[AccountStatement]())((statements: List[AccountStatement], transaction: Transaction) => {
                // TODO: Extract method here
                val previousBalance: Amount = if (statements.isEmpty) Amount(0) else statements.head.currentBalance
                // TODO: Extract method here
                val accountStatement: AccountStatement = transaction match {
                    case Deposit(amount) => AccountStatement(
                        Left(amount),
                        Amount(previousBalance.dolars + amount.dolars))
                    case Withdrawal(amount) => AccountStatement(
                        Right(amount),
                        Amount(previousBalance.dolars - amount.dolars))
                }
                accountStatement :: statements
            })
    }

    def formatStatement(accountStatements: List[AccountStatement]): List[String] = {
        accountStatements.map(accountStatement => {
            accountStatement.balance match {
                case Left(amount) => s"${amount.dolars} || || ${accountStatement.currentBalance.dolars}"
                case Right(amount) => s" || ${amount.dolars} || ${accountStatement.currentBalance.dolars}"
            }
        })
    }

    def printStatement(lines: List[String]): Unit = {
        println("date || credit || debit || balance")
        lines.foreach(line => println(line))
    }

    def main(args: Array[String]) {
        def initialAccount() = {
            Account(List())
        }

        printStatement(formatStatement(createBankStatement(
                withdrawal(Amount(30))(
                    deposit(Amount(50))(
                        initialAccount())))))
    }

}
