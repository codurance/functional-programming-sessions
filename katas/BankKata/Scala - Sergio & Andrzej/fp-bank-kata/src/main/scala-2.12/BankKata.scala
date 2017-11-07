object BankKata {

    case class Account(transactions: List[Transaction] = List())
    case class AccountStatement(balance: Either[Amount, Amount], currentBalance: Amount)
    case class Amount(value: Int)

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
        def previousBalance(statements: List[AccountStatement]) = {
            if (statements.isEmpty) Amount(0)
            else statements.head.currentBalance
        }

        def createAccountStatement(statements: List[AccountStatement], transaction: Transaction) = {
            transaction match {
                case Deposit(amount) => AccountStatement(
                    Left(amount),
                    Amount(previousBalance(statements).value + amount.value))
                case Withdrawal(amount) => AccountStatement(
                    Right(amount),
                    Amount(previousBalance(statements).value - amount.value))
            }
        }

        account.transactions
            .foldLeft(List[AccountStatement]())((statements: List[AccountStatement], transaction: Transaction) => {
                createAccountStatement(statements, transaction) :: statements
            })
    }

    def formatBankStatement(accountStatements: List[AccountStatement]): List[String] = {
        accountStatements.map(accountStatement => {
            accountStatement.balance match {
                case Left(amount) => s"${amount.value} || || ${accountStatement.currentBalance.value}"
                case Right(amount) => s" || ${amount.value} || ${accountStatement.currentBalance.value}"
            }
        })
    }

    def printBankStatement(lines: List[String]): Unit = {
        println("date || credit || debit || balance")
        lines.foreach(line => println(line))
    }

    def main(args: Array[String]) {
        printBankStatement(formatBankStatement(createBankStatement(
                withdrawal(Amount(30))(
                    deposit(Amount(50))(
                        Account())))))
    }

}
