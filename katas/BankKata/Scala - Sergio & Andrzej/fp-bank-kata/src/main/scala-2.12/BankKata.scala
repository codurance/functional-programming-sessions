import com.sun.javafx.stage.WindowCloseRequestHandler

object BankKata {

    case class Account(transactions: List[Transaction] = List())
    case class AccountStatement(transaction: Transaction, currentBalance: Amount)
    case class Amount(value: Int)

    sealed trait Transaction
    // TODO: Add value function
    case class Deposit(amount: Amount) extends Transaction
    case class Withdrawal(amount: Amount) extends Transaction

    def deposit(amount: Amount)(account: Account): Account = {
        Account(account.transactions ++ List(Deposit(amount)))
    }

    def withdrawal(amount: Amount)(account: Account): Account = {
        Account(account.transactions ++ List(Withdrawal(amount)))
    }

    private def createBankStatement(account: Account): List[AccountStatement] = {
        def previousBalance(statements: List[AccountStatement]) = {
            if (statements.isEmpty) Amount(0)
            else statements.head.currentBalance
        }

        def createAccountStatement(statements: List[AccountStatement], transaction: Transaction) = {
            transaction match {
                case Deposit(amount) => AccountStatement(
                    Deposit(amount),
                    Amount(previousBalance(statements).value + amount.value))
                case Withdrawal(amount) => AccountStatement(
                    Withdrawal(amount),
                    Amount(previousBalance(statements).value - amount.value))
            }
        }

        // TODO: try scan
        account.transactions
            .foldLeft(List[AccountStatement]())((statements: List[AccountStatement], transaction: Transaction) => {
                createAccountStatement(statements, transaction) :: statements
            })
    }

    def formatBankStatement(account: Account): List[String] = {
        def statementHeader = {
            "date || credit || debit || balance"
        }

        def statementLines = {
            createBankStatement(account).map(accountStatement => {
                accountStatement.transaction match {
                    case Deposit(amount) => s"${amount.value} || || ${accountStatement.currentBalance.value}"
                    case Withdrawal(amount) => s" || ${amount.value} || ${accountStatement.currentBalance.value}"
                }
            })
        }

        statementHeader :: statementLines
    }

    def printBankStatement(lines: List[String]): Unit = {
        lines.foreach(line => println(line))
    }

    def main(args: Array[String]) {
        printBankStatement(formatBankStatement(
                withdrawal(Amount(30))(
                    deposit(Amount(50))(
                        Account()))))
    }

}
