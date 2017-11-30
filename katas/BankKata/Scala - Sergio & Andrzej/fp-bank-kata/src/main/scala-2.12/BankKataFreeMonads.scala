import cats.free.Free
import cats.{Id, ~>}

import scala.collection.mutable

object BankKataFreeMonads {

    case class Account()
    case class StatementLine(credit: Int = 0, debit: Int = 0, balance: Int = 0, date: Long = 0)
    case class Transaction(amount: Int, date: Long)

    sealed trait TransactionOp[A]
    case class MakeDeposit(amount: Int, date: Long) extends TransactionOp[Unit]
    case class MakeWithdrawal(amount: Int, date: Long) extends TransactionOp[Unit]
    case class PrintBankStatement() extends TransactionOp[Unit]

    implicit def liftTransactionOp[A](t: TransactionOp[A]): Free[TransactionOp, A] = Free.liftF(t)

    def program: Free[TransactionOp, Unit] = for {
        _ <- MakeDeposit(50, now)
        _ <- MakeWithdrawal(30, now)
        _ <- MakeDeposit(90, now)
        _ <- MakeWithdrawal(40, now)
        _ <- PrintBankStatement()
    } yield ()

    private def now = System.currentTimeMillis()

    def transactionInterpreter = new (TransactionOp ~> Id) {
        val transactions: mutable.ListBuffer[Transaction] = mutable.ListBuffer[Transaction]()

        override def apply[A](fa: TransactionOp[A]) = fa match {
            case MakeDeposit(amount, date) => {
                transactions += Transaction(amount, date)
                ()
            }
            case MakeWithdrawal(amount, date) => {
                transactions += Transaction(-amount, date)
                ()
            }
            case PrintBankStatement() => {
                val statementLines = transactions
                        .scanLeft(StatementLine())((acc, curr) => {
                            val amount = curr.amount
                            val date = curr.date
                            val credit = if (amount > 0) amount else 0
                            val debit = if (amount < 0) amount else 0
                            StatementLine(credit, debit, amount + acc.balance, date)
                        })
                        .drop(1)
                        .reverse
                        .map(formatStatementLine)

                println("date || credit || debit || balance")
                statementLines.foreach(println)
                ()
            }
        }

        private def formatStatementLine(statementLine: StatementLine): String = {
            val amountToStr = (amount: Int) => if (amount == 0) "" else amount

            val credit = amountToStr(statementLine.credit)
            val debit = amountToStr(statementLine.debit)

            s"${statementLine.date} || $credit || $debit || ${statementLine.balance}"
        }

    }

    def interpreter = transactionInterpreter

    // Try test interpreter

    def main(args: Array[String]) {
        program.foldMap(interpreter)
    }

}
