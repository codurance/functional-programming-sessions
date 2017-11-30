import cats.free.Free
import cats.{Id, ~>}

import scala.collection.mutable

object BankKataFreeMonads {

    case class Account()

    sealed trait TransactionOp[A]
    case class Deposit(amount: Int) extends TransactionOp[Unit]
    case class Withdrawal(amount: Int) extends TransactionOp[Unit]
    case class PrintBankStatement() extends TransactionOp[Unit]

    implicit def liftTransactionOp[A](t: TransactionOp[A]): Free[TransactionOp, A] = Free.liftF(t)

    def program: Free[TransactionOp, Unit] = for {
        _ <- Deposit(50)
        _ <- Withdrawal(30)
        _ <- Deposit(90)
        _ <- Withdrawal(40)
        _ <- PrintBankStatement()
    } yield ()

    def transactionInterpreter = new (TransactionOp ~> Id) {
        val transactions: mutable.ListBuffer[Int] = mutable.ListBuffer[Int]()

        override def apply[A](fa: TransactionOp[A]) = fa match {
            case Deposit(amount) => {
                transactions += amount
                ()
            }
            case Withdrawal(amount) => {
                transactions += -amount
                ()
            }
            case PrintBankStatement() => {
                val statementLines = transactions
                        .scan(0)(_ + _)
                        .drop(1)
                        .zip(transactions)
                        .reverse
                        .map(t => formatStatementLine(t._1, t._2))

                println("date || credit || debit || balance")
                statementLines.foreach(println)
                ()
            }
        }

        private def formatStatementLine(balance: Int, amount: Int): String = {
            val credit = if (amount > 0) amount else ""
            val debit = if (amount < 0) amount else ""

            s"$credit || $debit || $balance"
        }

    }

    def interpreter = transactionInterpreter

    // Try test interpreter

    def main(args: Array[String]) {
        program.foldMap(interpreter)
    }

}
