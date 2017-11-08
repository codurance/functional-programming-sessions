import org.scalatest.{FlatSpec, Matchers}
import BankKata._

class BankKataTest extends FlatSpec with Matchers {
    "statement lines" should "be formatted" in {
        val statementLines = formatBankStatement(
            withdrawal(Amount(30))(
                deposit(Amount(50))(
                    Account())))

        statementLines should be(List(
            "date || credit || debit || balance",
            " || 30 || 20",
            "50 || || 50"))
    }
}
