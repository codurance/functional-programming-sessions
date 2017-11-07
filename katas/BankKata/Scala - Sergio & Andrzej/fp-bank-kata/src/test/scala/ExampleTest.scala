import org.scalatest.{FlatSpec, Matchers}
import BankKata._

class ExampleTest extends FlatSpec with Matchers {
    "statement lines" should "be formatted" in {
        val statementLines = formatBankStatement(createBankStatement(
            withdrawal(Amount(30))(
                deposit(Amount(50))(
                    Account(List())))))

        statementLines should be(List(
            " || 30 || 20",
            "50 || || 50"))
    }
}
