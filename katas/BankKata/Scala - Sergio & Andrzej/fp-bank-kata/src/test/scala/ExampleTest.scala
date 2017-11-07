import org.scalatest.{FlatSpec, Matchers}
import BankKata._

class ExampleTest extends FlatSpec with Matchers {
    "statement lines" should "be formatted" in {
        val statementLines = formatStatement(createBankStatement(
            withdrawal(Amount(30))(
                deposit(Amount(50))(
                    Account(List())))))
        statementLines should be(List(
            "None || Some(Amount(30)) || Amount(20)",
            "Some(Amount(50)) || None || Amount(50)"
        ))
    }
}
