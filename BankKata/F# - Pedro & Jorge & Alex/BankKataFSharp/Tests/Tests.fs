module Tests

open Xunit
open FsUnit
open FsUnit.Xunit
open BankKataFSharp.Code

(*
Given a client makes a deposit of 1000 on 10-01-2012
And a deposit of 2000 on 13-01-2012
And a withdrawal of 500 on 14-01-2012
When she prints her bank statement
Then she would see
date || credit || debit || balance
14/01/2012 || || 500.00 || 2500.00
13/01/2012 || 2000.00 || || 3000.00
10/01/2012 || 1000.00 || || 1000.00
*)
[<Theory>]
[<InlineData (1000, 1000)>]
[<InlineData (2000, 2000)>]
let ``Make a deposit of ``(amount, expectedBalance) = 
    createAccount 0 
    |> makeDeposit amount
    |> balance 
    |> should equal expectedBalance


[<Theory>]
[<InlineData(1000,1000,2000)>]
[<InlineData(1200,500,1700)>]
let ``Make deposit with starting balance``(initialBalance, amount, expectedBalance) =
    createAccount initialBalance 
    |> makeDeposit amount
    |> balance 
    |> should equal expectedBalance

[<Theory>]
[<InlineData(1000,1000,0)>]
[<InlineData(2000,500,1500)>]
let ``Make withdrawal`` (initialBalance, amount, expectedBalance) =
    createAccount initialBalance
    |> makeWithdrawal amount
    |> balance 
    |> should equal expectedBalance

[<Fact>]
let ``Print statement empty account`` () =
    createAccount 0
    |> printStatement 
    |> should equal "date || credit || debit || balance"

[<Fact>]
let ``Print statement with one deposit`` () =
    createAccount 0
    |> makeDeposit 1000
    |> printStatement 
    |> should equal <| "date || credit || debit || balance\n" +
                       (sprintf "%s || %i || || %i" (System.DateTime.Now.ToString()) 1000 1000)

[<Fact>]
let ``Print statement with two deposits`` () =
    createAccount 0
    |> makeDeposit 1000
    |> makeDeposit 500
    |> printStatement 
    |> should equal <| "date || credit || debit || balance\n" +
                       (sprintf "%s || %i || || %i\n" (System.DateTime.Now.ToString()) 1000 1000) +
                       (sprintf "%s || %i || || %i" (System.DateTime.Now.ToString()) 500 1500)                     