namespace BankKataFSharp

(*
## Name everything
## No mutable state
## Do not use intermediate variables
## One argument functions
## No Explicit recursion
## Exhaustive conditionals
## Expressions not statements.
## Generic building blocks - Keep Constraints to boundaries
## Side effects at the boundary
## Infinite Sequences (Lazy Evaluation of sequences?)
## Complete functions only
*)

module Code = 
    let balance (account : int list) = account |> List.sum

    let makeDeposit amount account=
        amount::account

    let makeWithdrawal amount account =
        (- amount)::account

    let createAccount initialBalance =
        [initialBalance]
            
    let prepareEntriesStatement account =
        account |> List.map (fun t -> sprintf "%s || %i || || %i" (System.DateTime.Now.ToString()) t (balance account))
    
    let printStatement account =
        prepareEntriesStatement account
        |> List.fold (fun acc s -> sprintf "%s\n%s" acc s) "date || credit || debit || balance"
