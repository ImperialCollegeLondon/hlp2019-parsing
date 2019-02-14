// Learn more about F# at http://fsharp.org

open System
open ParseHelpers

open Expecto

type NumTyp = NBin | NHex | NDec



/// Test the Expecto Test Framework!
[<Tests>]
let allTests = testList "all tests" [
    testCase "A simple test" <| fun () ->
        let expected = 4
        Expect.equal expected (2+2) "2+2 = 4"
    ]

[<Tests>]
let allFsChecks =
    /// sample configuration for fscheck tests
    let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

    /// list of 3 properties to test: eplace with real properties of your code
    let properties =
      testList "FsCheck samples" [
        testProperty "Addition is commutative" <| fun a b ->
          a + b = b + a

        testProperty "Reverse of reverse of a list is the original list" <|
          fun (xs:list<int>) -> List.rev (List.rev xs) = xs

        // you can also override the FsCheck config
        testPropertyWithConfig config "Product is distributive over addition" <|
          fun a b c ->
            a * (b + c) = a * b + a * c
      ]
    // Run the tests: will be run from runAllTestsInAssembly because allFsChecks is
    // tagged by [<Tests>]
    Tests.runTests defaultConfig properties

let dispTok (tok:Token) = sprintf "<%A|'%s':%d>" tok.TokType tok.Text tok.Pos

let dispTLst lst = lst |> List.map dispTok |> String.concat " ; " |> sprintf "[%s]"

let tokRes = tokenise tokenOpArray tokenEndStrings

let tokeniseSomething() =
    match tokRes "LOOP MOV R0, R1, 123" with
    | Error e -> printfn "Tokenise error: %s" e
    | Ok tokLst -> printfn "%s" (dispTLst tokLst)

let parseSomething() =
    printfn "--------"
    "{R0,R1,R2,R3}"
    |> tokRes
    |> fun r -> match r with 
                | Ok toks -> printfn "Toks:%s" (dispTLst toks) 
                | Error e -> printfn "Tokenise Error: %A" e
                r
    |> Result.mapError (fun s -> s,([]:Token list))
    |> function 
        | TPRegListLRes (rl, x) -> 
            match x with
            | Ok toks -> printfn "parse OK with rl=%A and remaining toks=%s" rl (dispTLst toks)
            | Error e -> printfn "Parse failed with  %A" e
        | x -> failwithf "What? TPRegListLRes always matches! Can't match: %A" x
    printfn "---------"

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig [||] |> ignore // run all tests defined with [<Tests>]
    tokeniseSomething()
    parseSomething()
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
