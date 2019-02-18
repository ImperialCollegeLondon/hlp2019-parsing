// Learn more about F# at http://fsharp.org

open System
open ParseHelpers

open Expecto
open FsCheck

open System.Text.RegularExpressions

type NumTyp = NBin | NHex | NDec

module Gen =
    // custom random generators using FsCheck.Gen combinators
    // See https://github.com/haf/expecto/blob/master/README.md#property-based-tests
    // See https://fscheck.github.io/FsCheck/TestData.html for Gen.* info
    let alphaGen = Gen.elements ([ 'a'..'z'] @ ['A'..'Z'])
    let digitGen = Gen.elements(['0'..'9'])
    let alphaNumGen = Gen.oneof [alphaGen ; digitGen] 
    let regNameGen = Gen.map (sprintf "R%d") (Gen.oneof [Gen.choose(0,15)])
    let manyAlphaNumGen = 
        let arrToStr a = a |> Seq.map Char.ToString |> String.concat ""
        Gen.map arrToStr  (Gen.arrayOf alphaNumGen)
    let idGen = Gen.map2 (fun (a:char) b -> 
        a.ToString() + b) alphaGen manyAlphaNumGen
    let opGen = Gen.elements tokenOpArray
    let numGen = Gen.map (sprintf "%d") (Gen.choose(0,9999999))
    let tokListGen = Gen.listOf (Gen.oneof [idGen ; idGen ; numGen; numGen; opGen])
    let optSpaceGen = Gen.elements ["";" "]

    // magic 'partial active pattern'
    // see https://fsharpforfunandprofit.com/posts/convenience-active-patterns/

    /// match only on a string which is NOT an operator, returning the string
    let (|MatchNotOp|_|) s =
        if Array.contains s tokenOpArray then None else Some s

    /// Custom generator of even integers
    type EvenInt = EvenInt of int
    let evenIntArb = 
        Gen.choose(1,100) 
        |> Gen.filter (fun i -> i % 2 = 0)
        |> Arb.fromGen
        |> Arb.convert EvenInt (fun (EvenInt l) -> l)

    /// Custom generator of valid strings for tokenisation
    /// Use gen {} comp expression for complex generation
    /// NB this will NOT normally be needed
    type TokString = TokString of string list
    let tokStringArb =
        gen {
            let! tokList = tokListGen
            let! sepList = Gen.listOfLength tokList.Length optSpaceGen
            let toksWithOptSeps = List.zip tokList sepList
            let getNextTokWithSep (lst: (string * string) list) =
                match lst with
                | [] | [_] -> ""
                | (MatchNotOp t1, s1) :: (MatchNotOp _, _) :: _ -> t1 + " "
                | (t1, s1) :: (_, _) :: _ -> t1 + s1
            let rec subLists  = function | [] -> [[]] | _ :: tl as lst  -> lst :: subLists tl
            return toksWithOptSeps 
                    |> subLists
                    |> List.map getNextTokWithSep
        }
        |> Arb.fromGen
        |> Arb.convert TokString (fun (TokString l) -> l)

    let addToConfig config =
        let addedTypes = [
                typeof<TokString>.DeclaringType
                typeof<EvenInt>.DeclaringType          
            ]
        { config with arbitrary = addedTypes @ config.arbitrary}

[<AutoOpen>]
module Auto =
    // this defines FsCheck tests that include the custom generators
    // each generator is associated with a one case D.U. type used to wrap its data
    let private config = Gen.addToConfig FsCheckConfig.defaultConfig
    let testProp name = testPropertyWithConfig config name
    let ptestProp name = ptestPropertyWithConfig config name
    let ftestProp name = ftestPropertyWithConfig config name
    let etestProp stdgen name = etestPropertyWithConfig stdgen config name

let printArbSamples (arb:Arbitrary<'a>) =
    let lst = (Gen.sample 7 10 arb.Generator)
    lst |> List.map (printf "%A\n")
    |> ignore
    printfn ""

let printGenSamples gen = Arb.fromGen gen |> printArbSamples

type Checker<'a,'b> = {Actual:'a ; Expected:'b }

let expectoEqualsBy f act exp mess =
    let opt =
        if f act = f exp then
            None
        else 
            Some {Actual=act; Expected=exp}
    Expect.isNone opt mess

[<Tests>]
let testCustomTokeniser = 
    let prop = testProp "Tokeniser delivers the correct number of tokens" <| fun (Gen.TokString ct) ->
            let tokStr = String.concat "" ct
            let tokLst = 
                ct 
                |> List.takeWhile (fun s -> Array.contains (s.Trim()) tokenEndStrings |> not)
                |> List.filter (fun s -> String.IsNullOrWhiteSpace s |> not ) 
            match tokenise tokenOpArray tokenEndStrings tokStr with
            | Ok lst -> Expect.equal lst.Length tokLst.Length 
                         (sprintf "Tokenise '%A' = %A" tokStr lst)
            | Error mess -> failwithf "Tokeniser failed with:%s" mess
    prop



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
    printArbSamples Gen.tokStringArb
    tokeniseSomething()
    parseSomething()
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
