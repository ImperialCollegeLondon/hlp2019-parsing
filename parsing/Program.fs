// Learn more about F# at http://fsharp.org

open System
open ParseHelpers

Expecto

let dispTok (tok:Token) = sprintf "<%A|'%s':%d>" tok.TokType tok.Text tok.Pos

let dispTLst lst = lst |> List.map dispTok |> String.concat ""

let tokRes = tokenise tokenOpArray tokenEndStrings

[<EntryPoint>]
let main argv =
    match tokRes "LOOP MOV R0, R1, 123" with
    | Error e -> printfn "Tokenise error: %s" e
    | Ok tokLst -> printfn "%s" (dispTLst tokLst)
    printfn "press any key to terminate"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
