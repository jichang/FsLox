// Learn more about F# at http://fsharp.org

open System
open System.IO
open FsLox
open FsLox.Scanner

let omitSpaces tokens =
    Seq.filter (fun token -> token.``type`` <> WHITESPACE) tokens

[<EntryPoint>]
let main argv =
    let filePath = argv.[0]
    let absFilePath = Path.Combine (Environment.CurrentDirectory, filePath)
    let scanRes = Scanner.scan absFilePath
    match scanRes with
    | Ok (tokens, _) ->
        match Parser.parse (tokens |> omitSpaces) Seq.empty with
        | Ok ast ->
            Seq.iter (fun expr -> printfn "%A" expr) ast.Exprs
            let value = Interpreter.interprete Map.empty ast
            printfn "%A" value
        | Error error ->
            printfn "%A" error
    | Error error ->
        printfn "%A" error

    0 // return an integer exit code