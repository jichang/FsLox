// Learn more about F# at http://fsharp.org

open System
open System.IO
open FsLox
open FsLox.Scanner

[<EntryPoint>]
let main argv =
    let filePath = argv.[0]
    let absFilePath = Path.Combine (Environment.CurrentDirectory, filePath)
    match Scanner.scan absFilePath with
    | Ok (tokens, _) ->
        let lexemes = Seq.map (fun (token: Token) -> printfn "%A" token; token.lexeme) tokens
        printfn "%A" (String.concat "" lexemes)
    | Error error ->
        printfn "%A" error

    0 // return an integer exit code