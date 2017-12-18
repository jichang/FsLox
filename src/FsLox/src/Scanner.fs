namespace FsLox

module Scanner =
    open System.IO

    type TokenType =
        // Single-character tokens
        | LEFT_PAREN
        | RIGHT_PAREN
        | LEFT_BRACE
        | RIGHT_BRACE
        | COMMA
        | DOT
        | MINUS
        | PLUS
        | MULTPLY
        | WHITESPACE
        | SEMICOLON
        | NEWLINE
        | SLASH
        | COMMENT
        // logic operator tokens
        | EQUAL
        | GREATER
        | GREATER_EQUAL
        | LESS
        | LESS_EQUAL
        // Literals
        | STRING
        | NUMBER
        | BOOLEAN
        | IDENTIFIER
        // Keywords
        | LET
        | TYPE
        | TYPECLASS
        | KIND
        | IF
        | THEN
        | ELSE
        | WHILE
        | FOR
        | DO
        | RETURN
        // End of file
        | EOF


    type Line =
        { num: int
          content: string
          peek: int
          offset: int }

    type Position =
        { line: Line
          lineNum: int
          lineOffset: int }

    type Token =
        { ``type``: TokenType
          lexeme: string
          startPosition: Position
          endPosition: Position }

    let isPeekEnd line = line.peek < line.content.Length
    let isEnd line = line.offset < line.content.Length

    let isDigit c =
        c >= '0' && c <= '9'

    let extractNumber line =
        let leftStr = line.content.Substring line.peek
        let chars =
            leftStr.ToCharArray ()
            |> Seq.ofArray
            |> Seq.takeWhile (fun ch -> isDigit ch || ch = '.')
        let charsLength = Seq.length chars
        let nextChar = if charsLength = leftStr.Length then ' ' else leftStr.Chars(charsLength)
        if nextChar = ' ' || nextChar = '\n' || nextChar = ';' then
            Some { line = line;
                   lineNum = line.num;
                   lineOffset = line.peek + charsLength - 1 }
        else
            None

    let search chars result line =
        match result with
        | None ->
            let leftStr = line.content.Substring line.peek
            let index = leftStr.IndexOfAny chars
            if index = -1 then
                None
            else
                Some { line = line;
                       lineNum = line.num 
                       lineOffset = index + line.peek }
        | _ -> result

    let slice lines startPosition endPosition =
        let cut str line =
            if startPosition.lineNum = endPosition.lineNum then
                if line.num = startPosition.lineNum then
                    str + line.content.[startPosition.lineOffset..endPosition.lineOffset]
                else
                    str
            else
                if line.num = endPosition.lineNum then
                    str + line.content.[0..endPosition.lineOffset - 1]
                elif line.num = startPosition.lineNum then
                    str + line.content.[startPosition.lineOffset..]
                else
                    str + line.content

        lines
        |> Seq.filter (fun line -> line.num >= startPosition.lineNum && line.num <= endPosition.lineNum)
        |> Seq.fold cut ""

    let countdown (result: int * (char * Line) option) (line: Line) =
        match result with
        | (n, None) ->
            if n = 0 then
                (0, Some(line.content.Chars line.peek, line))
            else
                let remainingCharsNum = line.content.Length - line.peek
                if remainingCharsNum <= n then
                    (n - remainingCharsNum, None)
                else
                    let peekPos = line.peek + n
                    (0, Some(line.content.Chars peekPos, line))
        | _ ->
            result

    let peek n remainingLines =
        if Seq.isEmpty remainingLines then
            None
        else
            remainingLines
            |> Seq.filter isPeekEnd
            |> Seq.fold countdown (n, None)
            |> snd

    let update (n, lines) line =
        if n <= 0 then
            (n, Seq.append lines [line])
        else
            let remainingLength = line.content.Length - line.peek
            if remainingLength < n then
                (n - remainingLength, Seq.append lines [{line with peek = line.content.Length}])
            else
                (0, Seq.append lines [{line with peek = line.peek + n}])

    let advance (remainingLines: seq<Line>) (n : int) =
        let initState : int * seq<Line> = (n, Seq.empty)

        if Seq.isEmpty remainingLines then
            remainingLines
        else
            remainingLines
            |> Seq.filter isPeekEnd
            |> Seq.fold update initState
            |> snd

    let forward remainingLines =
        remainingLines
        |> Seq.map (fun line -> { line with offset = line.peek })

    let backward remainingLines =
        remainingLines |> Seq.map (fun line -> { line with peek = line.offset})

    let rec scanTokens tokens remainingLines =
        let peekCurrent = peek 0 remainingLines
        let parseResult =
            match peekCurrent with
            | Some ('(', targetLine) ->
                let token =
                    { ``type`` = LEFT_PAREN
                      lexeme = "("
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some (')', targetLine) ->
                let token =
                    { ``type`` = RIGHT_PAREN
                      lexeme = ")"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('{', targetLine) ->
                let token =
                    { ``type`` = LEFT_BRACE
                      lexeme = "{"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('}', targetLine) ->
                let token =
                    { ``type`` = RIGHT_BRACE
                      lexeme = "}"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some (',', targetLine) ->
                let token =
                    { ``type`` = COMMA
                      lexeme = ","
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('.', targetLine) ->
                let token =
                    { ``type`` = DOT
                      lexeme = "."
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('-', targetLine) ->
                let token =
                    { ``type`` = MINUS 
                      lexeme = "-"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('+', targetLine) ->
                let token =
                    { ``type`` = PLUS
                      lexeme = "+"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some (';', targetLine) ->
                let token =
                    { ``type`` = SEMICOLON
                      lexeme = ";"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('*', targetLine) ->
                let token =
                    { ``type`` = MULTPLY
                      lexeme = "*"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('=', targetLine) ->
                let token =
                    { ``type`` = EQUAL
                      lexeme = "="
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('<', targetLine) ->
                let peekNext = peek 1 remainingLines
                let token =
                    match peekNext with
                    | Some ('=', targetLine) ->
                        { ``type`` = LESS_EQUAL
                          lexeme = "<="
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek + 2 } }
                    | _ ->
                        { ``type`` = LESS
                          lexeme = "<"
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('>', targetLine) ->
                let peekNext = peek 1 remainingLines
                let token =
                    match peekNext with
                    | Some ('=', targetLine) ->
                        { ``type`` = GREATER_EQUAL
                          lexeme = ">="
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek + 2 } }
                    | _ ->
                        { ``type`` = GREATER
                          lexeme = ">"
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('/', targetLine) ->
                let peekNext = peek 1 remainingLines
                let token =
                    match peekNext with
                    | Some ('/', targetLine) ->
                        { ``type`` = COMMENT
                          lexeme = targetLine.content.Substring targetLine.peek
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.content.Length - 1 } }
                    | _ ->
                        { ``type`` = SLASH
                          lexeme = "/"
                          startPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek }
                          endPosition =
                              { line = targetLine
                                lineNum = targetLine.num
                                lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('"', targetLine) ->
                let startPosition =
                    { line = targetLine
                      lineNum = targetLine.num
                      lineOffset = targetLine.peek }
                let searchRemainingLines = advance remainingLines 1
                let endPosition = Seq.fold (search [|'"'|]) None searchRemainingLines
                match endPosition with
                | Some endPosition ->
                    let lexeme = slice remainingLines startPosition endPosition
                    let token =
                        { ``type`` = STRING
                          lexeme = lexeme
                          startPosition = startPosition
                          endPosition = endPosition }
                    let updateRemainingLines =
                        advance remainingLines lexeme.Length
                        |> Seq.filter isEnd
                    Some (token, updateRemainingLines)
                | None ->
                    None
            | Some (' ', targetLine) ->
                let token =
                    { ``type`` = WHITESPACE
                      lexeme = " "
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some ('\n', targetLine) ->
                let token =
                    { ``type`` = NEWLINE
                      lexeme = "\n"
                      startPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek }
                      endPosition =
                          { line = targetLine
                            lineNum = targetLine.num
                            lineOffset = targetLine.peek + 1 } }
                let updateRemainingLines =
                    advance remainingLines token.lexeme.Length
                    |> Seq.filter isEnd
                Some (token, updateRemainingLines)
            | Some (c, targetLine) when isDigit c ->
                let startPosition =
                    { line = targetLine
                      lineNum = targetLine.num
                      lineOffset = targetLine.peek }
                let endPosition = extractNumber targetLine
                match endPosition with
                | Some endPosition ->
                    let lexeme = slice remainingLines startPosition endPosition
                    let token =
                        { ``type`` = NUMBER
                          lexeme = lexeme
                          startPosition = startPosition
                          endPosition = endPosition }
                    let updateRemainingLines =
                        advance remainingLines lexeme.Length
                        |> Seq.filter isEnd
                    Some (token, updateRemainingLines)
                | None -> None
            | Some (_, targetLine) ->
                let startPosition =
                    { line = targetLine
                      lineNum = targetLine.num
                      lineOffset = targetLine.peek }
                let searchRemainingLines = advance remainingLines 1
                let sepPosition = Seq.fold (search [|';'; ' '; '\n'|]) None searchRemainingLines
                match sepPosition with
                | Some sepPosition ->
                    let endPosition = {sepPosition with lineOffset = sepPosition.lineOffset - 1}
                    let lexeme = slice remainingLines startPosition endPosition
                    let token =
                        match lexeme with
                        | "let" ->
                            { ``type`` = LET
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "if" ->
                            { ``type`` = IF
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "then" ->
                            { ``type`` = THEN
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "else" ->
                            { ``type`` = ELSE
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "while" ->
                            { ``type`` = WHILE
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "for" ->
                            { ``type`` = FOR
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "do" ->
                            { ``type`` = DO
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "return" ->
                            { ``type`` = RETURN
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "true" ->
                            { ``type`` = BOOLEAN
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "false" ->
                            { ``type`` = BOOLEAN
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition
                            }
                        | "type" ->
                            { ``type`` = TYPE
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition }
                        | "typeclass" ->
                            { ``type`` = TYPECLASS
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition }
                        | _ ->
                            { ``type`` = IDENTIFIER
                              lexeme = lexeme
                              startPosition = startPosition
                              endPosition = endPosition }
                    let updateRemainingLines =
                        advance remainingLines lexeme.Length
                        |> Seq.filter isEnd
                    Some (token, updateRemainingLines)
                | None ->
                    None
            | _ ->
                None

        match parseResult with
        | Some (token, updateRemainingLines) ->
            let updateTokens = Seq.append tokens [token]
            if Seq.isEmpty updateRemainingLines then
                Ok (updateTokens, remainingLines)
            else
                scanTokens updateTokens updateRemainingLines
        | None ->
            Ok (tokens, remainingLines)

    let scanLine result newLine =
        match result with
        | Ok (tokens, remainingLines) ->
            let scanResult = scanTokens Seq.empty (Seq.append remainingLines [newLine])
            match scanResult with
            | Ok (newTokens, updateRemainingLines) ->
                let forwardRemainingLines =
                    forward updateRemainingLines
                    |> Seq.filter isEnd
                Ok (Seq.append tokens newTokens, forwardRemainingLines)
            | _ ->
                scanResult
        | _ -> result

    let scan (filePath : string) =
        File.ReadAllLines filePath
        |> Seq.mapi (fun num content -> { num = num + 1; content = content + "\n"; peek = 0; offset = 0 })
        |> Seq.fold scanLine (Ok (Seq.empty, Seq.empty))