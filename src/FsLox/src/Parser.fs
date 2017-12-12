namespace FsLox

module Parser =
    open FsLox.Scanner

    type IfExpr = 
        { conditionExpr: Expr
          thenBranchExpr: Expr
          elseBranchExpr: Expr option }
    and WhileExpr = 
        { conditionExpr: Expr
          loopBranchExpr: Expr
          elseBranchExpr: Expr option }
    and ForConditionExpr =
            { initExpr: Expr
              checkExpr: Expr
              endExpr: Expr }
    and ForExpr = 
        { conditionExpr: ForConditionExpr
          loopBranchExpr: Expr }
    and LetExpr =
        { targetExpr: Expr
          sourceExpr: Expr }
    and BinaryExpr =
        { operator: Token
          leftExpr: Expr
          rightExpr: Expr }
    and Expr =
        | Identifier of Token
        | If of IfExpr
        | While of IfExpr
        | For of ForExpr
        | Let of LetExpr
        | Lateral of Token
        | Binary of BinaryExpr
        | Return
        | Empty

    type ParserError =
        { Token: Token
          Message: string}

    type AST =
        { Exprs: Expr seq }

    let isType typ token =
        token.``type`` = typ

    let consumeToken tokens tokenType message =
        let token = Seq.head tokens
        if token.``type`` = tokenType then
            Ok (Seq.tail tokens)
        else
            Error { Token = token; Message = message }

    let rec parseLetExpression tokens =
        let firstToken = Seq.head tokens
        match firstToken.``type`` with
        | LET ->
            let tokensAfterLet = Seq.tail tokens
            let identifierToken = Seq.head tokensAfterLet
            match identifierToken.``type`` with
            | IDENTIFIER ->
                let targetExpr = Identifier identifierToken
                let tokensAfterTarget = Seq.tail tokensAfterLet
                let nextToken = Seq.head tokensAfterTarget
                if nextToken.``type`` = EQUAL then
                    let tokensAfterEqual =
                        Seq.tail tokensAfterTarget
                        |> Seq.skipWhile (isType NEWLINE)
                    match parseExpression tokensAfterEqual with
                    | Ok (Identifier _ as sourceExpr, tokensAfterSource)
                    | Ok (Lateral _ as sourceExpr, tokensAfterSource)
                    | Ok (Binary _ as sourceExpr, tokensAfterSource)
                    | Ok (If _ as sourceExpr, tokensAfterSource) ->
                        Ok (Let { targetExpr = targetExpr; sourceExpr = sourceExpr}, tokensAfterSource)
                    | parseRes ->
                        parseRes
                else
                    Error  { Token = nextToken; Message = "let statement expect operator =" }
            | _ ->
                Error { Token = Seq.head tokens; Message = "target of let statement should be a valid identifier expression" }
        | _ ->
            Error { Token = firstToken; Message = "first token of let statement must be let" }
    and parseIfExpression tokens =
        let firstToken = Seq.head tokens
        match firstToken.``type`` with
        | IF ->
            let tokensAfterIf = Seq.tail tokens
            let exprRes = parseExpression tokensAfterIf
            match exprRes with
            | Ok (conditionExpr, tokensAfterConditionExpr) ->
                let tokensAfterConditionExpr =
                    tokensAfterConditionExpr
                    |> Seq.skipWhile (isType NEWLINE)
                let thenToken = Seq.head tokensAfterConditionExpr
                if thenToken.``type`` = THEN then
                    let tokensAfterThen =
                        Seq.tail tokensAfterConditionExpr
                        |> Seq.skipWhile (isType NEWLINE)
                    match parseExpression tokensAfterThen with
                    | Ok (thenBranchExpr, tokensAfterThenBranch) ->
                        let tokensAfterThenBranch =
                            tokensAfterThenBranch
                            |> Seq.skipWhile (isType NEWLINE)
                        let elseToken = Seq.head tokensAfterThenBranch
                        match elseToken.``type`` with
                        | ELSE ->
                            let tokensAfterElse =
                                Seq.tail tokensAfterThenBranch
                                |> Seq.skipWhile (isType NEWLINE)
                            match parseExpression tokensAfterElse with
                            | Ok (elseBranchExpr, tokensAfterElseBranch) ->
                                Ok (If { conditionExpr = conditionExpr; thenBranchExpr = thenBranchExpr; elseBranchExpr = Some elseBranchExpr}, tokensAfterElseBranch)
                            | _ ->
                                Error {Token = Seq.head tokensAfterElse; Message = "else branch must has an expression"}
                        | _ ->
                            Ok (If { conditionExpr = conditionExpr; thenBranchExpr = thenBranchExpr; elseBranchExpr = None}, tokensAfterThenBranch)
                    | _ ->
                        Error { Token = thenToken; Message = "if expression must has a then expression" }
                else
                    Error { Token = thenToken; Message = "if expression must has a then expression" }
            | _ ->
                exprRes
        | _ ->
            Error { Token = firstToken; Message = "first token of if statement must be if" }
    and parseWhileExpression tokens =
        let firstToken = Seq.head tokens
        match firstToken.``type`` with
        | WHILE ->
            let tokensAfterWhile = Seq.tail tokens
            let exprRes = parseExpression tokensAfterWhile
            match exprRes with
            | Ok (conditionExpr, tokensAfterConditionExpr) ->
                let tokensAfterConditionExpr =
                    tokensAfterConditionExpr
                    |> Seq.skipWhile (isType NEWLINE)
                let doToken = Seq.head tokensAfterConditionExpr
                if doToken.``type`` = DO then
                    let tokensAfterDo =
                        Seq.tail tokensAfterConditionExpr
                        |> Seq.skipWhile (isType NEWLINE)
                    match parseExpression tokensAfterDo with
                    | Ok (loopBranchExpr, tokensAfterLoopBranch) ->
                        let tokensAfterLoopBranch =
                            tokensAfterLoopBranch
                            |> Seq.skipWhile (isType NEWLINE)
                        let elseToken = Seq.head tokensAfterLoopBranch
                        match elseToken.``type`` with
                        | ELSE ->
                            let tokensAfterElse =
                                Seq.tail tokensAfterLoopBranch
                                |> Seq.skipWhile (isType NEWLINE)
                            match parseExpression tokensAfterElse with
                            | Ok (elseBranchExpr, tokensAfterElseBranch) ->
                                Ok (While { conditionExpr = conditionExpr; thenBranchExpr = loopBranchExpr; elseBranchExpr = Some elseBranchExpr}, tokensAfterElseBranch)
                            | _ ->
                                Error {Token = Seq.head tokensAfterElse; Message = "else branch must has an expression"}
                        | _ ->
                            Ok (While { conditionExpr = conditionExpr; thenBranchExpr = loopBranchExpr; elseBranchExpr = None}, tokensAfterLoopBranch)
                    | _ ->
                        Error { Token = doToken; Message = "while expression must has a loop expression" }
                else
                    Error { Token = doToken; Message = "while expression must has a loop expression" }
            | _ ->
                exprRes
        | _ ->
            Error { Token = firstToken; Message = "first token of while statement must be while" }
    and parseForExpression tokens =
        let firstToken = Seq.head tokens
        match firstToken.``type`` with
        | FOR ->
            let tokensAfterFor = Seq.tail tokens
            let exprRes = parseExpression tokensAfterFor
            match exprRes with
            | Ok (conditionInitExpr, tokensAfterConditionInitExpr) ->
                let tokensAfterConditionInitExpr =
                    tokensAfterConditionInitExpr
                    |> Seq.skipWhile (isType NEWLINE)
                let semicolonToken = Seq.head tokensAfterConditionInitExpr
                if semicolonToken.``type`` = SEMICOLON then
                    let tokensAfterConditionInitExpr =
                        Seq.tail tokensAfterConditionInitExpr
                        |> Seq.skipWhile (isType NEWLINE)
                    match parseExpression tokensAfterConditionInitExpr with
                    | Ok (conditionCheckExpr, tokensAfterConditionCheckExpr) ->
                        let tokensAfterConditionCheckExpr =
                            tokensAfterConditionCheckExpr
                            |> Seq.skipWhile (isType NEWLINE)
                        let semicolonToken = Seq.head tokensAfterConditionCheckExpr
                        if semicolonToken.``type`` = SEMICOLON then
                            let tokensAfterCheckExpr =
                                Seq.tail tokensAfterConditionCheckExpr
                                |> Seq.skipWhile (isType NEWLINE)
                            match parseExpression tokensAfterCheckExpr with
                            | Ok (conditionEndExpr, tokensAfterConditionEndExpr) ->
                                let forConditionExpr =
                                    { initExpr = conditionInitExpr
                                      checkExpr = conditionCheckExpr
                                      endExpr = conditionEndExpr }
                                let tokensAfterConditionEndExpr =
                                    tokensAfterConditionEndExpr
                                    |> Seq.skipWhile (isType NEWLINE)
                                let doToken = Seq.head tokensAfterConditionEndExpr
                                if doToken.``type`` = DO then
                                    let tokensAfterDoExpr =
                                        Seq.tail tokensAfterConditionEndExpr
                                        |> Seq.skipWhile (isType NEWLINE)
                                    match parseExpression tokensAfterDoExpr with
                                    | Ok (loopBranchExpr, tokensAfterLoopBranch) ->
                                        Ok (For { conditionExpr = forConditionExpr; loopBranchExpr = loopBranchExpr }, tokensAfterLoopBranch)
                                    | _ ->
                                        Error { Token = doToken; Message = "for expression must has a do expression"}
                                else
                                    Error { Token = doToken; Message = "for expression must has a do token"}
                            | _ ->
                                Error {Token = Seq.head tokensAfterCheckExpr; Message = "for expression must has an end expression"}
                        else
                            Error { Token = semicolonToken; Message = "for check expression must has semicolon as seperator" }
                    | _ ->
                        Error { Token = Seq.head tokensAfterConditionInitExpr; Message = "for expression must has a check expression" }
                else
                    Error { Token = semicolonToken; Message = "for init expression must has semicolon as seperator" }
            | _ ->
                exprRes
        | _ ->
            Error { Token = firstToken; Message = "first token of for statement must be while" }
    and parseExpression tokens =
        let token = Seq.head tokens
        match token.``type`` with
        | IF ->
            parseIfExpression tokens
        | LET ->
            parseLetExpression tokens
        | WHILE ->
            parseWhileExpression tokens
        | FOR ->
            parseForExpression tokens
        | IDENTIFIER ->
            let tokensAfterIdentifier = Seq.tail tokens
            let nextToken = Seq.head tokensAfterIdentifier
            match nextToken.``type`` with
            | MINUS
            | PLUS
            | EQUAL
            | LESS
            | LESS_EQUAL
            | GREATER
            | GREATER_EQUAL
            | MULTPLY ->
                let tokensAfterOperator = Seq.tail tokensAfterIdentifier
                match parseExpression tokensAfterOperator with
                | Ok (Lateral _ as rightExpr, leftTokens)
                | Ok (Identifier _ as rightExpr, leftTokens)
                | Ok (Binary _ as rightExpr, leftTokens) ->
                    Ok (Binary { operator = nextToken; leftExpr = Identifier token; rightExpr = rightExpr }, leftTokens)
                | _ ->
                    Error {Token = nextToken; Message = "operator expect a identifier or lateral or binary expression"}
            | NEWLINE
            | THEN
            | DO
            | SEMICOLON ->
                Ok (Identifier token, tokensAfterIdentifier)
            | _ ->
                Error { Token = token; Message = "expect operator or semicolon" }
        | STRING ->
            Ok (Lateral token, Seq.tail tokens)
        | NUMBER ->
            Ok (Lateral token, Seq.tail tokens)
        | BOOLEAN ->
            Ok (Lateral token, Seq.tail tokens)
        | EQUAL ->
            Ok (Lateral token, Seq.tail tokens)
        | NEWLINE ->
            Ok (Empty, Seq.tail tokens)
        | SEMICOLON ->
            Ok (Empty, Seq.tail tokens)
        | RETURN ->
            Ok (Return, Seq.tail tokens)
        | _ ->
            Error { Token = token; Message = "invalid token" }

    let rec parse tokens exprs =
        let parseRes = parseExpression tokens
        match parseRes with
        | Ok (expr, leftTokens) ->
            let newExprs =
                match expr with
                | Empty -> exprs
                | _ -> Seq.append exprs [expr]
            if Seq.isEmpty leftTokens then
                Ok { Exprs = newExprs }
            else
                parse leftTokens newExprs
        | Error error ->
            Error error