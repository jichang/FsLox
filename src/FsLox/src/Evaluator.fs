namespace FsLox

open System
open Scanner
open Parser

module Evaluator =
    type Value =
        | Integer of int64
        | String of string
        | Boolean of bool
        | Void

    type Env =
        { parent: Env option
          vars: Map<string, Value>}

        static member empty =
            { parent = None; vars = Map.empty }

    let rec eval (env: Env) (expr: Expr) =
        match expr with
        | Lateral token ->
            match token.``type`` with
            | TokenType.NUMBER ->
                let value = Int64.Parse token.lexeme
                Ok (Integer value, env)
            | TokenType.STRING ->
                Ok (String token.lexeme, env)
            | TokenType.BOOLEAN ->
                if token.lexeme = "true" then
                    Ok (Boolean true, env)
                else
                    Ok (Boolean false, env)
            | _ ->
                Error "invalid lateral token type"
        | Identifier token ->
            match Map.tryFind token.lexeme env.vars with
            | Some value -> Ok (value, env)
            | None -> Error "identifier not exist"
        | Binary expr ->
            let leftOperand = eval env expr.leftExpr
            match leftOperand with
            | Ok (Integer leftOperandVal, env) ->
                let rightOperand = eval env expr.rightExpr
                match rightOperand with
                | Ok (Integer rightOperandVal, env) ->
                    match expr.operator.``type`` with
                    | TokenType.MINUS ->
                        Ok (Integer (leftOperandVal - rightOperandVal), env)
                    | TokenType.PLUS ->
                        let value = leftOperandVal + rightOperandVal
                        Ok (Integer (leftOperandVal + rightOperandVal), env)
                    | TokenType.MULTPLY ->
                        Ok (Integer (leftOperandVal * rightOperandVal), env)
                    | TokenType.EQUAL ->
                        Ok (Boolean (leftOperandVal = rightOperandVal), env)
                    | TokenType.LESS ->
                        Ok (Boolean (leftOperandVal < rightOperandVal), env)
                    | TokenType.LESS_EQUAL ->
                        Ok (Boolean (leftOperandVal <= rightOperandVal), env)
                    | TokenType.GREATER ->
                        Ok (Boolean (leftOperandVal > rightOperandVal), env)
                    | TokenType.GREATER_EQUAL ->
                        Ok (Boolean (leftOperandVal >= rightOperandVal), env)
                    | _ ->
                        Error "unsupport binary operator"
                | _ ->
                    Error "right operand is invalid"
            | _ ->
                Error "left operand is invalid"
        | Let expr ->
            match expr.targetExpr with
            | Identifier token ->
                match eval env expr.sourceExpr with
                | Ok (value, env) ->
                    let newEnv = { env with vars = Map.add token.lexeme value env.vars}
                    Ok (Void, newEnv)
                | Error errmsg ->
                    Error ("let should has valid source expression: " + errmsg)
            | _ ->
                Error "let should has identifier as target"
        | If expr ->
            match eval env expr.conditionExpr with
            | Ok (Boolean condition, env) ->
                let resultExpr =
                    if condition then
                        expr.thenBranchExpr
                    else
                        match expr.elseBranchExpr with
                        | Some expr -> expr
                        | None -> Empty
                match eval env resultExpr with
                | Ok (value, env) ->
                    Ok (value, env)
                | Error errmsg ->
                    Error ""
            | _ ->
                Error "if expression need a Boolean expression"
        | While expr ->
            Ok (Void, env)
        | For expr ->
            Ok (Void, env)
        | Fun expr ->
            Ok (Void, env)
        | Apply expr ->
            Ok (Void, env)
        | Return ->
            Ok (Void, env)
        | Empty ->
            Ok (Void, env)