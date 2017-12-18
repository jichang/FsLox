namespace FsLox

module Interpreter =
    let eval (env, oldValue) expr =
        match Evaluator.eval env expr with
        | Ok (newValue, newEnv) ->
            (newEnv, newValue)
        | _ ->
            (env, oldValue)

    let interprete env (ast: Parser.AST) =
        Seq.fold eval (Evaluator.Env.empty, Evaluator.Value.Void) ast.Exprs