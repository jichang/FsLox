namespace FsLox

open Parser

module Interpreter =
    let rec eval (env, value) (expr: Expr) =
        (env, value)

    let interprete env (ast: AST) =
        Seq.fold eval (env, None) ast.Exprs