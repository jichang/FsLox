namespace FsLox

open Parser

module Interpreter =
    let rec eval (env, value) expr =
        (env, value)

    let interprete env ast =
        Seq.fold eval (env, None) ast.Exprs