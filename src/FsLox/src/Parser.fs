namespace FxLox

module Parser =
    open FsLox.Scanner

    type UnaryOper =
        | Plus of Token
        | Minus of Token
        | Multply of Token

    type Expr =
        | Unary of UnaryOper * Expr
        | Binary of Expr * Expr * Token

    type AST =
        { Exprs: Expr seq }

    let parse (tokens : Token seq) : Expr seq =
        Seq.empty