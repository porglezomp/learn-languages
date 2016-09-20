module Sexp

type Sexp =
    | Atom of string
    | Str of string
    | Num of double
    | List of Sexp list

exception UnexpectedItems of string * string
exception UnexpectedChar of char

let parse str =
    let rec parseList acc = function
        | (')' :: xs) -> List.rev acc, xs
        | [] -> List.rev acc, []
        | xs ->
            let (res, rest) = parse' xs
            in parseList (res :: acc) rest
    and parseNumber acc dec =
        let finish acc xs =
            let res = acc |> List.rev |> List.toArray |> System.String
                          |> System.Double.Parse
            in Sexp.Num res, xs
        in function
            | (c :: xs) when System.Char.IsDigit c -> parseNumber (c :: acc) dec xs
            | ('.' :: xs) when not dec -> parseNumber ('.' :: acc) true xs
            | (c :: xs) when System.Char.IsWhiteSpace c -> finish acc xs
            | ('(' :: xs) -> finish acc ('(' :: xs)
            | (')' :: xs) -> finish acc (')' :: xs)
            | [] -> finish acc []
            | (c :: xs) -> raise <| UnexpectedChar c
    and parseIdent acc =
        let finish acc xs = List.rev acc |> List.toArray |> System.String, xs
        in function
            | ('(' :: xs) -> finish acc <| '(' :: xs
            | (')' :: xs) -> finish acc <| ')' :: xs
            | (c :: xs) when System.Char.IsWhiteSpace c ->
                finish acc xs
            | [] -> finish acc []
            | (c :: xs) -> parseIdent (c :: acc) xs
    and parse' = function
        | ('(' :: xs) ->
            let (res, rest) = parseList [] xs
            in (Sexp.List(res), rest)
        | [] -> Sexp.List([]), []
        | (c :: xs) when System.Char.IsWhiteSpace c -> parse' xs
        | (c :: xs) when System.Char.IsDigit c -> parseNumber [] false (c :: xs)
        | ('.' :: xs) -> parseNumber ['.'] true xs
        | xs ->
            let (res, rest) = parseIdent [] xs
            in Sexp.Atom(res), rest
    in match Seq.toList str |> parse' with
        | (res, []) -> res
        | (_, rest) -> raise <| UnexpectedItems ("Unexpected items",
                                                 List.toArray rest |> System.String)
