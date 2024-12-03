﻿type Token =
| Multiplication of n: int * m: int 
| Enable
| Disable

let tokenize lines = 
    let extractExprs line =
        let pattern = @"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)"
        let matches = System.Text.RegularExpressions.Regex.Matches (line, pattern)
        matches |> List.ofSeq |> List.map string  

    let tokenizeExprs exprs =
        let tokenizeExpr expr =
            let parseMultiplication multExpr =
                let pattern = @"mul\((\d{1,3}),(\d{1,3})\)"
                let matches = System.Text.RegularExpressions.Regex.Match (multExpr, pattern)
                Multiplication(int (string matches.Groups[1]), int (string matches.Groups[2]))
            
            match expr with
            | "do()" -> Enable
            | "don't()" -> Disable
            | _ -> parseMultiplication expr

        List.map tokenizeExpr exprs

    lines |> List.map (extractExprs >> tokenizeExprs) |> List.concat

let rec evaluate canDisable tokens =
    let helper (sum, enabled) token =
        match token with
        | Multiplication (n, m) -> sum + (if enabled then n * m else 0), enabled
        | Enable -> sum, true
        | Disable -> sum, false || (not canDisable)
    fst (List.fold helper (0, true) tokens)

let tokens = System.IO.File.ReadLines("input.dat") |> List.ofSeq |> tokenize
tokens |> evaluate false |> printfn "%A"  // part 1
tokens |> evaluate true |> printfn "%A"  // part 2