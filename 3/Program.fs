type Token =
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

    lines |> List.map extractExprs |> List.map tokenizeExprs  |> List.concat


let rec evaluateAlwaysEnabled tokens =
    match tokens with 
    | head::tail -> match head with
                    | Multiplication (n, m) -> n * m + evaluateAlwaysEnabled tail
                    | _ -> evaluateAlwaysEnabled tail
    | [] -> 0

let rec evaluate enabled tokens =
    match tokens with
    | head::tail -> match head with
                    | Multiplication (n, m) -> (if enabled then n * m else 0) + evaluate enabled tail 
                    | Enable -> evaluate true tail 
                    | Disable -> evaluate false tail 
    | [] -> 0


let tokens = System.IO.File.ReadLines("input.dat") |> List.ofSeq |> tokenize
tokens |> evaluateAlwaysEnabled |> printfn "%A"
tokens |> evaluate true |> printfn "%A"