let rec reverse lst = 
    match lst with
    | head::tail -> (reverse tail) @ [head]
    | [] -> []

let rec firstDigit chars = 
    match chars with
    | head::tail -> if System.Char.IsDigit head then string head else firstDigit tail
    | [] -> invalidOp "no digits"

let calibrationVal chars = int (firstDigit chars + (firstDigit (reverse chars)))



System.IO.File.ReadLines("input.dat") |> List.ofSeq |> List.map Seq.toList |> List.map calibrationVal |> List.sum |> printfn "%A"