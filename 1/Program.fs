let readInput fileName =
    let parseNums line =
        let pattern = @"(\d+)\s+(\d+)"
        let matches = System.Text.RegularExpressions.Regex.Match (line, pattern)
        (int (string matches.Groups[1]), int (string matches.Groups[2]))
    System.IO.File.ReadLines(fileName) |> List.ofSeq  |> List.map parseNums |> List.unzip 

let similarity left right =
    let count lst n = List.filter ((=) n) lst |> List.length
    List.map (fun n -> n * (count right n)) left

let left, right = readInput "input.dat"

// part 1
List.zip (List.sort left) (List.sort right) |> List.map (fun (a, b) -> abs (a - b)) |> List.sum |> printfn "%A"

// part 2
similarity left right |> List.sum |> printfn "%A"