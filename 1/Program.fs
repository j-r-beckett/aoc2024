open Common.Functions
open System.Text.RegularExpressions

let readInput filename =
    let parseNums line =
        let pattern = @"(\d+)\s+(\d+)"
        let matches = Regex.Match (line, pattern)
        (int (string matches.Groups[1]), int (string matches.Groups[2]))
    readlines filename |> List.map parseNums |> List.unzip 

let similarity left right =
    let count lst n = List.filter ((=) n) lst |> List.length
    List.map (fun n -> n * (count right n)) left

let left, right = readInput "input.dat"

List.zip (List.sort left) (List.sort right) |> List.map (fun (a, b) -> abs (a - b)) |> List.sum |> part1
similarity left right |> List.sum |> part1