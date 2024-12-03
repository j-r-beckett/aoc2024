namespace Common

open System.IO

module Functions =

    let part1 answer = printfn "part 1: %A" answer

    let part2 answer = printfn "part 2: %A" answer

    let readlines filename = File.ReadLines(filename) |> List.ofSeq
