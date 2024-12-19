﻿open Common.Functions
open System.Collections.Generic

let readInput filename =
    let lines = readlines filename

    let patterns = lines[0] |> _.Split(", ") |> Seq.toList
    let designs = lines[2..]
    patterns, designs

let rec canConstructDesign (patterns: list<string>) (design: string) =
    let rec helper designs =
        match designs with
        | [] -> false
        | head :: tail -> canConstructDesign patterns head || helper tail

    if design.Length = 0 then
        true
    else
        patterns
        |> List.filter (fun pattern -> design.StartsWith(pattern))
        |> List.map (fun pattern -> design[pattern.Length ..])
        |> helper


let waysToBuildDesign (patterns: list<string>) (design: string) =
    let cache = new Dictionary<string, int64>()

    let rec countWays (design: string) =
        let rec helper designs =
            match designs with
            | [] -> 0L
            | head :: tail -> (countWays head) + (helper tail)

        if cache.ContainsKey(design) then
            cache[design]
        else if design.Length = 0 then
            1L
        else
            let ways =
                patterns
                |> List.filter (fun pattern -> design.StartsWith(pattern))
                |> List.map (fun pattern -> design[pattern.Length ..])
                |> helper

            cache.Add(design, ways) |> ignore
            ways

    countWays design


let patterns, designs = readInput "input.dat"

designs
|> List.map (canConstructDesign patterns)
|> List.filter id
|> List.length
|> part1

designs |> List.map (waysToBuildDesign patterns) |> List.sum |> part2
