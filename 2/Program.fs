let parseInput fileName =
    let lines = System.IO.File.ReadLines(fileName) |> List.ofSeq
    let parseLevel (report : string) = report |> _.Split(' ') |> List.ofArray |> List.map int
    lines |> List.map parseLevel

type Mode =
    | Increasing
    | Decreasing

let isLevelSafe first second (mode : Mode) =
    match mode with
    | Increasing -> second > first && second - first <= 3
    | Decreasing -> first > second && first - second <= 3

let isSafe (level : int list) = 
    let pairs = List.zip level[..level.Length - 2] level[1..]
    (List.fold (fun soFar (a, b) -> soFar && isLevelSafe a b Increasing) true pairs) || (List.fold (fun soFar (a, b) -> soFar && isLevelSafe a b Decreasing) true pairs)

let allPossibleLevels (baseLevel : int list) =
    seq {
        for i in 0..baseLevel.Length do
            yield baseLevel[..i - 1] @ baseLevel[i + 1..]
    }

let isAnySafe (level : int list) =
    let safes = Seq.map isSafe (allPossibleLevels level) |> Seq.filter id |> List.ofSeq
    safes.Length > 0

let levels = parseInput "input.dat"
// allPossibleLevels levels[0]  |> List.ofSeq |> printfn "%A"
levels |> List.map isSafe |> List.filter id |> _.Length |> printfn "%A"
levels |> List.map isAnySafe |> List.filter id |> _.Length |> printfn "%A"