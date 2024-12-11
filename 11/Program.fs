open Common.Functions

let readStones filename =
    readlines filename
    |> List.head
    |> _.Split(' ')
    |> Array.toList
    |> List.map int64

let evolveSingleStone (stone: int64) : list<int64> =
    if stone = 0 then
        [ 1 ]
    else
        let s = string stone

        if s.Length % 2 = 0 then
            [ int s[.. s.Length / 2 - 1]; int s[s.Length / 2 ..] ]
        else
            [ stone * 2024L ]

let rec evolve (n: int) (stones: list<int64>) =
    if n = 0 then
        stones
    else
        stones |> List.map evolveSingleStone |> List.concat |> evolve (n - 1)

let rec fastEvolve (n: int) (stones: Map<int64, int64>) =
    let evolveAllStoneInstances (stone: int64, count: int64) : list<int64 * int64> =
        let additiveDiffs =
            evolveSingleStone stone |> List.map (fun stone -> (stone, count))

        [ (stone, -count) ] @ additiveDiffs

    let applyDiff (stones: Map<int64, int64>) (stone: int64, diff: int64) : Map<int64, int64> =
        Map.change
            stone
            (fun numStones ->
                match numStones with
                | Some n -> n + diff |> Some
                | None -> Some diff)
            stones

    if n = 0 then
        stones
    else
        stones
        |> Map.toList
        |> List.map (fun (stone, count) -> evolveAllStoneInstances (stone, count))
        |> List.concat
        |> List.fold applyDiff stones
        |> Map.filter (fun _ count -> count > 0)
        |> fastEvolve (n - 1)

let countStones evolutions stones = evolve evolutions stones |> List.length

let fastCountStones evolutions stones =
    let createStoneMap (stones: list<int64>) =
        stones
        |> List.groupBy (fun stone -> stone)
        |> List.map (fun (a, b) -> a, int64 b.Length)
        |> Map.ofList

    stones |> createStoneMap |> fastEvolve evolutions |> Map.values |> Seq.sum

let stones = readStones "input.dat"
stones |> countStones 25 |> part1
stones |> fastCountStones 75 |> part2
