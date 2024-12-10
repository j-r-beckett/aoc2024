open Common.Functions

let readMap filename =
    readlines filename |> List.map Seq.toList |> List.map (List.map (string >> int))

let adjacent (row, col) (map: list<list<int>>) =
    let inBounds (numRows, numCols) (row, col) =
        row >= 0 && row < numRows && col >= 0 && col < numCols

    [ (row - 1, col); (row + 1, col); (row, col - 1); (row, col + 1) ]
    |> List.filter (inBounds (map.Length, map[0].Length))

let calculateScore (map: list<list<int>>) (trailhead: int * int) =
    let rec find9s (currentElevation: int) (currRow, currCol) =
        if map[currRow][currCol] = 9 then
            Set.ofList [ (currRow, currCol) ]
        else
            adjacent (currRow, currCol) map
            |> List.filter (fun (nextRow, nextCol) -> map[nextRow][nextCol] = currentElevation + 1)
            |> List.map (find9s (currentElevation + 1))
            |> Set.unionMany

    find9s 0 trailhead |> Set.count

let calculateRating (map: list<list<int>>) (trailhead: int * int) =
    let rec findTrails (trailSoFar: list<int * int>) =
        let currRow, currCol = trailSoFar[trailSoFar.Length - 1]

        if map[currRow][currCol] = 9 then
            Set.ofList [ trailSoFar @ [ currRow, currCol ] ]
        else
            adjacent (currRow, currCol) map
            |> List.filter (fun (nextRow, nextCol) -> map[nextRow][nextCol] = map[currRow][currCol] + 1)
            |> List.map (fun (nextRow, nextCol) -> (findTrails (trailSoFar @ [ nextRow, nextCol ])))
            |> Set.unionMany

    findTrails [ trailhead ] |> Set.count

let sumCounter (counter: list<list<int>> -> int * int -> int) (map: list<list<int>>) =
    [ for row in 0 .. map.Length - 1 do
          for col in 0 .. map[0].Length - 1 -> row, col ]
    |> List.filter (fun (row, col) -> map[row][col] = 0)
    |> List.map (counter map)
    |> List.sum

let map = readMap "input.dat"
map |> sumCounter calculateScore |> part1
map |> sumCounter calculateRating |> part2
