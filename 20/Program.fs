open Common.Functions

let isInBounds (map: list<list<char>>) (row, col) =
    row >= 0 && row < map.Length && col >= 0 && col < map[0].Length

let isOnPath (map: list<list<char>>) (row, col) =
    map[row][col] = '.' || map[row][col] = 'S' || map[row][col] = 'E'

let findDistances (map: list<list<char>>) =
    let nextPathPos (pathSoFar: list<int * int>) =
        let findAdjacent (row, col) =
            [ (row + 1, col); (row - 1, col); (row, col + 1); (row, col - 1) ]
            |> List.filter (isInBounds map)

        let currPos = pathSoFar[pathSoFar.Length - 1]

        let prevPos =
            if pathSoFar.Length >= 2 then
                pathSoFar[pathSoFar.Length - 2]
            else
                (-1, -1)

        findAdjacent currPos
        |> List.filter (isOnPath map)
        |> List.filter ((<>) prevPos)
        |> List.head

    let pathLength =
        [ for row in [ 0 .. map.Length - 1 ] do
              for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
        |> List.filter (isOnPath map)
        |> List.length

    let startPos =
        [ for row in [ 0 .. map.Length - 1 ] do
              for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
        |> List.filter (fun (row, col) -> map[row][col] = 'S')
        |> List.head

    List.fold
        (fun (pathSoFar: list<int * int>) _ -> pathSoFar @ [ nextPathPos pathSoFar ])
        [ startPos ]
        [ 0 .. pathLength - 2 ]
    |> List.rev
    |> List.indexed
    |> List.map (fun (dist, pos) -> pos, dist)
    |> Map.ofList

let rec countCheats (map: list<list<char>>) (maxCheatDist: int) (minTimeSaved: int) =
    let distances = findDistances map

    let countCheatEnds (cheatStartRow, cheatStartCol) =
        let timeSaved (cheatEndRow, cheatEndCol) =
            let cheatDist =
                (abs (cheatEndRow - cheatStartRow)) + (abs (cheatEndCol - cheatStartCol))

            distances[(cheatStartRow, cheatStartCol)]
            - distances[(cheatEndRow, cheatEndCol)]
            - cheatDist

        [ for rowDiff in [ -maxCheatDist .. maxCheatDist ] do
              for colDiff in [ -(maxCheatDist - (abs rowDiff)) .. (maxCheatDist - (abs rowDiff)) ] -> rowDiff, colDiff ]
        |> List.map (fun (rowDiff, colDiff) -> cheatStartRow + rowDiff, cheatStartCol + colDiff)
        |> List.filter (isInBounds map)
        |> List.filter (isOnPath map)
        |> List.filter (fun cheatEnd -> timeSaved cheatEnd >= minTimeSaved)
        |> List.length

    Map.keys distances |> Seq.map countCheatEnds |> Seq.sum

let map = readlines "input.dat" |> List.map Seq.toList
countCheats map 2 100 |> part1
countCheats map 20 100 |> part2
