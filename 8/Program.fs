open Common.Functions

type Antenna =
    { Position: int * int; Frequency: char }

type Map = list<list<char>>

let readMap filename : Map =
    readlines filename |> List.map Seq.toList

let findAntennas (map: Map) =
    [ for row in [ 0 .. map.Length - 1 ] do
          for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
    |> List.filter (fun (row, col) -> map[row][col] <> '.')
    |> List.map (fun (row, col) ->
        { Position = row, col
          Frequency = map[row][col] })


let isInBounds (numRows, numCols) (row, col) =
    row >= 0 && row < numRows && col >= 0 && col < numCols


let calculateAntinodes (row1, col1) (row2, col2) (bounds: int * int) =
    let deltaRow, deltaCol = row1 - row2, col1 - col2

    [ (row1 + deltaRow, col1 + deltaCol); (row2 - deltaRow, col2 - deltaCol) ]
    |> List.filter (isInBounds bounds)


let calculateResonantAntinodes (row1, col1) (row2, col2) (bounds: int * int) =
    let deltaRow, deltaCol = row1 - row2, col1 - col2

    let rec helper (row: int, col: int) (direction: int) =
        let antinode = row + deltaRow * direction, col + deltaCol * direction

        match isInBounds bounds antinode with
        | true -> [ antinode ] @ helper antinode direction
        | false -> []

    helper (row1, col1) 1 @ helper (row2, col2) -1 @ [ (row1, col1); (row2, col2) ]


let rec calculateAntinodesOfFreq (bounds: int * int) f (positions: list<int * int>) =
    match positions with
    | [] -> Set.empty
    | headPosition :: tailPositions ->
        tailPositions
        |> List.map (fun tailPosition -> f headPosition tailPosition bounds)
        |> List.map Set.ofList
        |> Set.unionMany
        |> Set.union (calculateAntinodesOfFreq bounds f tailPositions)


let countAllAntinodes f (map: Map) =
    map
    |> findAntennas
    |> List.groupBy (fun antenna -> antenna.Frequency)
    |> List.map (fun (_, antennas) -> List.map (fun antenna -> antenna.Position) antennas)
    |> List.map (calculateAntinodesOfFreq (map.Length, map[0].Length) f)
    |> Set.unionMany
    |> Set.count


let map = readMap "input.dat"
map |> countAllAntinodes calculateAntinodes |> part1
map |> countAllAntinodes calculateResonantAntinodes |> part2
