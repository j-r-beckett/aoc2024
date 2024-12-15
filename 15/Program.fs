open Common.Functions

type Direction = int * int

let up = -1, 0
let right = 0, 1
let down = 1, 0
let left = 0, -1

let (+!) (aRow, aCol) (bRow, bCol) = aRow + bRow, aCol + bCol

let mapAt (map: array<array<char>>) pos = map[fst pos][snd pos]

let readMap filename =
    let map =
        readlines filename
        |> List.map Seq.toList
        |> List.filter (fun line -> not line.IsEmpty && line[0] = '#')
        |> List.map List.toArray
        |> List.toArray

    let robotRow, robotCol =
        [ for row in [ 0 .. map.Length - 1 ] do
              for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
        |> List.filter (fun (row, col) -> map[row][col] = '@')
        |> List.head

    Array.set map[robotRow] robotCol '.'
    map, (robotRow, robotCol)

let readDirections filename =
    let direction (c: char) =
        match c with
        | '^' -> Some up
        | '>' -> Some right
        | 'v' -> Some down
        | '<' -> Some left
        | _ -> None

    readlines filename
    |> List.map Seq.toList
    |> List.concat
    |> List.map direction
    |> List.choose id

let readInput filename =
    let map, robotPos = readMap filename
    map, readDirections filename, robotPos


let printMap ((map: array<array<char>>), robotPos) =
    for row in [ 0 .. map.Length - 1 ] do
        for col in [ 0 .. map[0].Length - 1 ] do
            if (row, col) = robotPos then
                printf "@"
            else
                map[row][col] |> string |> printf "%s"

        printfn ""


let move tryPushBoxesFn (map, robotPos) direction =
    tryPushBoxesFn map (robotPos +! direction) direction

    if mapAt map (robotPos +! direction) = '.' then
        map, (robotPos +! direction)
    else
        map, robotPos

let findGps (boxRow, boxCol) = (100 * boxRow) + boxCol 


let solve isBoxFn tryPushBoxesFn (enbiggenFn: array<array<char>> -> array<array<char>>) filename =
    let map, directions, robotPos = readInput filename
    directions |> List.fold (move tryPushBoxesFn) (map, robotPos) |> ignore // mutates map
    let embiggenedMap = enbiggenFn map

    [ for row in [ 0 .. embiggenedMap.Length - 1 ] do
        for col in [ 0 .. embiggenedMap[0].Length - 1 ] -> row, col ]
    |> List.filter (isBoxFn embiggenedMap)
    |> List.map findGps
    |> List.sum

let solvePart1 filename =
    let rec tryPushBoxes map (boxRow, boxCol) direction =
        let rec findLastBoxPos currBoxPos =
            let nextBoxPos = (currBoxPos +! direction)

            match mapAt map nextBoxPos with
            | 'O' -> findLastBoxPos nextBoxPos
            | '.' -> Some nextBoxPos
            | '#' -> None
            | _ -> raise (System.ArgumentException "Unknown map value")

        if map[boxRow][boxCol] <> 'O' then
            ()
        else
            match findLastBoxPos (boxRow, boxCol) with
            | Some(lastBoxRow, lastBoxCol) ->
                Array.set map[boxRow] boxCol '.'
                Array.set map[lastBoxRow] lastBoxCol 'O'
            | None -> ()


    let isBox (map: array<array<char>>) (row, col) = map[row][col] = 'O'

    solve isBox tryPushBoxes id filename


let solvePart2 filename =
    let rec tryPushBoxes map (boxRow, boxCol) direction =
        let rec findLastBoxPos currBoxPos =
            let nextBoxPos = (currBoxPos +! direction)

            match mapAt map nextBoxPos with
            | 'O' -> findLastBoxPos nextBoxPos
            | '.' -> Some nextBoxPos
            | '#' -> None
            | _ -> raise (System.ArgumentException "Unknown map value")

        if map[boxRow][boxCol] <> 'O' then
            ()
        else
            match findLastBoxPos (boxRow, boxCol) with
            | Some(lastBoxRow, lastBoxCol) ->
                Array.set map[boxRow] boxCol '.'
                Array.set map[lastBoxRow] lastBoxCol 'O'
            | None -> ()


    let isBox (map: array<array<char>>) (row, col) = map[row][col] = 'O'

    solve isBox tryPushBoxes id filename


solvePart1 "input.dat" |> part1
solvePart2 "input.dat" |> part2