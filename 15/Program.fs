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

    map

let findRobot (map: array<array<char>>) =
    [ for row in [ 0 .. map.Length - 1 ] do
          for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
    |> List.filter (fun (row, col) -> map[row][col] = '@')
    |> List.head

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

let printMap ((map: array<array<char>>), robotPos) =
    for row in [ 0 .. map.Length - 1 ] do
        for col in [ 0 .. map[0].Length - 1 ] do
            if (row, col) = robotPos then
                printf "@"
            else
                map[row][col] |> string |> printf "%s"

        printfn ""

let gpsValue (map: array<array<char>>) (boxRow, boxCol) = 
    if map[boxRow][boxCol] = 'O' || map[boxRow][boxCol] = '['
    then (100 * boxRow) + boxCol
    else 0

let solve moveFn (map: array<array<char>>) (directions: list<int * int>) =
    let robotRow, robotCol = findRobot map
    Array.set map[robotRow] robotCol '.'

    directions
    |> List.fold moveFn (map, (robotRow, robotCol))
    |> ignore // mutates map

    [ for row in [ 0 .. map.Length - 1 ] do
          for col in [ 0 .. map[0].Length - 1 ] -> row, col ]
    |> List.map (gpsValue map)
    |> List.sum

let solvePart1 filename =
    let move (map: array<array<char>>, robotPos) direction =
        let rec canPushBoxes (boxRow, boxCol) =
            let nextBoxRow, nextBoxCol = (boxRow, boxCol) +! direction
            
            if map[nextBoxRow][nextBoxCol] = '.'
            then true
            else if map[nextBoxRow][nextBoxCol] = '#'
            then false
            else canPushBoxes (nextBoxRow, nextBoxCol)

        let rec pushBoxes (boxRow, boxCol) = 
            let nextBoxRow, nextBoxCol = (boxRow, boxCol) +! direction

            let push () =
                Array.set map[nextBoxRow] nextBoxCol 'O'
                Array.set map[boxRow] boxCol '.'
            
            if map[nextBoxRow][nextBoxCol] = '.'
            then 
                push()
            else 
                pushBoxes (nextBoxRow, nextBoxCol)
                push()


        let nextPossibleRobotPos = robotPos +! direction
        if mapAt map nextPossibleRobotPos = '.'
        then map, nextPossibleRobotPos
        else if mapAt map nextPossibleRobotPos = 'O' && canPushBoxes nextPossibleRobotPos
        then 
            pushBoxes nextPossibleRobotPos
            map, nextPossibleRobotPos
        else
            map, robotPos

    solve move (readMap filename) (readDirections filename)


let solvePart2 filename =
    let move (map: array<array<char>>, robotPos) direction = 
        let rowDir, colDir = direction

        let rec canPushBoxesHorizontally (boxRow, boxCol) =
            let nextBoxRow, nextBoxCol = (boxRow, boxCol) +! direction +! direction
            
            if map[nextBoxRow][nextBoxCol] = '.'
            then true
            else if map[nextBoxRow][nextBoxCol] = '#'
            then false
            else canPushBoxesHorizontally (nextBoxRow, nextBoxCol)

        let rec pushBoxesHorizontally (boxRow, boxCol) = 
            let push () =
                Array.set map[boxRow] (boxCol + 2 * colDir) (map[boxRow][boxCol + colDir])
                Array.set map[boxRow] (boxCol + colDir) (map[boxRow][boxCol])
                Array.set map[boxRow] boxCol '.'
            
            if map[boxRow][boxCol + 2 * colDir] = '.'
            then 
                push()
            else 
                pushBoxesHorizontally (boxRow, boxCol + 2 * colDir)
                push()

        let rec canPushBoxesVertically (boxRow, boxCol) =
            let helper (row, col) =
                map[row][col] = '.' || ((map[row][col] = '[' || map[row][col] = ']') && canPushBoxesVertically (row, col))
            if map[boxRow][boxCol] = '['
            then canPushBoxesVertically (boxRow, boxCol + 1)
            else helper (boxRow + rowDir, boxCol) && helper (boxRow + rowDir, boxCol - 1)


        let rec pushBoxesVertically (boxRow, boxCol) =
            let push () =
                Array.set map[boxRow + rowDir] boxCol ']'
                Array.set map[boxRow + rowDir] (boxCol - 1) '['
                Array.set map[boxRow] boxCol '.'
                Array.set map[boxRow] (boxCol - 1) '.'

            if map[boxRow][boxCol] = '['
            then pushBoxesVertically (boxRow, boxCol + 1)
            else if map[boxRow][boxCol] = ']'
                then
                    if map[boxRow + rowDir][boxCol] = ']' || map[boxRow + rowDir][boxCol] = '['
                    then
                        pushBoxesVertically (boxRow + rowDir, boxCol)
                    if map[boxRow + rowDir][boxCol - 1] = ']'
                    then
                        pushBoxesVertically (boxRow + rowDir, boxCol - 1)
                    push()

        let nextPossibleRobotPos = robotPos +! direction
        if mapAt map nextPossibleRobotPos = '.'
        then map, nextPossibleRobotPos
        else if (mapAt map nextPossibleRobotPos = '[' || mapAt map nextPossibleRobotPos = ']') 
        then
            if fst direction = 0 && canPushBoxesHorizontally nextPossibleRobotPos
            then 
                pushBoxesHorizontally nextPossibleRobotPos
                map, nextPossibleRobotPos
            else if fst direction <> 0 && canPushBoxesVertically nextPossibleRobotPos
            then
                pushBoxesVertically nextPossibleRobotPos
                map, nextPossibleRobotPos
            else
                map, robotPos
        else map, robotPos

    let map = readMap filename

    let embiggenedMap = Array.create map.Length Array.empty

    for rowIndex in [ 0 .. embiggenedMap.Length - 1 ] do
        Array.set embiggenedMap rowIndex (Array.create (map[0].Length * 2) '.')

    for row in [ 0 .. map.Length - 1 ] do
        for col in [ 0 .. map[0].Length - 1 ] do
            if map[row][col] = 'O' then
                Array.set embiggenedMap[row] (col * 2) '['
                Array.set embiggenedMap[row] (col * 2 + 1) ']'
            else if map[row][col] = '#' then
                Array.set embiggenedMap[row] (col * 2) '#'
                Array.set embiggenedMap[row] (col * 2 + 1) '#'
            else if map[row][col] = '@' then
                Array.set embiggenedMap[row] (col * 2) '@'

    solve move embiggenedMap (readDirections filename)


let filename = "input.dat"
solvePart1 filename |> part1
solvePart2 filename |> part2
