open Common.Functions

let parseInput filename =
    readlines filename |> List.map Seq.toArray |> List.toArray


let rec moveGuard (map: array<array<char>>) (guardRow: int, guardCol: int) =
    let direction row col = match map[row][col] with
                            | '^' -> -1, 0
                            | '>' -> 0, 1
                            | 'v' -> 1, 0
                            | '<' -> 0, -1
                            | _ -> raise (System.InvalidOperationException "unkown guard direction")

    let isBlocked (row: int) (col: int) (direction: int * int) = map[row + (fst direction)][col + (snd direction)] = '#'

    let willStepOffMap row col direction = 
        let nextRow, nextCol = row + (fst direction), col + (snd direction)
        nextRow <= -1 || nextRow >= map.Length || nextCol <= -1 || nextCol >= map[0].Length

    let rotateGuard () =
        let newGuard = match map[guardRow][guardCol] with
                        | '^' -> '>'
                        | '>' -> 'v'
                        | 'v' -> '<'
                        | '<' -> '^'
                        | _ -> raise (System.InvalidOperationException "unkown guard direction")
        Array.set map[guardRow] guardCol newGuard

    let stepGuardForward () =
        let guardDirection = direction guardRow guardCol
        let newGuardRow, newGuardCol = guardRow + (fst guardDirection), guardCol + (snd guardDirection)
        // printfn "%A, %A" newGuardRow newGuardCol
        Array.set map[newGuardRow] newGuardCol (map[guardRow][guardCol])
        Array.set map[guardRow] guardCol 'X'
        newGuardRow, newGuardCol

    let guardDirection = direction guardRow guardCol
    if willStepOffMap guardRow guardCol guardDirection
    then 
        Array.set map[guardRow] guardCol 'X'
        map
    else 
        if isBlocked guardRow guardCol guardDirection
        then 
            rotateGuard () |> ignore
            moveGuard map (guardRow, guardCol) 
        else 
            moveGuard map (stepGuardForward ()) 


let rec isLoop (map: array<array<char>>) (guardRow: int, guardCol: int) (visited: Set<string>) =
    let direction row col = match map[row][col] with
                            | '^' -> -1, 0
                            | '>' -> 0, 1
                            | 'v' -> 1, 0
                            | '<' -> 0, -1
                            | _ -> raise (System.InvalidOperationException "unkown guard direction")

    let isBlocked (row: int) (col: int) (direction: int * int) = map[row + (fst direction)][col + (snd direction)] = '#'

    let willStepOffMap row col direction = 
        let nextRow, nextCol = row + (fst direction), col + (snd direction)
        nextRow <= -1 || nextRow >= map.Length || nextCol <= -1 || nextCol >= map[0].Length

    let rotateGuard () =
        let newGuard = match map[guardRow][guardCol] with
                        | '^' -> '>'
                        | '>' -> 'v'
                        | 'v' -> '<'
                        | '<' -> '^'
                        | _ -> raise (System.InvalidOperationException "unkown guard direction")
        Array.set map[guardRow] guardCol newGuard

    let stepGuardForward () =
        let guardDirection = direction guardRow guardCol
        let newGuardRow, newGuardCol = guardRow + (fst guardDirection), guardCol + (snd guardDirection)
        Array.set map[newGuardRow] newGuardCol (map[guardRow][guardCol])
        Array.set map[guardRow] guardCol '.'
        newGuardRow, newGuardCol

    let guardDirection = direction guardRow guardCol
    if willStepOffMap guardRow guardCol guardDirection
    then 
        false
    else 
        let key = (string guardRow) + (string guardCol) + (string (map[guardRow][guardCol]))
        if Set.contains  key visited
        then true
        else
            if isBlocked guardRow guardCol guardDirection
            then 
                rotateGuard () |> ignore
                isLoop map (guardRow, guardCol) visited
            else 
                isLoop map (stepGuardForward ()) (Set.add key visited)

let findGuard (map: array<array<char>>) =
    // let containsGuard row = Array.contains map[row] "^" || Array.contains map[row] ">" || Array.contains map[row] "v" || Array.contains map[row] "<"
    // let guardCol = Array.find
    // let helper row =
    //     if containsGuard row
    //     then row, Array.findIndex map[row] 
    let rec findGuardHelper row =
        let rec guardCol col =
            if col >= map[0].Length
            then None
            else
                match map[row][col] with
                    | '^' -> Some col
                    | '>' -> Some col
                    | 'v' -> Some col
                    | '<' -> Some col
                    | _ -> guardCol (col + 1)
        if row >= map.Length
        then None
        else match guardCol 0 with
                | Some col -> Some (row, col)
                | None -> findGuardHelper (row + 1)
    findGuardHelper 0

let countXs (map: array<array<char>>) = 
    let countXsInRow row = map[row] |> Array.fold (fun sumSoFar c -> sumSoFar + (if c = 'X' then 1 else 0)) 0
    [0..map.Length - 1] |> List.map countXsInRow |> List.sum

let deepCopy (map: array<array<char>>) = map |> Array.map Array.copy

// let findLoops (map: array<array<char>>) (guardRow: int, guardCol: int)=
//     let setObstruction row col = Array.set map[row] col '#'
//     let unsetObstruction row col = Array.set map[row] col '.'
//     let rec loopsInRow startCol row  =
//         if startCol >= map[0].Length
//         then 0
//         else if map[row][startCol] = '.'
//         then
//             setObstruction row startCol
//             let hasLoop = isLoop (deepCopy map) (guardRow, guardCol) Set.empty
//             unsetObstruction row startCol 
//             (if hasLoop then 1 else 0) + (loopsInRow (startCol + 1) row)
//         else
//             loopsInRow (startCol + 1) row
//     [0..map.Length - 1] |> List.map (loopsInRow 0) |> List.sum

let countLoops (map: array<array<char>>) (guardRow: int, guardCol: int) (positions: list<int * int>) =
    let isLoopHelper position =
        let newMap = deepCopy map
        Array.set newMap[fst position] (snd position) '#'
        // printfn "Setting map at %i, %i to obstruction" (fst position) (snd position)
        // printfn "%A" newMap
        let res = isLoop newMap (guardRow, guardCol) Set.empty
        // if res then printfn "%A" position
        res
    positions |> List.filter isLoopHelper |> List.length

let map = parseInput "input.dat"

match findGuard map with
| Some guardPos -> 
    let newMap = moveGuard (deepCopy map) guardPos
    countXs newMap |> part1
    // printfn "%A" map
    // printfn "%A" newMap

    let positions = [for row in [0..map.Length - 1] do for col in [0..map[0].Length - 1] -> (row, col)] 
                    |> List.filter (fun (row, col) -> map[row][col] = '.')
    // let positions = [(6, 3)]
    // printfn "positions: %A" positions
    // printfn "%A" map
    countLoops map guardPos positions |> part2  // 14251 too high
| None -> printfn "No guard found"