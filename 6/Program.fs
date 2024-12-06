open Common.Functions

let parseInput filename =
    readlines filename |> List.map Seq.toArray |> List.toArray

type Delta = int * int

type Direction =
| Up of Delta
| Right of Delta
| Down of Delta
| Left of Delta

type Guard = {row: int; col: int; direction: Direction}

let rec escapeRoute (map: array<array<char>>) (guardRow: int, guardCol: int) visited =
    let direction row col =
        match map[row][col] with
        | '^' -> -1, 0
        | '>' -> 0, 1
        | 'v' -> 1, 0
        | '<' -> 0, -1
        | _ -> raise (System.InvalidOperationException "unkown guard direction")

    let isBlocked (row: int) (col: int) (direction: int * int) =
        map[row + (fst direction)][col + (snd direction)] = '#'

    let willStepOffMap row col direction =
        let nextRow, nextCol = row + (fst direction), col + (snd direction)

        nextRow <= -1
        || nextRow >= map.Length
        || nextCol <= -1
        || nextCol >= map[0].Length

    let rotateGuard () =
        let newGuard =
            match map[guardRow][guardCol] with
            | '^' -> '>'
            | '>' -> 'v'
            | 'v' -> '<'
            | '<' -> '^'
            | _ -> raise (System.InvalidOperationException "unkown guard direction")

        Array.set map[guardRow] guardCol newGuard

    let stepGuardForward () =
        let guardDirection = direction guardRow guardCol

        let newGuardRow, newGuardCol =
            guardRow + (fst guardDirection), guardCol + (snd guardDirection)

        Array.set map[newGuardRow] newGuardCol (map[guardRow][guardCol])
        Array.set map[guardRow] guardCol '.'
        newGuardRow, newGuardCol

    let guardDirection = direction guardRow guardCol
    let key = guardRow, guardCol, (map[guardRow][guardCol])

    if willStepOffMap guardRow guardCol guardDirection then
        Set.add key visited |> Set.map (fun (row, col, _) -> row, col) |> Some
    else
        if Set.contains key visited then
            None
        else if isBlocked guardRow guardCol guardDirection then
            rotateGuard () |> ignore
            escapeRoute map (guardRow, guardCol) visited
        else
            escapeRoute map (stepGuardForward ()) (Set.add key visited)

let findGuard (map: array<array<char>>) =
    let rec findGuardHelper row =
        let rec guardCol col =
            if col >= map[0].Length then
                None
            else
                match map[row][col] with
                | '^' -> Some col
                | '>' -> Some col
                | 'v' -> Some col
                | '<' -> Some col
                | _ -> guardCol (col + 1)

        if row >= map.Length then
            None
        else
            match guardCol 0 with
            | Some col -> Some(row, col)
            | None -> findGuardHelper (row + 1)

    findGuardHelper 0

let deepCopy (map: array<array<char>>) = map |> Array.map Array.copy

let countLoops (map: array<array<char>>) (guardRow: int, guardCol: int) (positions: list<int * int>) =
    let isLoopHelper position =
        let newMap = deepCopy map
        Array.set newMap[fst position] (snd position) '#'
        match escapeRoute newMap (guardRow, guardCol) Set.empty with
        | Some _ -> false
        | None -> true

    positions |> List.filter isLoopHelper |> List.length

let map = parseInput "test.dat"

match findGuard map with
| Some guardPos ->
    let route = match escapeRoute (deepCopy map) guardPos Set.empty with
                | Some route -> route
                | None -> raise (System.InvalidOperationException("no route found"))
    route |> Set.count |> part1

    let positions = route |> Set.toList |> List.filter (fun (row, col) -> (row, col) <> guardPos)
    countLoops map guardPos positions |> part2
| None -> printfn "No guard found"
