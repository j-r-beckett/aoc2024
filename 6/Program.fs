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

let map = parseInput "input.dat"
map |> findGuard |> printfn "%A"
let newMap = match findGuard map with
                | Some guardPos -> Some (moveGuard map guardPos)
                | None -> None
match newMap with
| Some m -> printfn "%A" m
| None -> printfn "whoops"
match newMap with
| Some m -> printfn "%A" (countXs m)
| None -> printfn "whoops"