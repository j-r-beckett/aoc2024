open Common.Functions

let parseInput filename =
    readlines filename |> List.map Seq.toArray |> List.toArray

type Map = array<array<char>>

type Delta = int * int

type Direction =
    | Up
    | Right
    | Down
    | Left

    member direction.Delta =
        match direction with
        | Up -> -1, 0
        | Right -> 0, 1
        | Down -> 1, 0
        | Left -> 0, -1

type Guard =
    { row: int
      col: int
      direction: Direction }

let parseDirection directionChar =
    match directionChar with
    | '^' -> Some Up
    | '>' -> Some Right
    | 'v' -> Some Down
    | '<' -> Some Left
    | _ -> None

let rotate (guard: Guard) =
    let newDirection =
        match guard.direction with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    { guard with direction = newDirection }

let stepForward (guard: Guard) =
    let newRow, newcol =
        guard.row + (fst guard.direction.Delta), guard.col + (snd guard.direction.Delta)

    { guard with
        row = newRow
        col = newcol }

let isBlocked (guard: Guard) (map: Map) =
    let movedGuard = stepForward guard
    map[movedGuard.row][movedGuard.col] = '#'

let willStepOffMap (guard: Guard) (map: Map) =
    let movedGuard = stepForward guard

    movedGuard.row <= -1
    || movedGuard.row >= map.Length
    || movedGuard.col <= -1
    || movedGuard.col >= map[0].Length

let rec escapeRoute (map: Map) (guard: Guard) visited =
    let visitedKey = guard.row, guard.col, guard.direction

    if willStepOffMap guard map then
        Set.add visitedKey visited |> Some
    else if Set.contains visitedKey visited then
        None
    else if isBlocked guard map then
        escapeRoute map (rotate guard) visited
    else
        escapeRoute map (stepForward guard) (Set.add visitedKey visited)

let findGuard (map: Map) =
    let rec findGuardHelper positions =
        match positions with
        | [] -> raise (System.ArgumentException("no guard found"))
        | (row, col) :: tail ->
            match parseDirection (map[row][col]) with
            | Some direction ->
                { row = row
                  col = col
                  direction = direction }
            | None -> findGuardHelper tail

    findGuardHelper
        [ for row in [ 0 .. map.Length - 1 ] do
              for col in [ 0 .. map[0].Length - 1 ] -> row, col ]


let countLoops (map: Map) (guard: Guard) (positions: list<int * int>) =
    let isLoop (obsRow, obsCol) =
        Array.set map[obsRow] obsCol '#'

        let ret =
            match escapeRoute map guard Set.empty with
            | Some _ -> false
            | None -> true

        Array.set map[obsRow] obsCol '.'
        ret

    positions |> List.filter isLoop |> List.length

let map = parseInput "input.dat"
let guard = findGuard map

let visited =
    match escapeRoute map guard Set.empty with
    | Some route -> route
    | None -> raise (System.InvalidOperationException("no route found for part 1"))

let route = visited |> Set.map (fun (row, col, _) -> row, col) |> Set.toList
route |> List.length |> part1

let positions =
    route |> List.filter (fun (row, col) -> (row, col) <> (guard.row, guard.col))

countLoops map guard route |> part2
