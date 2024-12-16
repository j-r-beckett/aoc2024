open Common.Functions
open FSharpx.Collections
open System

let readMap filename =
    readlines filename |> List.map Seq.toList

let allPositions (map: list<list<char>>) =
    [ for row in [ 0 .. map.Length - 1 ] do
          for col in [ 0 .. map[0].Length - 1 ] -> row, col ]

let find (map: list<list<char>>) (mapValue: char) =
    allPositions map
    |> List.filter (fun (row, col) -> map[row][col] = mapValue)
    |> List.head

type GridMap = list<list<char>>

type Position = int * int

type Score = int

type Direction =
    | Up
    | Right
    | Down
    | Left

let vector direction =
    match direction with
    | Up -> (-1, 0)
    | Right -> (0, 1)
    | Down -> (1, 0)
    | Left -> (0, -1)

let (+!) (row, col) direction =
    let deltaRow, deltaCol = vector direction
    (row + deltaRow, col + deltaCol)

let allDirections = [ Up; Right; Down; Left ]

// https://stackoverflow.com/a/35848799
// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let rotate (rotateRight: bool) direction =
    let increment = if rotateRight then 1 else -1

    allDirections[((List.findIndex ((=) direction) allDirections) + increment)
                  %! allDirections.Length]

type State =
    { Direction: Direction
      Position: Position
      Score: Score }

let rotateRight state =
    { state with
        Direction = rotate true state.Direction
        Score = state.Score + 1000 }

let rotateLeft state =
    { state with
        Direction = rotate false state.Direction
        Score = state.Score + 1000 }

let forward (map: GridMap) (state: State) =
    let (nextRow, nextCol) = state.Position +! state.Direction

    if
        nextRow >= 0
        && nextRow < map.Length - 1
        && nextCol >= 0
        && nextCol < map[0].Length - 1
        && map[nextRow][nextCol] = '.'
    then
        Some
            { state with
                Position = (nextRow, nextCol)
                Score = state.Score + 1 }
    else
        None


let adjacentStates (map: GridMap) (state: State) =
    let rotationStates = [ rotateLeft state; rotateRight state ]

    match forward map state with
    | Some forwardState -> rotationStates @ [ forwardState ]
    | None -> rotationStates

let manhattanDistance (currentState: State) (destination: Position) =
    let currRow, currCol = currentState.Position
    let destRow, destCol = destination
    (abs (destRow - currRow)) + (abs (destCol - currCol))


let printMap (map: GridMap) (currState: State) (visited: Set<Position>)=
    for row in [ 0 .. map.Length - 1 ] do
        for col in [ 0 .. map[0].Length - 1 ] do
            if (row, col) = currState.Position then
                let posRep =
                    match currState.Direction with
                    | Up -> "^"
                    | Right -> ">"
                    | Down -> "v"
                    | Left -> "<"

                printf "%s" posRep
            else if Set.contains (row, col) visited
            then printf "O"
            else
                printf "%s" (map[row][col] |> string)

        printfn ""

    printfn ""


let aStar (map: GridMap) (startState: State) (destination: Position) =
    let rec findBestEndState frontier (seen: Set<Position * Direction>) =
        match PriorityQueue.tryPop frontier with
        | Some((_, frontierState), poppedFrontier) ->
            if frontierState.Position = destination then
                frontierState
            else

                let adjacentStates =
                    adjacentStates map frontierState
                    |> List.filter (fun adj -> Set.contains (adj.Position, adj.Direction) seen |> not)

                let newFrontier =
                    adjacentStates
                    |> List.map (fun adj -> (adj.Score + (manhattanDistance adj destination), adj))
                    |> List.fold (fun frontier element -> PriorityQueue.insert element frontier) poppedFrontier

                let newSeen =
                    Set.union seen (adjacentStates |> List.map (fun adj -> adj.Position, adj.Direction) |> Set.ofList)

                findBestEndState newFrontier newSeen 
        | None -> raise (InvalidOperationException ("Destination not found"))

    let frontier =
        PriorityQueue.empty false
        |> PriorityQueue.insert (manhattanDistance startState destination, startState)

    findBestEndState frontier (Set.ofList [ (startState.Position, startState.Direction) ])

let map = readMap "input.dat"

let startState =
    { Position = find map 'S'
      Direction = Right
      Score = 0 }

let endPosition = find map 'E'

let cleanMap =
    List.map
        (List.map (fun c ->
            if c = 'S' then '.'
            else if c = 'E' then '.'
            else c))
        map

let endState = aStar cleanMap startState endPosition 
endState.Score |> part1