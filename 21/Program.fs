open Common.Functions
open System.Collections.Generic

type KeypadMap = Map<char, Map<char, char>>

let mapKeypad (baseKeypad: list<list<char>>) =
    let isInBounds row col =
        row >= 0 && row < baseKeypad.Length && col >= 0 && col < baseKeypad[0].Length

    let findEdges (keypad: list<list<char>>) (edgeFn: (int * int) -> Map<char, char>) =
        let edges (startRow, startCol) =
            (keypad[startRow][startCol], edgeFn (startRow, startCol))

        [ for row in [ 0 .. keypad.Length - 1 ] do
              for col in [ 0 .. keypad[0].Length - 1 ] -> row, col ]
        |> List.filter (fun (row, col) -> keypad[row][col] <> '.')
        |> List.map edges
        |> Map.ofList

    let edgeFn (row, col) =
        if baseKeypad[row][col] = '.' then
            Map.empty
        else
            [ ('^', (row - 1, col))
              ('v', (row + 1, col))
              ('<', (row, col - 1))
              ('>', (row, col + 1)) ]
            |> List.filter (fun (_, (adjRow, adjCol)) -> isInBounds adjRow adjCol && baseKeypad[adjRow][adjCol] <> '.')
            |> List.map (fun (direction, (adjRow, adjCol)) -> direction, baseKeypad[adjRow][adjCol])
            |> Map.ofList

    findEdges baseKeypad edgeFn

let findPaths (keypadMap: KeypadMap) (start: char) (goal: char) =
    let queue = new Queue<char * list<char>>()
    queue.Enqueue((start, []))

    let visited = new HashSet<char>()
    visited.Add(start) |> ignore

    let bfsStep () =
        let curr, pathSoFar = queue.Dequeue()

        if curr = goal then
            raise (System.InvalidOperationException("Stepping past goal"))
        else
            for (KeyValue(direction, next)) in keypadMap[curr] do
                visited.Add(next) |> ignore
                queue.Enqueue((keypadMap[curr][direction], pathSoFar @ [ direction ])) |> ignore

    let rec bfs () =
        if queue.Count = 0 then
            raise (System.InvalidOperationException("No path found"))
        else
            let pathsToGoal =
                queue |> Seq.toList |> List.filter (fun (pos, _) -> pos = goal) |> List.map snd

            if pathsToGoal.IsEmpty then
                for _ in [ 0 .. queue.Count - 1 ] do
                    bfsStep ()

                bfs ()
            else
                pathsToGoal

    bfs ()





let part1ButtonPresses (code: list<char>) =


    let findAllNeededButtonPresses (keyPad: KeypadMap) (desiredResults: list<list<char>>) =
        // let findAllShortestPaths (desiredResult: list<char>) =
        //     List.zip (['A'] @ desiredResult[..desiredResult.Length - 2]) desiredResult
        //     |> List.map (fun (prev, next) -> findPaths keyPad prev next |> List.map (fun path -> path @ ['A']))

        // List.map findAllShortestPaths desiredResults

        let combineShortestPaths (shortestPaths: list<list<char>>) (prevEnd: char, nextStart: char) =
            let nextShortestPaths = findPaths keyPad prevEnd nextStart 
            [for prevPath in shortestPaths do for nextPath in nextShortestPaths -> prevPath @ nextPath @ ['A']]
            // |> List.map (fun nextPath-> shortestPaths |> List.map (fun prevPath -> prevPath @ nextPath))



        let resultPaths = desiredResults 
                        |> List.map (fun desiredResult -> List.fold combineShortestPaths [[]] (List.zip (['A'] @ desiredResult[..desiredResult.Length - 2]) desiredResult))
                        |> List.concat

        let shortestPathLen = List.min (List.map List.length resultPaths)
        resultPaths |> List.filter (fun path -> path.Length = shortestPathLen)

    let numericKp =
        [ [ '7'; '8'; '9' ]; [ '4'; '5'; '6' ]; [ '1'; '2'; '3' ]; [ '.'; '0'; 'A' ] ]
        |> mapKeypad

    let directionalKp = [ [ '.'; '^'; 'A' ]; [ '<'; 'v'; '>' ] ] |> mapKeypad

    [ code ] 
    |> findAllNeededButtonPresses numericKp
    |> findAllNeededButtonPresses directionalKp
    |> findAllNeededButtonPresses directionalKp
    // |> List.map (List.map string)
    // |> List.map (String.concat "")


let pathComplexity (code: list<char>, path: list<char>) =
    let numeric = code[..code.Length - 2] |> List.map string |> String.concat "" |> int
    numeric * path.Length


let codes = readlines "input.dat" |> List.map Seq.toList
// for path in part1ButtonPresses codes[0] do
//     printfn "%A" path
// part1ButtonPresses codes[0] |> List.map Seq.length |> List.distinct |> printfn "%A"
codes
|> List.map (fun code -> code, part1ButtonPresses code |> List.head)
|> List.map pathComplexity
|> List.sum
|> part1
// pathComplexity codes[0] (part1ButtonPresses codes[0] |> List.head) |> printfn "%A"

// findPaths numericKeypadMap '0' '6' |> printfn "%A"
