open Common.Functions
open System.Collections.Generic

let readBytePositions filename maxBytes =
    let parseBytePosition (byteStr: string) =
        let delimiterIndex = byteStr.IndexOf(',')
        int byteStr[delimiterIndex + 1 ..], int byteStr[.. delimiterIndex - 1]

    (readlines filename)[.. maxBytes - 1] |> List.map parseBytePosition

let prettyPrint (memory: list<list<char>>) =
    for row in memory do
        for char in row do
            printf "%s" (char |> string)

        printfn ""

    printfn ""


let buildMemory (numRows, numCols) (bytePositions: list<int * int>) =
    [ for row in [ 0 .. numRows - 1 ] ->
          [ for col in [ 0 .. numCols - 1 ] -> if List.contains (row, col) bytePositions then '#' else '.' ] ]
    |> List.map List.toArray
    |> List.toArray


let isInBounds (numRows, numCols) (row, col) =
    row >= 0 && row < numRows && col >= 0 && col < numCols

let findShortestPath (startRow, startCol) (goalRow, goalCol) (memory: array<array<char>>) =
    let queue = new Queue<(int * int) * int>()
    queue.Enqueue((startRow, startCol), 0)

    let visited = new HashSet<int * int>()
    visited.Add((startRow, startCol)) |> ignore

    let bfsStep () =
        let (currRow, currCol), distanceToStart = queue.Dequeue()

        if (currRow, currCol) = (goalRow, goalCol) then
            Some distanceToStart
        else
            let adjacent =
                [ (currRow - 1, currCol)
                  (currRow + 1, currCol)
                  (currRow, currCol - 1)
                  (currRow, currCol + 1) ]
                |> List.filter (isInBounds (memory.Length, memory[0].Length))
                |> List.filter (fun neighbor -> visited.Contains(neighbor) |> not)
                |> List.filter (fun (neighborRow, neighborCol) -> memory[neighborRow][neighborCol] = '.')

            for adj in adjacent do
                visited.Add(adj) |> ignore
                queue.Enqueue(adj, distanceToStart + 1)

            None

    let rec bfs () =
        if queue.Count = 0 then
            None
        else
            match bfsStep () with
            | Some n -> Some n
            | None -> bfs ()

    bfs ()


let rec findBlockingByte (memory: array<array<char>>) (bytePositions: list<int * int>) =
    match bytePositions with
    | [] -> raise (System.ArgumentException("ran out of bytes"))
    | (byteRow, byteCol) :: remaining ->
        Array.set memory[byteRow] byteCol '#'

        match findShortestPath (0, 0) (memory.Length - 1, memory[0].Length - 1) memory with
        | Some _ -> findBlockingByte memory remaining
        | None -> (byteRow, byteCol)


let numRows, numCols = 71, 71

readBytePositions "input.dat" 1024
|> buildMemory (numRows, numCols)
|> findShortestPath (0, 0) (numRows - 1, numCols - 1)
|> _.Value
|> part1

let blockingByteRow, blockingByteCol =
    readBytePositions "input.dat" 3450
    |> findBlockingByte (buildMemory (numRows, numCols) [])

sprintf "%i,%i" blockingByteCol blockingByteRow |> part2
