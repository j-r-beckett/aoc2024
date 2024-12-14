open Common.Functions
open System.Text.RegularExpressions
open FSharpx.Collections

type Robot =
    { Position: int64 * int64
      Velocity: int64 * int64 }

let readRobots filename =
    let parsePosition line =
        let pattern = @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)"
        let matches = Regex.Match(line, pattern)
        let nums = (Seq.toList matches.Groups)[1..4] |> List.map (string >> int64)
        (nums[1], nums[0]), (nums[3], nums[2])

    let robot (position, velocity) =
        { Position = position
          Velocity = velocity }

    readlines filename |> List.map parsePosition |> List.map robot

// https://stackoverflow.com/a/35848799
// Euclidean remainder, the proper modulo operation
let inline (%!) a b = (a % b + b) % b

let simulate (numRows, numCols) seconds robots =
    let simulateSingleRobot robot =
        let newRow = ((fst robot.Position) + seconds * (fst robot.Velocity)) %! numRows
        let newCol = ((snd robot.Position) + seconds * (snd robot.Velocity)) %! numCols

        { robot with
            Position = (newRow, newCol) }

    robots |> List.map simulateSingleRobot

let rec countRobots robots pos =
    match robots with
    | [] -> 0L
    | headRobot :: tailRobots -> (if headRobot.Position = pos then 1L else 0L) + (countRobots tailRobots pos)

let safetyFactor (numRows: int64, numCols: int64) robots =
    let countRobotsInQuadrant (topLeft, bottomRight) =
        [ for row in [ fst topLeft .. fst bottomRight ] do
              for col in [ snd topLeft .. snd bottomRight ] -> row, col ]
        |> List.map (countRobots robots)
        |> List.sum

    let simpleCeil n = n |> float |> ceil |> int64
    let simpleFloor n = n |> float |> floor |> int64

    // top left, bottom right, top right, bottom left
    let quadrants =
        [ ((0L, 0L), (numRows / 2L - 1L, numCols / 2L - 1L))
          ((simpleCeil ((float numRows) / 2.0), simpleCeil ((float numCols) / 2.0)), (numRows - 1L, numCols - 1L))
          ((0, simpleCeil ((float numCols) / 2.0)), (numRows / 2L - 1L, numCols - 1L))
          ((simpleCeil ((float numRows) / 2.0), 0L), (numRows - 1L, numCols / 2L - 1L)) ]

    quadrants |> List.map countRobotsInQuadrant |> List.fold (*) 1L



let printRobots (numRows, numCols) robots =
    for row in [ 0L .. numRows - 1L ] do
        for col in [ 0L .. numCols - 1L ] do
            let numRobots = countRobots robots (row, col)
            if numRobots = 0 then printf "." else printf "%i" numRobots

        printfn ""

let rec any lst =
    match lst with
    | [] -> false
    | head :: tail -> head || any tail

let largestBlob robots =
    let isRobotInBlob (robotRow: int64, robotCol: int64) (blob: Set<int64 * int64>) =
        [ (robotRow + 1L, robotCol)
          (robotRow - 1L, robotCol)
          (robotRow, robotCol - 1L)
          (robotRow, robotCol + 1L);
          (robotRow + 1L, robotCol - 1L)
          (robotRow + 1L, robotCol + 1L)
          (robotRow - 1L, robotCol - 1L)
          (robotRow - 1L, robotCol + 1L) ]
        |> List.map (fun pos -> Set.contains pos blob)
        |> any

    let findBlobs robot possibleBlobs =
        possibleBlobs
        |> List.indexed
        |> List.map (fun (index, blob) -> int64 index, isRobotInBlob robot blob)
        |> List.filter snd
        |> List.map fst

    let rec accumulate robots blobsSoFar =
        match robots with
        | [] -> blobsSoFar
        | robot :: remainingRobots ->
            let blobIndicesToBeMerged = findBlobs robot blobsSoFar

            let unchangedBlobs =
                blobsSoFar
                |> List.indexed
                |> List.filter (fun (index, _) -> List.contains (int64 index) blobIndicesToBeMerged |> not)
                |> List.map snd

            let mergedBlobs =
                blobsSoFar
                |> List.indexed
                |> List.filter (fun (index, _) -> List.contains (int64 index) blobIndicesToBeMerged)
                |> List.map snd
                |> Set.unionMany
                |> Set.add robot

            accumulate remainingRobots (unchangedBlobs @ [ mergedBlobs ])

    accumulate robots [] |> List.map Set.count |> List.max


let robots = readRobots "input.dat"
let numRows, numCols = 103L, 101L

robots
|> simulate (numRows, numCols) 100
|> safetyFactor (numRows, numCols)
|> part1

robots |> printRobots (numRows, numCols)
// for seconds in [ 0..10000 ] do
//     if seconds % 50 = 0
//     then printfn "seconds progress: %A" seconds
//     let newRobots = robots |> simulate (numRows, numCols) seconds
//     let maxBlobSize = newRobots |> List.map (fun robot -> robot.Position) |> largestBlob
//     if maxBlobSize > 10
//     then
//         printfn "seconds: %A" seconds
//         printfn "max blob size: %A" maxBlobSize
//         printfn ""
//         printRobots (numRows, numCols) newRobots
//         printfn "\n\n-------\n\n"
