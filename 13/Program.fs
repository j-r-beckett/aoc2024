open Common.Functions
open System.Text.RegularExpressions

type Point = { X: int64; Y: int64 }

let point (x, y) = { X = x; Y = y }

type Machine = { A: Point; B: Point; Prize: Point }

let parseMachine (lines: list<string>) =
    let buttonPattern = @"Button .: X\+(\d+), Y\+(\d+)"
    let prizePattern = @"Prize: X=(\d+), Y=(\d+)"

    let extractPoint line pattern =
        let matches = Regex.Match(line, pattern)

        (matches.Groups[1] |> string |> int64, matches.Groups[2] |> string |> int64)
        |> point

    { A = extractPoint lines[0] buttonPattern
      B = extractPoint lines[1] buttonPattern
      Prize = extractPoint lines[2] prizePattern }

let readMachines filename =
    let lines = readlines filename

    [ 0 .. lines.Length / 4 ]
    |> List.map (fun i -> parseMachine lines[i * 4 .. (i + 1) * 4 - 1])

let minimumTokens (machine: Machine) =
    let bPressesNumerator =
        machine.Prize.Y * machine.A.X - machine.Prize.X * machine.A.Y

    let bPressesDenominator = machine.B.Y * machine.A.X - machine.B.X * machine.A.Y

    if bPressesNumerator % bPressesDenominator <> 0 then
        None
    else
        let bPresses = bPressesNumerator / bPressesDenominator

        let aPressesNumerator = machine.Prize.X - bPresses * machine.B.X
        let aPressesDenominator = machine.A.X

        if aPressesNumerator % aPressesDenominator <> 0 then
            None
        else
            let aPresses = aPressesNumerator / aPressesDenominator
            Some(aPresses * 3L + bPresses)


let totalMinimumTokens prizeShift machines =
    machines
    |> List.map (fun machine ->
        { machine with
            Prize = (machine.Prize.X + prizeShift, machine.Prize.Y + prizeShift) |> point })
    |> List.map (fun machine ->
        match minimumTokens machine with
        | Some n -> n
        | None -> 0)
    |> List.sum


let machines = readMachines "input.dat"
machines |> totalMinimumTokens 0 |> part1
machines |> totalMinimumTokens 10000000000000L |> part2
