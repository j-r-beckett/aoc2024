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

let rec gcd x y = if y = 0L then x else gcd y (x % y)

type Fraction = { Num: int64; Den: int64 }

let reduce (a: Fraction) =
    let divisor = gcd a.Num a.Den

    { Num = a.Num / divisor
      Den = a.Den / divisor }

let (^*) (a: Fraction) (b: Fraction) =
    { Num = a.Num * b.Num
      Den = a.Den * b.Den }
    |> reduce

let (^/) (a: Fraction) (b: Fraction) =
    { Num = a.Num * b.Den
      Den = a.Den * b.Num }
    |> reduce

let (^+) (a: Fraction) (b: Fraction) =
    { Num = a.Num * b.Den + b.Num * a.Den
      Den = a.Den * b.Den }
    |> reduce

let (^-) (a: Fraction) (b: Fraction) =
    a ^+ ({ Num = -1; Den = 1 } ^* b) |> reduce

let (&*) (row: list<Fraction>) (a: Fraction) = List.map ((^*) a) row
let (&/) (row: list<Fraction>) (a: Fraction) = List.map (fun r -> r ^/ a) row

let (&+) (row1: list<Fraction>) (row2: list<Fraction>) =
    List.zip row1 row2 |> List.map (fun (a, b) -> a ^+ b)

let (&-) (row1: list<Fraction>) (row2: list<Fraction>) =
    List.zip row1 row2 |> List.map (fun (a, b) -> a ^- b)

let minimumTokens (machine: Machine) =
    let row1 =
        [ machine.A.X; machine.B.X; machine.Prize.X ]
        |> List.map (fun n -> { Num = n; Den = 1 })

    let row2 =
        [ machine.A.Y; machine.B.Y; machine.Prize.Y ]
        |> List.map (fun n -> { Num = n; Den = 1 })

    let row1' = row1 &/ row1[0]
    let row2' = row2 &- (row1' &* row2[0])
    let row2'' = row2' &/ row2'[1]
    let row1'' = row1' &- (row2'' &* row1'[1])

    let aPresses, bPresses = row1''[2], row2''[2]

    match (aPresses.Den, bPresses.Den) with
    | (1L, 1L) -> Some(aPresses.Num * 3L + bPresses.Num)
    | _ -> None


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
