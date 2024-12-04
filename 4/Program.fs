open Common.Functions

let part1Comparator (target: string) (lines: string list) (row, col) =
    let rec compareToTarget (row, col) distance (direction: int * int) =
        if distance = 0 then
            true
        else if row < 0 || row >= lines.Length || col < 0 || col >= lines[0].Length then
            false
        else
            lines[row][col] = target[target.Length - distance]
            && (compareToTarget (row + (fst direction), col + (snd direction)) (distance - 1) direction)

    let directions =
        [ (0, -1); (0, 1); (-1, 0); (1, 0); (-1, -1); (-1, 1); (1, -1); (1, 1) ]

    directions
    |> List.map (fun direction -> compareToTarget (row, col) target.Length direction)
    |> List.filter id
    |> _.Length

let part2Comparator (target: string) (lines: string list) (row, col) =
    if
        row <= 0
        || row >= lines.Length - 1
        || col <= 0
        || col >= lines[0].Length - 1
        || lines[row][col] <> 'A'
    then
        0
    else
        let p1 = lines[row - 1][col - 1], lines[row + 1][col + 1]
        let p2 = lines[row + 1][col - 1], lines[row - 1][col + 1]

        if ((p1 = ('M', 'S') || p1 = ('S', 'M')) && (p2 = ('M', 'S') || p2 = ('S', 'M'))) then
            1
        else
            0

let incidences (target: string) (lines: string list) comparator =
    [ for row in [ 0 .. lines.Length - 1 ] do
          for col in [ 0 .. lines[0].Length - 1 ] -> row, col ]
    |> List.map (comparator target lines)
    |> List.sum

let lines = readlines "input.dat"
incidences "XMAS" lines part1Comparator |> part1
incidences "XMAS" lines part2Comparator |> part2
