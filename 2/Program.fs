open Common.Functions

let parseInput filename =
    let parseReport (report : string) = report |> _.Split(' ') |> List.ofArray |> List.map int
    readlines filename |> List.ofSeq |> List.map parseReport

type Mode =
    | Increasing
    | Decreasing

let isSafe report =
    let rec helper (report: int list) mode =
        let isCompliant () =
            match mode with
            | Increasing -> report[1] > report[0] && report[1] - report[0] <= 3
            | Decreasing -> report[0] > report[1] && report[0] - report[1] <= 3 
        if report.Length <= 1 then true else isCompliant () && helper report[1..] mode
    helper report Increasing || helper report Decreasing

let isAnyPossibleReportSafe report =
    let rec any (lst : bool list) = if lst.IsEmpty then false else lst[0] || any lst[1..]
    
    let allPossibleReports (baseReport : int list) = 
        let remove i = baseReport[..i - 1] @ baseReport[i + 1..]
        [for i in 0..baseReport.Length do yield (remove i)]

    any (List.map isSafe (allPossibleReports report))

let countTrue f lst = List.fold (fun count e -> count + (if f e then 1 else 0)) 0 lst

let reports = parseInput "input.dat"
reports |> countTrue isSafe |> part1 
reports |> countTrue isAnyPossibleReportSafe |> part2