open Common.Functions

let readMap filename =
    readlines filename |> List.map Seq.toList

let isInBounds map (row, col) =
    row >= 0 && row < List.length map && col >= 0 && col < List.length map[0]

let adjacent map (row, col) =
    [ (row + 1, col); (row - 1, col); (row, col - 1); (row, col + 1) ]
    |> List.filter (isInBounds map)

let rec exploreRegion map visited (row, col) =
    adjacent map (row, col)
    |> List.filter (fun (adjRow, adjCol) -> map[adjRow][adjCol] = map[row][col])
    |> List.filter (fun (adjRow, adjCol) -> not (Set.contains (adjRow, adjCol) visited))
    |> List.fold (fun soFar position -> exploreRegion map soFar position) (Set.add (row, col) visited)


let rec tryGetRegion (row, col) possibleRegions =
    match possibleRegions with
    | [] -> None
    | possibleRegion :: tailRegions ->
        if Set.contains (row, col) possibleRegion then
            Some possibleRegion
        else
            tryGetRegion (row, col) tailRegions

let findAllRegions map =
    let rec helper regions positions =
        match positions with
        | [] -> regions
        | position :: tailPositions ->
            match tryGetRegion position regions with
            | Some _ -> helper regions tailPositions
            | None -> helper ([ exploreRegion map Set.empty position ] @ regions) tailPositions

    helper
        []
        [ for row in [ 0 .. map.Length - 1 ] do
              for col in [ 0 .. map[0].Length - 1 ] -> row, col ]

let calculateArea = Set.count

let calculatePerimeter map region =
    let countOuterBorders (row, col) =
        let innerBorders =
            adjacent map (row, col)
            |> List.filter (fun (adjRow, adjCol) -> map[adjRow][adjCol] = map[row][col])
            |> List.length

        4 - innerBorders

    region |> Set.toList |> List.map countOuterBorders |> List.sum

let countSides region =
    let isCorner (p1, p2, p3) =
        // p1 and p3 in region and p2 not in region, or p1 and p3 not in region
        (Set.contains p1 region
         && Set.contains p2 region |> not
         && Set.contains p3 region)
        || (Set.contains p1 region |> not && Set.contains p3 region |> not)

    let countCorners (row, col) =
        [ ((row, col - 1), (row - 1, col - 1), (row - 1, col))
          ((row - 1, col), (row - 1, col + 1), (row, col + 1))
          ((row, col + 1), (row + 1, col + 1), (row + 1, col))
          ((row + 1, col), (row + 1, col - 1), (row, col - 1)) ]
        |> List.map isCorner
        |> List.filter id
        |> List.length

    region |> Set.toList |> List.map countCorners |> List.sum

let score f g regions =
    regions |> List.map (fun region -> (f region) * (g region)) |> List.sum

let map = readMap "input.dat"
let regions = map |> findAllRegions

score calculateArea (calculatePerimeter map) regions |> part1

score calculateArea countSides regions |> part2
