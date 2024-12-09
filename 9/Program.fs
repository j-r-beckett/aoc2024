open Common.Functions

let getBlocks (diskMap: list<int>) : list<Option<int>> =
    let helper (index, value) =
        if index % 2 <> 0 then
            [ for _ in [ 0 .. value - 1 ] -> None ]
        else
            [ for _ in [ 0 .. value - 1 ] -> Some(int ((double index) / 2.0)) ]

    diskMap |> List.indexed |> List.map helper |> List.concat

let rec prettyPrint blocks =
    match blocks with
    | [] -> ""
    | head :: tail ->
        (match head with
         | None -> "."
         | Some n -> string n)
        + (prettyPrint tail)

let readDiskmap filename =
    readlines filename |> List.head |> Seq.toList |> List.map (string >> int)

let compressBlocks (blocks: list<option<int>>) =
    let isSome option =
        match option with
        | None -> false
        | Some _ -> true

    let isNone = not << isSome
    let numEmpty = blocks |> List.filter isNone |> List.length
    let fillers = blocks[blocks.Length - numEmpty ..] |> List.filter isSome |> List.rev

    let helper (numEmptiesSoFar: int, resultSoFar: list<option<int>>) (value: option<int>) =
        match value with
        | Some n -> numEmptiesSoFar, resultSoFar @ [ Some n ]
        | None -> numEmptiesSoFar + 1, resultSoFar @ [ fillers[numEmptiesSoFar] ]

    (blocks[.. blocks.Length - numEmpty - 1] |> List.fold helper (0, []) |> snd)
    @ [ for _ in [ 0 .. numEmpty - 1 ] -> None ]

type File = { NumBlocks: int; Id: int }

type FreeSpace =
    { NumFreeBlocks: int
      Files: list<File> }

type DiskLocation =
    | File of file: File
    | FreeSpace of freeSpace: FreeSpace

let getDiskLocations (diskMap: list<int>) =
    let helper (index, value) =
        if index % 2 <> 0 then
            FreeSpace { NumFreeBlocks = value; Files = [] }
        else
            File
                { NumBlocks = value
                  Id = (int ((double index) / 2.0)) }

    diskMap |> List.indexed |> List.map helper

let compressFiles (diskLocations: list<DiskLocation>) =
    let diskLocations = diskLocations |> List.toArray

    for i in [ 0 .. diskLocations.Length - 1 ] |> List.rev do
        match diskLocations[i] with
        | FreeSpace freeSpace -> ()
        | File file ->
            let jOption: option<int> =
                Array.tryFindIndex
                    (fun diskLoc ->
                        match diskLoc with
                        | FreeSpace freeSpace -> freeSpace.NumFreeBlocks >= file.NumBlocks
                        | _ -> false)
                    diskLocations[0 .. i - 1]

            match jOption with
            | Some j ->
                match diskLocations[j] with
                | File _ -> raise (System.InvalidOperationException "expected free space")
                | FreeSpace freeSpace ->
                    Array.set
                        diskLocations
                        j
                        (FreeSpace
                            { NumFreeBlocks = freeSpace.NumFreeBlocks - file.NumBlocks
                              Files = freeSpace.Files @ [ file ] })

                    Array.set
                        diskLocations
                        i
                        (FreeSpace
                            { NumFreeBlocks = file.NumBlocks
                              Files = [] })
            | None -> ()

    let helper diskLocation =
        let processFile file =
            [ for _ in [ 0 .. file.NumBlocks - 1 ] -> Some file.Id ]

        match diskLocation with
        | File file -> processFile file
        | FreeSpace freeSpace ->
            (freeSpace.Files |> List.map processFile |> List.concat)
            @ [ for _ in [ 0 .. freeSpace.NumFreeBlocks - 1 ] -> None ]

    diskLocations |> Array.toList |> List.map helper |> List.concat


let computeChecksum (blocks: list<option<int>>) : int64 =
    blocks
    |> List.indexed
    |> List.map (fun (index: int, value: option<int>) ->
        match value with
        | None -> int64 0
        | Some n -> (int64 n) * (int64 index))
    |> List.sum


let diskMap = readDiskmap "input.dat"
diskMap |> getBlocks |> compressBlocks |> computeChecksum |> part1
diskMap |> getDiskLocations |> compressFiles |> computeChecksum |> part2
