open Common.Functions

let rec generateSecretNumbers n initial =

    let nextNumber num =
        let one = (num * 64L ^^^ num) % 16777216L
        let two = ((one / 32L) ^^^ one) % 16777216L
        let three = ((two * 2048L) ^^^ two) % 16777216L
        three

    // let rec helper n curr =
    //     if n = 0 then curr else helper (n - 1) (nextNumber curr)

    // helper n initial
    if n = 0 then
        [ initial ]
    else
        [ initial ] @ (generateSecretNumbers (n - 1) (nextNumber initial))


let makeBananaMap (secretNumbers: list<int64>) =
    let prices = secretNumbers |> List.map (fun n -> n % 10L)

    let diffs =
        List.zip prices[1..] prices[.. prices.Length - 2]
        |> List.map (fun (next, prev) -> next - prev)

    [ 4 .. diffs.Length - 1 ]
    |> List.rev
    |> List.map (fun i -> diffs[i - 3 .. i], prices[i + 1])
    |> Map.ofList

let findBestChange (bananaMaps: list<Map<list<int64>, int64>>) =
    let findBananas (change: list<int64>) =
        bananaMaps |> List.map (Map.tryFind change) |> List.choose id |> List.sum

    let allChanges =
        bananaMaps
        |> List.map (fun map -> Map.keys map |> Seq.cast |> List.ofSeq)
        |> List.concat
        |> List.distinct

    allChanges |> List.map findBananas |> List.max


let seeds = readlines "input.dat" |> List.map int64

seeds
|> List.map (generateSecretNumbers 2000)
|> List.map (fun lst -> lst[lst.Length - 1])
|> List.sum
|> part1

seeds
|> List.map (generateSecretNumbers 2000)
|> List.map makeBananaMap
|> findBestChange
|> printfn "%A"
