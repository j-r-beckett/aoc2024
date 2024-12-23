open Common.Functions
open System.Collections.Generic

let readComputerGraph filename =
    let connections = readlines filename |> List.map (fun line -> line[..1], line[3..])
    let map = new Dictionary<string, List<string>>()

    for (c1, c2) in connections do
        map.TryAdd(c1, new List<string>()) |> ignore
        map[c1].Add(c2)
        map.TryAdd(c2, new List<string>()) |> ignore
        map[c2].Add(c1)

    map
    |> Seq.map (|KeyValue|)
    |> Seq.map (fun (key, lst) -> key, Set.ofSeq lst)
    |> Map.ofSeq

let cliquesOfThree (graph: Map<string, Set<string>>) =
    [ for c1 in graph.Keys do
          for c2 in graph.Keys -> c1, c2 ]
    |> List.filter (fun (c1, c2) -> Set.contains c1 graph[c2] && Set.contains c2 graph[c1])
    |> List.map (fun (c1, c2) -> c1, c2, Set.intersect graph[c1] graph[c2])
    |> List.map (fun (c1, c2, intersection) -> intersection |> Set.map (fun c3 -> Set.ofList [ c1; c2; c3 ]))
    |> Set.unionMany

let maxClique (neighbors: Map<string, Set<string>>) =
    let rec bronKerbosh (R: Set<string>) (P: Set<string>) (X: Set<string>) =
        let mutable P = P
        let mutable X = X

        seq {
            if Set.isEmpty P && Set.isEmpty X then
                yield R

            for v in P do
                yield! bronKerbosh (Set.add v R) (Set.intersect P neighbors[v]) (Set.intersect X neighbors[v])
                P <- Set.remove v P
                X <- Set.add v X
        }

    bronKerbosh Set.empty (neighbors.Keys |> Set.ofSeq) Set.empty
    |> Seq.maxBy Set.count

let graph = readComputerGraph "input.dat"

cliquesOfThree graph
|> Set.filter (fun set -> Set.exists (fun (c: string) -> c[0] = 't') set)
|> Set.count
|> part1

graph |> maxClique |> Set.toList |> List.sort |> String.concat "," |> part2
