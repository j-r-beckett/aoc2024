open Common.Functions

type Rule = {Before: int; After: int}
type Update = int list

let readInput filename =
    let rec split (lines: string list) =
        if lines[0] = ""
        then [], lines[1..]
        else 
            let pendingRules, allUpdates = (split lines[1..])
            ([lines[0]] @ pendingRules, allUpdates)
    let parseRule (ruleStr: string) = 
        let barIndex = ruleStr.IndexOf("|")
        {Before=int ruleStr[..barIndex - 1]; After=int ruleStr[barIndex + 1..]}
    let parseUpdate (updateStr: string) = 
        updateStr.Split(",") 
        |> Array.toList 
        |> List.map int
    let ruleStrs, updateStrs = readlines filename |> split
    List.map parseRule ruleStrs, List.map parseUpdate updateStrs 

let rec brokenRules rules update =
    let isRuleSatisfied rule update = 
        let indexOf v lst = lst |> List.findIndex ((=) v)
        if (not (List.contains rule.Before update)) || (not (List.contains rule.After update))
        then true
        else indexOf rule.Before update < indexOf rule.After update
    match rules with
    | [] -> []
    | rule::tail -> (if isRuleSatisfied rule update then [] else [rule]) @ brokenRules tail update

let allRulesSatisfied rules update = brokenRules rules update = []

let rec fixOrdering rules update =
    let swap lst v1 v2 = lst |> List.map (fun v -> if v = v1 then v2 else if v = v2 then v1 else v)
    match brokenRules rules update with
    | [] -> update
    | brokenRule::_ -> fixOrdering rules (swap update brokenRule.Before brokenRule.After) 

let middlePageNumber (update: Update) = update[update.Length / 2]

let rules, updates = readInput "input.dat"

updates 
|> List.filter (allRulesSatisfied rules) 
|> List.map middlePageNumber |> List.sum |> part1

updates 
|> List.filter (not << allRulesSatisfied rules)
|> List.map (fixOrdering rules) 
|> List.map middlePageNumber |> List.sum |> part2