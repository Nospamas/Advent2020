open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic

type Content =
    | Contains of int * string
    | Nothing

let deserializeRule (rule: string) =
    match rule.Trim() with 
    | "no other bags" -> Nothing
    | _ ->
        let matcher = Regex.Match(rule.Trim(), @"^(\d+) (.+) bag[s]?\.?")

        let qty = int (matcher.Groups.[1].Value.Trim())
        let color = matcher.Groups.[2].Value.Trim()

        Contains(qty, color)

let deserialize (line: string) =
    let matcher = Regex.Match(line, @"^(.+) bags contain (.*)\.")
    let color = matcher.Groups.[1].Value.Trim()
    let rules = matcher.Groups.[2].Value.Trim().Split(',')
    let rulesDeserialized =
        rules
        |> Seq.map deserializeRule
        |> List.ofSeq
    color, rulesDeserialized

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Seq.map deserialize
    |> dict



let rec walkUp (bagType: string) (allRules: string list) (lines: IDictionary<string, Content list>): string list =
    let matchedRules =
        lines
        |> Seq.filter (fun brule ->
            (brule.Value)
            |> Seq.exists (fun rule -> 
                match rule with 
                | Nothing -> false 
                | Contains(_, b) when b = bagType -> true
                | Contains(_) -> false
            )
        )
    match Seq.length matchedRules with
    | 0 -> 
        allRules
    | _  ->
        matchedRules
        |> Seq.collect ( fun brule ->
            (walkUp brule.Key (brule.Key::allRules) lines)
        )
        |> List.ofSeq

let print (line: 'a) =
    printfn "%A" line

let rec walkDown (bagType: string)  (lines: IDictionary<string, Content list>) : obj list =
    lines.[bagType]
    |> List.collect (fun rule ->
        match rule with
        | Nothing -> []
        | Contains(qty, bType) -> [rule; (walkDown bType lines)] 
    )

let rec walkDownTotal (bagType: string) (lines: IDictionary<string, Content list>): int =
    lines.[bagType]
    |> List.sumBy (fun rule ->
        match rule with
        | Nothing -> 0
        | Contains(qty, bType) -> 
            qty + (qty * (walkDownTotal bType lines))
    )

// validate the regex rules
let rulesTotal = 
    File.ReadAllLines("./input.txt")
    |> Seq.sumBy (fun line -> line.Split(',').Length)

let rulesDict =
    inputLines.Values
    |> Seq.sumBy (fun rules -> rules.Length)

assert(rulesTotal = rulesDict)

let result1 =
    (walkUp "shiny gold" [] inputLines)
    |> Seq.distinct
    |> Seq.length

let result2 = 
    (walkDownTotal "shiny gold" inputLines)


let result3 = 
    (walkDown "shiny gold" inputLines)

printfn "%A, %A" result2 result3

