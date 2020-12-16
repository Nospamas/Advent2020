open System.IO
open System
open System.Collections
open System.Text.RegularExpressions
open System.Collections.Generic

#time
let inputBlocks =
    File.ReadAllText("./input.txt").Split([| "\r\n\r\n"|], StringSplitOptions.None)

let deserializeC (line: string) = 
    let matcher = Regex.Match(line, @"^(.*): (\d+)\-(\d+) or (\d+)\-(\d+)$")

    let name = matcher.Groups.[1].Value
    let range1min = (int matcher.Groups.[2].Value)
    let range1max = (int matcher.Groups.[3].Value)
    let range2min = (int matcher.Groups.[4].Value)
    let range2max = (int matcher.Groups.[5].Value)

    name, range1min, range1max, range2min, range2max 

let criteria = 
    inputBlocks.[0]
        .Split([| Environment.NewLine |], StringSplitOptions.None).[0..]
        |> Seq.map deserializeC
        |> Seq.toArray

let myTicket = 
    inputBlocks.[1]
        .Split([| Environment.NewLine |], StringSplitOptions.None).[1].Split(',')
        |> Array.map int
let nearBy = 
    inputBlocks.[2]
        .Split([| Environment.NewLine |], StringSplitOptions.None).[1..]
        |> Seq.map (fun l ->
            l.Split(',')
            |> Seq.map int
        )

let testRanges (number: int) =
    let rec inner (criteriaIndex: int) (isValid: bool) (testNum: int) =
        if (Seq.length criteria) = criteriaIndex then
            not isValid
        else if isValid then
            not isValid
        else 
            let _, r1Min, r1Max, r2Min, r2Max = (criteria.[criteriaIndex])
            let newValid = ((testNum >= r1Min) && (testNum <= r1Max)) || ((testNum >= r2Min) && (testNum <= r2Max))
            inner (criteriaIndex+1) newValid testNum
    
    inner 0 false number

let errorRate =
    nearBy 
    |> Seq.sumBy (fun ticket ->
        ticket
        |> Seq.filter testRanges
        |> Seq.sum
    )

let nonErrorTickets =
    nearBy
    |> Seq.filter (fun ticket ->
        let error =
            ticket
            |> Seq.filter testRanges
            |> Seq.length
        not (error > 0)
    )
    |> Seq.map Seq.toArray

let numberMatchCriteria ((_, r1Min, r1Max, r2Min, r2Max): (string * int * int * int * int)) (testNum: int) =
    ((testNum >= r1Min) && (testNum <= r1Max)) || ((testNum >= r2Min) && (testNum <= r2Max))

let findIndex crit =
    let rec inner (matchIndex: int) (acc: int Set) =
        if (matchIndex = (Seq.length myTicket)) then
            (crit, acc)
        else 
            let isGood =
                nonErrorTickets
                |> Seq.map (fun ticket -> ticket.[matchIndex])
                |> Seq.forall (numberMatchCriteria crit)
            match isGood with
            | true -> inner (matchIndex+1) (acc.Add(matchIndex))
            | false -> inner (matchIndex+1) (acc)
    
    inner 0 Set.empty

let possibleCriteria =
    criteria
    |> Seq.map findIndex

let assignedCriteria=
    let toCalc =
        possibleCriteria
        |> Seq.map (fun criteria ->
            let name, _, _, _, _ = (fst criteria)
            let possible = (snd criteria)
            name, possible
        )
        |> dict
    
    let rec inner (toCalc: IDictionary<string, int Set>) (calced: Dictionary<string, int>) =
        if (toCalc.Keys.Count = 0) then
            calced
        else 
            let pentry =
                toCalc
                |> Seq.filter (fun kvp -> kvp.Value.Count = 1)
            
            //printfn "%A, %A" pentry (pentry |> Seq.length)
            let entry = (pentry |> Seq.head)
            
            let setValue = (Set.minElement entry.Value)
            calced.Add(entry.Key, setValue)
            let nextCalc =
                toCalc
                |> Seq.filter (fun kvp ->
                    kvp.Key <> entry.Key
                    )
                |> Seq.map (fun kvp ->
                    kvp.Key, (Set.remove setValue kvp.Value)
                )
                |> dict
            inner nextCalc calced
        
        
    inner toCalc (Dictionary<string, int>())

let result2 = 
    assignedCriteria
    |> Seq.filter (fun kvp ->
        kvp.Key.Contains("departure")
    )
    |> Seq.map (fun kvp ->
        myTicket.[kvp.Value]
    )
    |> Seq.map int64
    |> Seq.reduce (*)

#time
printfn "%A,%A" errorRate result2