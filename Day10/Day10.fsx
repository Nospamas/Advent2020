open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

type Tree<'a> =
    | Leaf of 'a
    | Node of 'a * Tree<'a> seq

let deserialize (line: string) = 
    (int line)

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize
    |> Array.sort

let jumps = Dictionary<int, int>() 
jumps.Add(1, 0)
jumps.Add(2, 0)
jumps.Add(3, 0)
let len = inputLines |> Seq.length

let rec walkJolts (index: int) (last: int) (acc: Dictionary<int, int>) (input: int array) =
    if index < len then
        let value = input.[index]
        let diff =  value - last
        acc.[diff] <- acc.[diff] + 1
        (walkJolts (index+1) value acc input)
    else
        acc

(walkJolts 0 0 jumps inputLines)

let result1 = jumps.[1] * (jumps.[3]+1)


let maxJolt = 
    inputLines
    |> Seq.max

// This adds the device and two dummy values to allow us to window all the way through our voltages
let input2 =
    Seq.append inputLines (seq { (maxJolt+3); 1000; 1000 })  
    |> Seq.append (seq {0}) 

let withinRange (target: int) (value: int) =
    value <= (target + 3) && value > target

let dictValOr0 (dict: Dictionary<int, int64>) (target: int) =
    match dict.TryGetValue(target) with
    | (true, value) -> value
    | (false, _) -> 0L

let processConnections (totalConnections: Dictionary<int, int64>) ((jolt, connected): int * int seq)  =
    // each jolt's total connections is anything it can connect to added together
    totalConnections.[jolt] <-
        connected
        |> Seq.sumBy(fun jolt ->
            (dictValOr0 totalConnections jolt)
        )
    totalConnections

let baseDict = Dictionary<int, int64>()
// seed with our device's connections, it will always be one as it connects tot he max jolt+3
baseDict.Add(maxJolt+3, 1L)

let resultDict = 
    input2
    |> Seq.windowed 4
    |> Seq.map (fun values ->
        let current = values.[0]
        let vals = 
            values.[1..3]
            |> Seq.filter (withinRange current)
        (current, vals)
    )
    // start from the top so we can use our seed device 
    |> Seq.rev
    |> Seq.fold processConnections (baseDict)

let result2 = resultDict.[0]

#time
printfn "%A, %A" result1 result2