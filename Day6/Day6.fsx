open System.IO
open System
open System.Linq

let countUnique (line: string) =
    line.Replace("\r", "").Replace("\n","")
    |> Seq.distinct
    |> Seq.length


let countIntersect (line: string) =
    let all = line.Trim().Split( [| "\r\n" |], StringSplitOptions.None)
    let mutable filtered = all.[0].ToCharArray()
    for a in all do
        filtered <- (filtered.Intersect(a.ToCharArray()) |> Seq.toArray)

    filtered.Length

let inputLines = 
    File.ReadAllText("./input.txt").Split( [| "\r\n\r\n" |], StringSplitOptions.None)

let result1 =
    inputLines
    |> Seq.map countUnique
    |> Seq.sum

let result2 = 
    inputLines
    |> Seq.map countIntersect
    |> Seq.sum

printfn "%A, %A" result1 result2

