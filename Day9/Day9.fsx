open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let sw = Stopwatch()

sw.Start()

let deserialize (line: string) = 
    (int64 line)

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize

let rec checkNums (index: int) =
    let checkVal = inputLines.[index]
    let toCheck = inputLines.[(index-25)..index]
    let hasNum = 
        toCheck
        |> Seq.exists (fun num ->
            toCheck
            |> Seq.exists (fun num2 -> (num + num2) = checkVal)
        )
    
    if hasNum then
        checkNums (index+1)
    else
        checkVal
    
let result1 = checkNums 25

let rec findSet (target: int64) (currentIndex: int) =
    let rec addTilOver (target: int64) (startIndex: int) (endIndex: int) =
        let sumOfNums = 
            inputLines.[startIndex..endIndex]
            |> Seq.sum
        
        if (sumOfNums < target) then
            (addTilOver (target) (startIndex) (endIndex+1))
        else
            (sumOfNums = target, startIndex, endIndex)

    let matchedTarget, startI, endI = (addTilOver target currentIndex currentIndex)
    if not matchedTarget then
        (findSet target (currentIndex+1))
    else 
        let min = 
            inputLines.[startI..endI]
            |> Seq.min
        let max = 
            inputLines.[startI..endI]
            |> Seq.max
        
        min+max
         
        
let result2 = (findSet result1 0)

sw.Stop()

printfn "%A, %A, %A" result1 result2 sw.ElapsedMilliseconds