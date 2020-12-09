open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let sw = Stopwatch()

sw.Start()
type Op =
    | Acc of int
    | Jmp of int
    | Nop of int

let deserialize (line: string) = 
    match (line.[0..2]) with
    | "acc" -> 
        let opVal = (int (line.Split(' ').[1]))
        Acc(opVal)
    | "jmp" -> 
        let opVal = (int (line.Split(' ').[1]))
        Jmp(opVal)
    | "nop" -> 
        let opVal = (int (line.Split(' ').[1]))
        Nop(opVal)
    | _ -> failwithf "unexpected instruction %A" line

let mutable inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize

let instructionCount = inputLines |> Array.length 

let mutable visited = [| for i in 0..(instructionCount+1) do yield false |]

let rec walkInstruction (lineNumber: int) (acc: int)  =
    if lineNumber = instructionCount then
        printfn "worked!"
        (acc, true)
    else 
        match visited.[lineNumber] with
        | true -> (acc, false)
        | false ->
            let newOp = inputLines.[lineNumber]
            visited.[lineNumber] <- true
            match newOp with
            | Nop(_) -> walkInstruction (lineNumber+1) acc
            | Jmp(x) -> walkInstruction (lineNumber+x) acc
            | Acc(x) -> walkInstruction (lineNumber+1) (acc+x)

let result1 = walkInstruction 0 0

let resetVisited () =
    visited <- [| for i in 0..(instructionCount+1) do yield false |]


let switchOp (lineNumber: int) =
    match inputLines.[lineNumber] with
    | Nop(x) -> inputLines.[lineNumber] <- Jmp(x)
    | Jmp(x) -> inputLines.[lineNumber] <- Nop(x)
    | Acc(x) -> ()

resetVisited()
let rec testTilWorks (lineNumber: int)=
    if lineNumber <> 0 then
        switchOp (lineNumber-1)
    
    switchOp lineNumber
    resetVisited()
    match (walkInstruction 0 0) with
    | (x, true) -> 
        printfn "Modified Instruction on line: %A to %A" lineNumber inputLines.[lineNumber]
        x
    | (x, false) -> testTilWorks (lineNumber+1)

let result2 = testTilWorks 0
    
sw.Stop()

printfn "%A, %A, %A" (fst result1) result2 sw.ElapsedMilliseconds