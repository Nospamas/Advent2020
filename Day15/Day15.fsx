open System.IO
open System
open System.Collections
open System.Text.RegularExpressions
open System.Collections.Generic
#time
let input = 
    File.ReadAllText("./input.txt").Split(',')
    |> Seq.rev
    |> Seq.map int
    |> Seq.toList

let walkNumbers (stopAt: int) (input: int list) =
    let rec inner (stopAt: int) (index: int) (spoken: int list) =
        //let output = if index % 10000 = 0 then (printfn "%A" index)
        if (index = (stopAt)) then
            spoken
        else
            let last::rest  = spoken 
            let matchedTil = 
                rest
                |> List.takeWhile (fun i -> i <> last)
                |> List.length
            
            let nextNum = if matchedTil = (List.length rest) then 0 else (matchedTil+1)
            
            inner stopAt (index+1) (nextNum::spoken)

    inner stopAt ((Seq.length input)) input

let inline getDictionaryOrDefault (memos: Dictionary<int, int>) (value) =
    match memos.TryGetValue(value) with
    | true, x -> x+1
    | false, _ -> -1
    
let walkNumbers2 (stopAt: int) (input: int list) =
    let rec inner (stopAt: int) (index: int) (last:int) (memos: Dictionary<int, int>)  =
        //let output = if index % 10000 = 0 then (printfn "%A" index)
        if (index = (stopAt)) then
            last
        else
            let lastSeenOrZero = (getDictionaryOrDefault memos last)
            memos.[last] <- index-1
            match lastSeenOrZero with
            | -1 ->
                inner stopAt (index+1) (0) memos
            | _ ->
                let newNum = index - lastSeenOrZero
                inner stopAt (index+1) (newNum) memos
            
    
    let inMemos = Dictionary<int, int>()
    
    let last::rest = input
    rest
    |> Seq.rev
    |> Seq.iteri (fun index value ->
        inMemos.[value] <- index
    )
    |> ignore
    
    inner stopAt ((Seq.length input)) last inMemos


let result1 = (Seq.head (walkNumbers 2020 input))
let result2 = (walkNumbers2 30000000 input)

#time
printfn "%A, %A" result1 result2 