open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let inputLines = 
    File.ReadAllLines("./input.txt")

let earliest = (int inputLines.[0])

let closestFactor (minFactor: int) (factor: int) = 
    let closest =
        seq { for i in 0 .. 1000000 do 
            i * factor
          }
        |> Seq.takeWhile (fun x ->
            x < minFactor
        )
        |> Seq.rev 
        |> Seq.head
    
    factor, (closest+factor), ((closest+factor)-minFactor)

let serviceBusses =
    inputLines.[1].Split(',')
    |> Seq.filter (fun bus ->
        bus <> "x"
    )
    |> Seq.map int
    |> Seq.map (closestFactor earliest)
    |> Seq.sortBy (fun (factor, closest, difference) ->
        difference
    )

let (bus, nearestTime, difference) =
    serviceBusses
    |> Seq.head

let result1 = 
    bus * (difference)

let serviceBusses2 = 
    inputLines.[1].Split(',')
    |> Seq.mapi (fun index bus ->
         bus,index
    )
    |> Seq.filter (fun (bus, index) ->
        bus <> "x"
    )
    |> Seq.map (fun (bus, index) ->
        (int64 (int bus)), (int64 index)
    )
    |> Seq.toList
    
let earliestStepOffests (busses: (int64*int64) list) =
    let rec inner (offset: int64) (advance: int64) (matched: (int64*int64) list) (busses: (int64*int64) list) =
        // debug: printfn "%A, %A, %A, %A"  offset advance matched busses
        if (Seq.length busses) = 0 then
            offset
        else 
            let nMatch =
                busses
                |> List.filter (fun (bus, index) ->
                    (offset+index)%bus = 0L
                )
            
            match List.length nMatch with
            | 0 -> (inner (offset+advance) advance matched busses)
            | 1 ->
                let (mBus, _) = List.head nMatch
                let remaining = (List.except nMatch busses)
                (inner (offset) (advance*mBus) (nMatch@matched) remaining)
            | _ ->
                // could be handled if it comes up
                failwith "more than one match!"
                
    inner 1L 1L [] busses
    
let result2 = (earliestStepOffests serviceBusses2)
#time
printfn "%A, %A" result1 result2