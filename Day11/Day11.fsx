open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

type Seat = 
    | Empty
    | Floor
    | Occupied
    | Edge

let charToSeat (seat: char) =
    match seat with
    | 'L' -> Empty
    | '#' -> Occupied
    | '.' -> Floor
    | _ -> failwith "unexpected character for seat"

let deserialize (line: string) = 
    line
    |> Seq.map charToSeat
    |> Seq.toArray

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Seq.map deserialize
    |> Seq.toArray

let checkLocations = [| (-1, -1); (-1, 0); (-1, 1); (0, 1); (1, 1); (1, 0); (1, -1); (0, -1) |]

let safeAccessDefaultEdge (lastState: Seat array array) (x: int) (y: int) =
    if x >= lastState.Length || x < 0 then
        Edge
    elif y >= lastState.[0].Length || y < 0 then
        Edge
    else
        lastState.[x].[y]

let allAdjacentEmpty (lastState: Seat array array) (rowIndex: int) (columnIndex: int) =
    let hasOccupiedSeat =
        checkLocations
        |> Seq.exists (fun (xOffset, yOffset) -> 
            match (safeAccessDefaultEdge lastState (rowIndex+xOffset) (columnIndex+yOffset)) with
            | Occupied -> true
            | _ -> false
        )
    
    not hasOccupiedSeat

let has4OccupiedAdjacent (lastState: Seat array array) (rowIndex: int) (columnIndex: int) =
    let totalOccupiedAdjacent =
        checkLocations
        |> Seq.filter (fun (xOffset, yOffset) -> 
            match (safeAccessDefaultEdge lastState (rowIndex+xOffset) (columnIndex+yOffset)) with
            | Occupied -> true
            | _ -> false
        )
        |> Seq.length
    
    totalOccupiedAdjacent >= 4 

let processSeatStep (lastState: Seat array array) (rowIndex: int) (columnIndex: int) (seat: Seat) =
    match seat with
    | Floor -> Floor
    | Empty ->
        match (allAdjacentEmpty lastState rowIndex columnIndex) with
        | true -> Occupied
        | false -> Empty
    | Occupied ->
        match (has4OccupiedAdjacent lastState rowIndex columnIndex) with
        | true -> Empty
        | false -> Occupied
    | Edge -> failwith "We shouldn't see an edge on the ferry."

let processRowStep (lastState: Seat array array) (rowIndex: int) (row: Seat array)  =
    row
    |> Array.mapi (processSeatStep lastState rowIndex)


let processFerryStep (inputLines: Seat array array) =
    inputLines
    |> Array.mapi (processRowStep inputLines)

let getUnchangedState (input: Seat array array) =
    let rec inner (acc: int) (current: Seat array array) =
        let next = (processFerryStep current)
        if current = next then
            next
        else
            if acc % 10 = 0 then printfn "%A" acc
            inner (acc+1) (next)
    (inner 0 input)

let resultFerry = (getUnchangedState inputLines)

let countFerry (ferry: Seat array array) =
    ferry
    |> Seq.sumBy (fun row ->
        row
        |> Seq.sumBy (fun seat ->
            match seat with
            | Occupied -> 1
            | _ -> 0
        )
    )

let rec lookForSeat (i: int) (j: int) (x: int) (y: int)  (lastState: Seat array array) (rowIndex: int) (columnIndex: int) =
    match (safeAccessDefaultEdge lastState (rowIndex+x) (columnIndex+y)) with
    | Floor -> (lookForSeat i j (x+i) (y+j) lastState rowIndex columnIndex)
    | x -> x 

let directionFunctions = 
    checkLocations
    |> Array.map ( fun (x,y) ->
        lookForSeat x y x y
    )

let allSeenEmpty (lastState: Seat array array) (rowIndex: int) (columnIndex: int) =
    let hasOccupiedSeat =
        directionFunctions
        |> Seq.exists (fun checker -> 
            match (checker lastState (rowIndex) (columnIndex)) with
            | Occupied -> true
            | _ -> false
        )
    
    not hasOccupiedSeat

let has5OccupiedSeen (lastState: Seat array array) (rowIndex: int) (columnIndex: int) =
    let totalOccupiedAdjacent =
        directionFunctions
        |> Seq.filter (fun checker -> 
            match (checker lastState rowIndex columnIndex) with
            | Occupied -> true
            | _ -> false
        )
        |> Seq.length
    
    totalOccupiedAdjacent >= 5

let processSeatStep2 (lastState: Seat array array) (rowIndex: int) (columnIndex: int) (seat: Seat) =
    match seat with
    | Floor -> Floor
    | Empty ->
        match (allSeenEmpty lastState rowIndex columnIndex) with
        | true -> Occupied
        | false -> Empty
    | Occupied ->
        match (has5OccupiedSeen lastState rowIndex columnIndex) with
        | true -> Empty
        | false -> Occupied
    | Edge -> failwith "Should not see edges on ferry"

let processRowStep2 (lastState: Seat array array) (rowIndex: int) (row: Seat array)  =
    row
    |> Array.Parallel.mapi (processSeatStep2 lastState rowIndex)


let processFerryStep2 (inputLines: Seat array array) =
    inputLines
    |> Array.mapi (processRowStep2 inputLines)

let getUnchangedState2 (input: Seat array array) =
    let rec inner (acc: int) (current: Seat array array) =
        let next = (processFerryStep2 current)
        if current = next then
            next
        else
            if acc % 10 = 0 then printfn "%A" acc
            inner (acc+1) (next)
    (inner 0 input)

let resultFerry2 = (getUnchangedState2 inputLines)

let result1 = (countFerry resultFerry)

let result2 = (countFerry resultFerry2)
    
printfn "%A, %A" result1 result2