open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

type Action =
    | North of int
    | South of int
    | East of int
    | West of int
    | Left of int
    | Right of int
    | Forward of int

let deserialize (line: string) = 
    let matcher = Regex.Match(line, @"^(.)([0-9]*)$")
    let moveType = matcher.Groups.[1].Value
    let value = matcher.Groups.[2].Value
    match moveType with
    | "N" -> North(int value)
    | "S" -> South(int value)
    | "E" -> East(int value)
    | "W" -> West(int value)
    | "L" -> Left(int value)
    | "R" -> Right(int value)
    | "F" -> Forward(int value)
    | _ -> failwithf "Unexpected action %A" line

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize

let walkActions (index: int) (input: Action array) =
    let rec inner ((x,y): int * int) (facing: int) (index: int) (input: Action array) =
       //let output = if index > 0 then (printfn "%A -> %A, %A boat %A, %A" input.[index-1] i j x y) else ()
        if index = input.Length then
            x, y
        else
            match input.[index] with
            | North(z) -> inner (x, y+z) facing (index+1) input
            | South(z) -> inner (x, y-z) facing (index+1) input
            | East(z) -> inner (x+z, y)  facing (index+1) input
            | West(z) -> inner (x-z, y)  facing (index+1) input
            | Left(z) ->
                inner (x,y) (facing-z) (index+1) input
            | Right(z) ->
                inner (x,y) (facing+z) (index+1) input
            | Forward(z) ->
                match facing%360 with
                | 0 -> inner (x, y+z) facing (index+1) input
                | 90 | -270 -> inner (x+z, y)  facing (index+1) input
                | 180 | -180 -> inner (x, y-z) facing (index+1) input
                | 270 | -90 -> inner (x-z, y)  facing (index+1) input
                | _ -> failwithf "Unexpected facing: %A" (facing%360)

    inner (0,0) 90 index input

let rotateWaypoint ((x,y): int * int) (rotation: int) =
    match rotation%360 with
    | 0 -> (x,y)
    | 90 | -270 -> (y, -x)
    | 180 | -180-> (-x, -y)
    | 270 | -90 -> (-y, x)
    | _ -> failwithf "unexpected rotation: %A" (rotation%360) 

let walkWaypointActions (index: int) (input: Action array) =
    let rec inner ((i, j): int * int) ((x, y): int * int) (index: int) (input: Action array) =
        //let output = if index > 0 then (printfn "%A -> %A, %A boat %A, %A" input.[index-1] i j x y) else ()
        if index = input.Length then
            x, y
        else
            match input.[index] with
            | North(z) -> inner (i, j+z) (x,y) (index+1) input
            | South(z) -> inner (i, j-z) (x,y)  (index+1) input
            | East(z) -> inner (i+z, j) (x,y) (index+1) input
            | West(z) -> inner (i-z, j) (x,y) (index+1) input
            | Left(z) ->
                let newWaypoint = rotateWaypoint (i, j) -z
                inner newWaypoint (x,y) (index+1) input
            | Right(z) ->
                let newWaypoint = rotateWaypoint (i, j) z
                inner newWaypoint (x,y) (index+1) input
            | Forward(z) ->
                inner (i,j) (x+(i*z),y+(j*z)) (index+1) input

    inner (10,1) (0,0) index input

let manhattanDistance ((a, b): int * int) ((x,y): int*int) =
    (abs ((abs (a-x)) + (abs (b-y))))

let resultLoc1 = (walkActions 0 inputLines)
let result1 = manhattanDistance (0,0) resultLoc1


let resultLoc2 = (walkWaypointActions 0 inputLines)
let result2 =  manhattanDistance (0,0) resultLoc2

#time
printfn "%A,%A" result1 result2