open System.IO

let inputLines = 
    File.ReadAllLines("./input.txt")


let lineWidth = Seq.length inputLines.[0]

let rec move ((left, down): int * int) ((increment, downIncrement): int*int) (trees: int) =
    try
        match inputLines.[down].[left] with
        | '#' -> move ((left+increment)%lineWidth, (down+downIncrement)) (increment, downIncrement) trees+1
        | _ -> move ((left+increment)%lineWidth, (down+downIncrement)) (increment, downIncrement) trees
    with
    | :? System.Exception as e ->
        trees

printfn "%A" (move (3, 1) (3, 1) 0)

let slope1: int64 = int64 (move (1,1) (1,1) 0)
let slope2: int64 = int64 (move (3,1) (3,1) 0)
let slope3: int64 = int64 (move (5,1) (5,1) 0)
let slope4: int64 = int64 (move (7,1) (7,1) 0)
let slope5: int64 = int64 (move (1,2) (1,2) 0)

printfn "%A, %A" (slope1, slope2, slope3, slope4, slope5) (slope1*slope2*slope3*slope4*slope5)