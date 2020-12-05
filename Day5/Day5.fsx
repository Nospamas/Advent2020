open System.IO

let inputLines = 
    File.ReadAllLines("./input.txt")

let calculatePosition (line: string) =
    let xpart = line.[0..6]
    let ypart = line.[7..9]

    let x = 
        xpart
        |> Seq.rev
        |> Seq.mapi (fun index letter  ->
            match letter with
            | 'F' -> 0, index
            | 'B' -> 1, index
            | _ -> failwith "bad character"
        )
        |> Seq.sumBy (fun (digit, index) ->
            digit * (pown 2 index)
        )
    
    let y =
        ypart
        |> Seq.rev
        |> Seq.mapi (fun index letter ->
            match letter with
            | 'L' -> 0, index
            | 'R' -> 1, index
            | _ -> failwith "bad character"
        )
        |> Seq.sumBy (fun (digit, index) ->
            digit * (pown 2 index)
        )
    x,y


let calculateId (x, y) =
    (x * 8) + y

let result =
    inputLines
    |> Seq.map calculatePosition
    |> Seq.map calculateId

let result1 = 
    result 
    |> Seq.max

let result2 = 
    (seq {  54 .. 1 .. 930 })
    |> Seq.except result

printfn "%A, %A" result1 result2