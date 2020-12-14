open System.IO
open System
open System.Collections
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

type Command =
    | Mask of string
    | Memory of int64 * int64

let deserialize (line: string) =
    match line.StartsWith("mask") with
    | true ->
        let maskVal =
            line.Split(' ').[2]

        Mask(maskVal)
    | false ->
        let matcher =
            Regex.Match(line, @"^mem\[(\d*)\]\s=\s(\d*)$")

        let mAddress = int64 matcher.Groups.[1].Value
        let mValue = int64 matcher.Groups.[2].Value
        Memory(mAddress, mValue)


let inputLines =
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize

let bToInt (vals: BitArray) =
    let rec inner (index: int) (acc: int) (values: BitArray) =
        if (index = values.Length) then
            acc
        else
            let vAdd =
                if values.[index] then (acc + (pown 2 index)) else 0

            inner (index + 1) (acc + vAdd) values

    inner 0 0 vals

// let dictAddOrReplace (d: Dictionary<int, int>) (key: int) (value: int) ->
//     match d.TryGetValue(key) with
//     | true, _ ->

let rec intToBinary (i: int64) =
    match i with
    | 0L
    | 1L -> string i
    | _ ->
        let bit = string (i % 2L)
        (intToBinary (i / 2L)) + bit

let padTo (len: int) (line: string) =
    let x = len - (line.Length+1)
    String([| for _ in 0..x do '0' |])+line
    
let pad36 = padTo 36
let runCommands (input: Command array) =
    let rec inner (line: int) (currentMask: string) (mValues: Dictionary<int64, int64>) (lines: Command array): Dictionary<int64,int64> =
        if line = lines.Length then
            mValues
        else
            match lines.[line] with
            | Mask (x) -> inner (line + 1) x mValues lines
            | Memory (mAddress, mValue) ->
                let maskedValue =
                    (pad36 (intToBinary mValue))
                    |> Seq.mapi (fun index bit ->
                        match currentMask.[index] with
                        | '0' -> '0'
                        | '1' -> '1'
                        | 'X' -> bit
                        | _ -> failwithf "unexpected mask value: %A, %A" currentMask.[index] currentMask)
                    |> Seq.toArray
                    |> String

                //printfn "%s, %d" (pad36 (intToBinary mValue)) (Convert.ToInt64((pad36 (intToBinary mValue)), 2))
                //printfn "%s" (currentMask)
                //printfn "%s, %d" maskedValue (Convert.ToInt64(maskedValue, 2))
                mValues.[mAddress] <- Convert.ToInt64(maskedValue, 2)
                inner (line + 1) currentMask mValues lines

    inner 0 "" (Dictionary<int64, int64>()) input

let foldMask (permutationBinary: string) (offset, acc) c =
   //printfn "%A" permutationBinary
   match c with
   | '0' -> offset, (acc@['0'])
   | '1' -> offset, (acc@['1'])
   | 'X' ->
       (offset+1), (acc@[permutationBinary.[offset]])
   | _ -> failwithf "Unexpected memory bit: %A" c
   

let getPermutation (mask: string) (permutationBinary: string) =
    let permutedAddress =
        mask
        |> Seq.fold (foldMask permutationBinary) (0, [])
        |> snd
        |> Seq.toArray
        |> String
        
    Convert.ToInt64(permutedAddress, 2)
    

let writeToMemory (mValues: Dictionary<int64, int64>) (mAddress: string) (mValue: int64) =
    let totalXes =
        mAddress
        |> Seq.filter (fun x ->
                x = 'X'
        )
        |> Seq.length
    
    let permutations =
        totalXes
        |> (pown 2)
        |> int64
    
    //printfn "Writing %A addresses" (permutations-1L)
    
    for i in 0L..(permutations-1L) do
        let nAddress = (getPermutation mAddress (padTo totalXes (intToBinary i)))
        //printfn "%A <- %A" nAddress mValue
        mValues.[nAddress] <- mValue

let runCommands2 (input: Command array) =
    let rec inner (line: int) (currentMask: string) (mValues: Dictionary<int64, int64>) (lines: Command array): Dictionary<int64,int64> =
        if line = lines.Length then
            //printfn "%A" mValues
            mValues
        else
            match lines.[line] with
            | Mask (x) -> inner (line + 1) x mValues lines
            | Memory (mAddress, mValue) ->
                let maskedAddress =
                    (pad36 (intToBinary mAddress))
                    |> Seq.mapi (fun index bit ->
                        match currentMask.[index] with
                        | '0' -> bit
                        | '1' -> '1'
                        | 'X' -> 'X'
                        | _ -> failwithf "unexpected mask value: %A, %A" currentMask.[index] currentMask)
                    |> Seq.toArray
                    |> String

                writeToMemory mValues maskedAddress mValue
                inner (line+1) currentMask mValues lines
    inner 0 "" (Dictionary<int64, int64>()) input

let result1 =
    (runCommands inputLines)
    |> Seq.sumBy (fun kvp -> kvp.Value)

let result2 =
    (runCommands2 inputLines)
    |> Seq.sumBy (fun kvp -> kvp.Value)

printfn "%A, %A" result1 result2
    
