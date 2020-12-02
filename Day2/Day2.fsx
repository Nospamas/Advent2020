open System
open System.IO


let inputLines = 
    File.ReadAllLines("./input.txt")

let deserialize (line: string) =
    let a = line.Split ':'
    let password = a.[1].Trim()
    let b = a.[0].Split ' '
    let letter = b.[1]
    let c = b.[0].Split '-'
    let minInstances = c.[0]
    let maxInstances = c.[1]

    (Convert.ToInt32(minInstances), Convert.ToInt32(maxInstances), Convert.ToChar(letter), password)

let isValid ((minInstances, maxInstances, letter, password) : int*int*char*string)  =
    let instanceCount =
        password
        |> Seq.filter (fun char -> char = letter)
        |> Seq.length
    
    instanceCount >= minInstances && instanceCount <= maxInstances

let isValid2 ((firstIndex, secondIndex, letter, password) : int*int*char*string)  =
    ((password.[firstIndex - 1] = letter) <> (password.[secondIndex-1] = letter))

let isGood = 
    inputLines
    |> Seq.map deserialize
    |> Seq.filter isValid2

printf "%A" (Seq.length isGood)
