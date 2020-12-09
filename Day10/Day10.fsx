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

sw.Stop()