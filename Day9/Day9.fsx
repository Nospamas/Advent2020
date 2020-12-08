open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let sw = new Stopwatch()

sw.Start()

let deserialize (line: string) = 
    ""

let mutable inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize


sw.Stop()