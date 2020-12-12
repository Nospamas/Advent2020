open System.IO
open System
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Diagnostics

let deserialize (line: string) = 
    line

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize