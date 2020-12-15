open System.IO
open System
open System.Collections
open System.Text.RegularExpressions
open System.Collections.Generic

let deserialize (line: string) =
    line

let inputLines =
    File.ReadAllLines("./input.txt")
    |> Array.map deserialize
