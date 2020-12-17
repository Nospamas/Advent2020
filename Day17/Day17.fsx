open System.IO
open System
open System.Collections
open System.Text.RegularExpressions
open System.Collections.Generic
#time 
type PState =
  | Active
  | Inactive

let toPState =
  function
  | '.' -> Inactive
  | '#' -> Active
  | x -> failwithf "Unexpected character: %A" x

let deserialize (line: string) = line.ToCharArray() |> Array.map toPState

let inputLines =
  [| File.ReadAllLines("./input.txt")
     |> Array.map deserialize |]


let neighbours =
  [| (-1, 1, 1)
     (0, 1, 1)
     (1, 1, 1)
     (-1, 0, 1)
     (0, 0, 1)
     (1, 0, 1)
     (-1, -1, 1)
     (0, -1, 1)
     (1, -1, 1)


     (-1, 1, 0)
     (0, 1, 0)
     (1, 1, 0)
     (-1, 0, 0)
     (1, 0, 0)
     (-1, -1, 0)
     (0, -1, 0)
     (1, -1, 0)


     (-1, 1, -1)
     (0, 1, -1)
     (1, 1, -1)
     (-1, 0, -1)
     (0, 0, -1)
     (1, 0, -1)
     (-1, -1, -1)
     (0, -1, -1)
     (1, -1, -1) |]

let neighbours2 =
  [| (1, -1, 1, 1)
     (1, 0, 1, 1)
     (1, 1, 1, 1)
     (1, -1, 0, 1)
     (1, 0, 0, 1)
     (1, 1, 0, 1)
     (1, -1, -1, 1)
     (1, 0, -1, 1)
     (1, 1, -1, 1)

     (1, -1, 1, 0)
     (1, 0, 1, 0)
     (1, 1, 1, 0)
     (1, -1, 0, 0)
     (1, 0, 0, 0)
     (1, 1, 0, 0)
     (1, -1, -1, 0)
     (1, 0, -1, 0)
     (1, 1, -1, 0)

     (1, -1, 1, -1)
     (1, 0, 1, -1)
     (1, 1, 1, -1)
     (1, -1, 0, -1)
     (1, 0, 0, -1)
     (1, 1, 0, -1)
     (1, -1, -1, -1)
     (1, 0, -1, -1)
     (1, 1, -1, -1)
     // d2
     (0, -1, 1, 1)
     (0, 0, 1, 1)
     (0, 1, 1, 1)
     (0, -1, 0, 1)
     (0, 0, 0, 1)
     (0, 1, 0, 1)
     (0, -1, -1, 1)
     (0, 0, -1, 1)
     (0, 1, -1, 1)

     (0, -1, 1, 0)
     (0, 0, 1, 0)
     (0, 1, 1, 0)
     (0, -1, 0, 0)
     (0, 1, 0, 0)
     (0, -1, -1, 0)
     (0, 0, -1, 0)
     (0, 1, -1, 0)

     (0, -1, 1, -1)
     (0, 0, 1, -1)
     (0, 1, 1, -1)
     (0, -1, 0, -1)
     (0, 0, 0, -1)
     (0, 1, 0, -1)
     (0, -1, -1, -1)
     (0, 0, -1, -1)
     (0, 1, -1, -1)
     // d3
     (-1, -1, 1, 1)
     (-1, 0, 1, 1)
     (-1, 1, 1, 1)
     (-1, -1, 0, 1)
     (-1, 0, 0, 1)
     (-1, 1, 0, 1)
     (-1, -1, -1, 1)
     (-1, 0, -1, 1)
     (-1, 1, -1, 1)

     (-1, -1, 1, 0)
     (-1, 0, 1, 0)
     (-1, 1, 1, 0)
     (-1, -1, 0, 0)
     (-1, 0, 0, 0)
     (-1, 1, 0, 0)
     (-1, -1, -1, 0)
     (-1, 0, -1, 0)
     (-1, 1, -1, 0)

     (-1, -1, 1, -1)
     (-1, 0, 1, -1)
     (-1, 1, 1, -1)
     (-1, -1, 0, -1)
     (-1, 0, 0, -1)
     (-1, 1, 0, -1)
     (-1, -1, -1, -1)
     (-1, 0, -1, -1)
     (-1, 1, -1, -1) |]

printfn
    "%A"
    (neighbours2
    |> Seq.distinct
    |> Seq.length)

let expandSpace (input: PState array array array) =
  let z = (input.Length + 1)
  let y = (input.[0].Length + 1)
  let x = (input.[0].[0].Length + 1)

  [| for i in 0 .. z do
       [| for j in 0 .. y do
            [| for k in 0 .. x do
                 match i, j, k with
                 | 0, _, _
                 | _, 0, _
                 | _, _, 0 -> Inactive
                 | a, _, _ when a = z -> Inactive
                 | _, a, _ when a = y -> Inactive
                 | _, _, a when a = x -> Inactive
                 | _, _, _ -> input.[i - 1].[j - 1].[k - 1] |] |] |]

let expandSpace2 (input: PState array array array array) =
  let w = (input.Length + 1)
  let z = (input.[0].Length + 1)
  let y = (input.[0].[0].Length + 1)
  let x = (input.[0].[0].[0].Length + 1)

  [| for i in 0 .. w do
       [| for j in 0 .. z do
            [| for k in 0 .. y do
                 [| for l in 0 .. x do
                      match i, j, k, l with
                      | 0, _, _, _
                      | _, 0, _, _
                      | _, _, 0, _
                      | _, _, _, 0 -> Inactive
                      | a, _, _, _ when a = w -> Inactive
                      | _, a, _, _ when a = z -> Inactive
                      | _, _, a, _ when a = y -> Inactive
                      | _, _, _, a when a = x -> Inactive
                      | _, _, _, _ -> input.[i - 1].[j - 1].[k - 1].[l - 1] |] |] |] |]

let safeGetPState (full: PState array array array) ((z, y, x): int * int * int) =
  if (z < 0) || (y < 0) || (x < 0) then Inactive
  else if z >= full.Length then Inactive
  else if y >= full.[0].Length then Inactive
  else if x >= full.[0].[0].Length then Inactive
  else full.[z].[y].[x]

let translate (z, y, x) (a, b, c) = (z + a), (y + b), (x + c)


let ifActive1 =
  function
  | Active -> 1
  | Inactive -> 0

let countActive (state': PState array array array) =
  state'
  |> Seq.sumBy (fun layer ->
       layer
       |> Seq.sumBy (fun column -> column |> Seq.sumBy ifActive1))

let printState (state': PState array array array) =
  state'
  |> Seq.iter (fun layer ->
       printfn "---------"

       layer
       |> Seq.iter (fun column ->
            column
            |> Seq.iter (fun state ->
                 match state with
                 | Active -> (printf "#")
                 | Inactive -> (printf "."))

            printfn ""))

let printState2 (state': PState array array array array) =
  state'
  |> Seq.iteri (fun di d4 ->
       printfn "---------"

       printfn "%A" di

       d4
       |> Seq.iter (fun layer ->
            
            printfn "-------"
            layer
            |> Seq.iter (fun column ->
                 column
                 |> Seq.iter (fun state ->
                      match state with
                      | Active -> (printf "#")
                      | Inactive -> (printf "."))

                 printfn "")))

let processCell (full: PState array array array) (z: int) (y: int) (x: int) (cell: PState) =
  let activeNeighbourCount =
    neighbours
    |> Seq.map (translate (z, y, x))
    |> Seq.map (safeGetPState full)
    |> Seq.sumBy (ifActive1)
  //printfn "%A" activeNeighbourCount
  match cell with
  | Active ->
      if activeNeighbourCount = 2
         || activeNeighbourCount = 3 then
        Active
      else
        Inactive
  | Inactive -> if activeNeighbourCount = 3 then Active else Inactive

let processColumn (full: PState array array array) (z: int) (y: int) (column: PState array) =
  column |> Array.mapi (processCell full z y)

let processLayer (full: PState array array array) (z: int) (layer: PState array array) =
  layer |> Array.mapi (processColumn full z)

let cycle (maxCycle: int) (input: PState array array array) =
  let rec inner (cycleCount: int) (maxCycle: int) (state': PState array array array) =
    //printfn "%A" (state' |> countActive)
    //(printState state')
    if (cycleCount = maxCycle) then
      state'
    else
      // expand so we can check boundry items
      let newSpace = (expandSpace state')
      // process this cycle
      let nextState =
        newSpace |> Array.mapi (processLayer newSpace)

      //(printState newSpace)
      //(printState nextState)

      inner (cycleCount + 1) maxCycle nextState

  inner 0 maxCycle input

let resultGrid = (cycle 6 inputLines)


let countActive2 (state': PState array array array array) =
  state'
  |> Seq.sumBy (fun d4 ->
       d4
       |> Seq.sumBy (fun layer ->
            layer
            |> Seq.sumBy (fun column -> column |> Seq.sumBy ifActive1)))

let safeGetPState2 (full: PState array array array array) ((w, z, y, x): int * int * int * int) =
  if (w < 0) || (z < 0) || (y < 0) || (x < 0) then Inactive
  else if w >= full.Length then Inactive
  else if z >= full.[0].Length then Inactive
  else if y >= full.[0].[0].Length then Inactive
  else if x >= full.[0].[0].[0].Length then Inactive
  else full.[w].[z].[y].[x]

let translate2 (w, z, y, x) (a, b, c, d) = (w + a), (z + b), (y + c), (x + d)

let processCell2 (full: PState array array array array)  (w: int) (z: int) (y: int) (x: int) (cell: PState) =
  let activeNeighbourCount =
    neighbours2
    |> Seq.map (translate2 (w, z, y, x))
    |> Seq.map (safeGetPState2 full)
    |> Seq.sumBy (ifActive1)
  //printfn "%A" activeNeighbourCount
  match cell with
  | Active ->
      if activeNeighbourCount = 2
         || activeNeighbourCount = 3 then
        Active
      else
        Inactive
  | Inactive -> if activeNeighbourCount = 3 then Active else Inactive

let processColumn2 (full: PState array array array array) (w: int) (z: int) (y: int) (column: PState array) =
  column |> Array.mapi (processCell2 full w z y)

let processLayer2 (full: PState array array array array) (w: int) (z: int) (layer: PState array array) =
  layer |> Array.mapi (processColumn2 full w z)

let process4d (full: PState array array array array) (w: int) (d4: PState array array array) =
  d4 |> Array.mapi (processLayer2 full w)

let input2 = [| inputLines |]

let cycle2 (maxCycle: int) (input: PState array array array array) =
  let rec inner (cycleCount: int) (maxCycle: int) (state': PState array array array array) =
    //printfn "%A" (state' |> countActive2)
    //(printState2 state')

    if (cycleCount = maxCycle) then
      state'
    else
      // expand so we can check boundry items
      let newSpace = (expandSpace2 state')
      // process this cycle
      let nextState =
        newSpace |> Array.mapi (process4d newSpace)

      //(printState2 newSpace)
      //(printState2 nextState)

      inner (cycleCount + 1) maxCycle nextState

  inner 0 maxCycle input

let resultGrid2 = (cycle2 6 input2)
let result1 = resultGrid |> countActive
let result2 = resultGrid2 |> countActive2

#time
printfn "%A, %A" result1 result2
