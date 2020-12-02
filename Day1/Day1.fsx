open System.IO

let inputLines = 
    File.ReadAllLines("./input.txt")
    |> Seq.map System.Convert.ToInt32

let cartesian xs ys = 
    xs |> Seq.collect (fun x -> ys |> Seq.map (fun y -> x, y))

let result1 = 
    (cartesian inputLines inputLines)
    |> Seq.map (fun (a, b) ->
        a, b, a+b
    )
    |> Seq.find (fun (a, b ,c) -> c = 2020 )
    |> (fun (a, b, c) -> a * b * c)

let result2 = 
    (cartesian inputLines inputLines)
    |> cartesian inputLines
    |> Seq.map (fun (a, (b, c) ) -> a, b, c)
    |> Seq.map (fun (a, b, c) ->
        a, b, c, a+b+c
    )
    |> Seq.find (fun (a, b ,c, d) -> d = 2020 )
    |> (fun (a, b, c, d) -> a * b * c)

printf "%A:%A" result1 result2
