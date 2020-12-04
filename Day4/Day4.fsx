open System
open System.IO
open System.Text.RegularExpressions

let blankLine = (Environment.NewLine+Environment.NewLine)
let inputLines = 
    File.ReadAllText("./input.txt").Split( [| blankLine |], StringSplitOptions.None)

let hasElement (element: string) (line: (string *string) seq) =
    match (Seq.tryFind (fun item -> (fst item) = element) line) with
    | None -> false
    | Some(_) -> true 

let deserialize (line: string) = 
    let pieces = 
        line.Split( [| Environment.NewLine |], StringSplitOptions.None)
        |> Seq.collect (fun piece -> piece.Split(' '))
        |> Seq.map ( fun piece -> 
            let kvp = piece.Split(':')
            kvp.[0], kvp.[1])
    
    pieces
    
    
let isValid (pieces : (string *string) seq) = 
        (hasElement "byr" pieces)
        && (hasElement "iyr" pieces)
        && (hasElement "eyr" pieces)
        && (hasElement "hgt" pieces)
        && (hasElement "hcl" pieces)
        && (hasElement "ecl" pieces)
        && (hasElement "pid" pieces)

let isBirthYearValid (b: string) = 
    try
        let byr = Convert.ToInt32(b)
        let isValid = byr >= 1920 && byr <= 2002
        if not isValid then 
            printfn "Birth Year Invalid: %A" b
        isValid
    with
    | :? System.Exception -> false


let isIssueYearValid (b: string) = 
    try
        let byr = Convert.ToInt32(b)
        let isValid = byr >= 2010 && byr <= 2020
        if not isValid then 
            printfn "Issue Year Invalid: %A" b
        isValid

    with
    | :? System.Exception -> false

let isExpiryYearValid (b: string) = 
    try
        let byr = Convert.ToInt32(b)
        let isValid = byr >= 2020 && byr <= 2030
        if not isValid then 
            printfn "Expiry Year Invalid: %A" b
        isValid

    with
    | :? System.Exception -> false

let isHeightValid (b: string) = 
    try
        match b.Contains("cm"), b.Contains("in") with
        | true, _ -> 
            let byr = Convert.ToInt32( b.Split( [| "cm" |], StringSplitOptions.None).[0] )
            let isValid = byr >= 150 && byr <= 193
            if not isValid then 
                printfn "Height Invalid: %A" b
            isValid
        | _, true -> 
            let byr = Convert.ToInt32(b.Split([| "in" |], StringSplitOptions.None).[0])
            let isValid = byr >= 59 && byr <= 76
            if not isValid then 
                printfn "Height Invalid: %A" b
            isValid

    with
    | :? System.Exception -> 
        printfn "Height Failed: %A" b
        false

let isHairValid (b: string) =
    let m = Regex.Match(b, @"^#[0-9a-f]{6}$")
    if not m.Success then
        printfn "Hair Invalid: %A" b
    m.Success 

let isEyeColourValid (b: string) =
    match b with
    | "amb" -> true
    | "blu" -> true
    | "brn" -> true
    | "gry" -> true
    | "grn" -> true
    | "hzl" -> true
    | "oth" -> true
    | _ -> 
        printfn "Eye Invalid: %A" b
        false

let isPasportIdValid (b: string) = 
    let m = Regex.Match(b, @"^\d{9}$")
    if not m.Success then
        printfn "Passport Invalid: %A" b
    m.Success 

let getPiece (key: string) (pieces : (string * string) seq) =
    pieces
    |> Seq.find (fun piece -> (fst piece) = key)
    |> snd

let isValid2 (pieces : (string * string) seq) =
    let byr = (getPiece "byr" pieces)
    let birthValid = (isBirthYearValid byr), byr, "birth"
    let iyr = (getPiece "iyr" pieces)
    let issueValid = (isIssueYearValid iyr), iyr, "issue"
    let eyr = (getPiece "eyr" pieces)
    let expiryValid = (isExpiryYearValid eyr), eyr, "expiry"
    let hgt =  (getPiece "hgt" pieces)
    let heightValid = (isHeightValid hgt), hgt, "height"
    let hcl = (getPiece "hcl" pieces)
    let hairValid = (isHairValid hcl), hcl, "hair"
    let ecl = (getPiece "ecl" pieces)
    let eyeColourValid = (isEyeColourValid ecl), ecl, "eye"
    let pid = (getPiece "pid" pieces)
    let passportIdValid = (isPasportIdValid pid), pid, "passid"

    [birthValid; issueValid; expiryValid; heightValid; hairValid; eyeColourValid; passportIdValid]

let passports = 
    inputLines
    |> Seq.map deserialize
    |> Seq.filter isValid
    |> Seq.map isValid2
    |> Seq.filter (fun ppt ->
        ppt
        |> Seq.map (fun (t, _, _) -> t) 
        //|> Seq.contains false
        |> Seq.forall (fun item -> item)
    )
    // |> Seq.map (fun item ->
    //     printfn "%A" item
    //     item
    // )
    |> Seq.length

printfn "%A" passports