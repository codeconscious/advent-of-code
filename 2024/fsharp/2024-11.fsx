open System

let input =
    IO.File.ReadAllText("input/2024/11.txt").TrimEnd().Split(' ')
    |> List.ofArray

let checkStone stone =
    let cleanString s = UInt64.Parse(s) |> string

    match stone with
    | "0" -> ["1"]
    | s when s.Length % 2 = 0 ->
        if s[0] = '-' then failwith $"Invalid data '{s}'"

        let halfway = s.Length / 2
        let left = s[..halfway - 1] |> cleanString
        let right = s[halfway..] |> cleanString
        [left; right]
    | s -> [string (UInt64.Parse(s) * 2024UL)]

let combinedCollect times list =
    List.fold (fun acc _ -> List.collect checkStone acc) list [1..times]

input
|> combinedCollect 25
|> _.Length
|> printfn "%d"
