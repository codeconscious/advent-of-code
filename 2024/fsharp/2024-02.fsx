let lines = System.IO.File.ReadAllLines("input/2024/02.txt")

let parseInput (lines: string array) =
    lines |> Array.map (fun l -> l.Split(' ') |> Array.map System.Int32.Parse)

module Validation =
    // Refactored from a lovely Kotlin solution employed by another engineer at work.
    let validate row =
        let isInRange intervals =
            intervals |> Seq.forall (fun i -> Seq.contains i [|1..3|]) ||
            intervals |> Seq.forall (fun i -> Seq.contains i [|-3..-1|])
        row |> Seq.pairwise |> Seq.map (fun (x, y) -> x - y) |> isInRange

module Puzzles =
    open Validation

    let private fullyValid, invalid =
        lines
        |> parseInput
        |> Array.partition validate

    let private dampenedValid =
        // Adapted from the code sample at https://stackoverflow.com/a/1231711/11767771.
        let rec combinations count arr =
            match count, arr with
            | 0, _ -> [[]]
            | _, [||] -> []
            | k, _ ->
                let x = arr[0]
                let xs = arr[1..]
                List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

        invalid
        |> Array.filter (fun row ->
            row
            |> combinations (row.Length - 1)
            |> List.exists validate)

    let puzzle1 = fullyValid.Length
    let puzzle2 = fullyValid.Length + dampenedValid.Length

Puzzles.puzzle1 |> printfn "%d"
Puzzles.puzzle2 |> printfn "%d"
