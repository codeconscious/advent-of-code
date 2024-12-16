open System
open System.Diagnostics

let lines = System.IO.File.ReadAllLines("input/2024/07.txt")

let parseInput (lines: string array) =
    lines
    |> Array.map _.Split([| ':'; ' ' |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x |> Array.map Int64.Parse)
    |> Array.map (fun x -> Array.head x, Array.tail x)

module Operators =
    type Operator = Operator of (int64 -> int64 -> int64)
    let operatorsPart1 = [| Operator (+); Operator (*) |]
    let private combineInts x y = $"{x}{y}" |> Int64.Parse // It would be better to avoid a string-based approach.
    let operatorsPart2 = operatorsPart1 |> Array.append <| [| Operator combineInts |]

module Utilities =
    // This function required some external reference, but it should prove helpful in the future.
    let rec generateCombinations length xs : 'a array array =
        let rec loop acc length =
            if length = 0
            then [| acc |]
            else
                [| for x in xs do
                   let newPrefix = Array.append acc [| x |]
                   yield! loop newPrefix (length - 1) |]

        if length <= 0
        then [||]
        else loop [||] length

module Logic =
    open Operators
    open Utilities

    type private Equation =
        { Test: int64
          Elements: {| Head: int64
                       Tail: int64 array |} }

    let private toEquation lineData =
        { Test = fst lineData
          Elements = {| Head = snd lineData |> Array.head
                        Tail = snd lineData |> Array.tail |} }

    let private verifyEquation operators equation =
        let evaluateCombination operatorSet =
            let folder state = function
                Operator o, i -> o state i // Must maintain arg order for part 2.

            equation.Elements.Tail
            |> Array.zip operatorSet
            |> Array.fold folder equation.Elements.Head

        operators
        |> generateCombinations equation.Elements.Tail.Length
        |> Array.map evaluateCombination
        |> Array.exists (fun x -> x = equation.Test)
        |> fun result -> if result then equation.Test else 0

    let solve operators =
        lines
        |> parseInput
        |> Array.map toEquation
        |> Array.map (fun equation -> equation |> verifyEquation operators)
        |> Array.sum

open Operators
open Logic

let measureTime label f =
    let startTime = Stopwatch.GetTimestamp()
    let result = f ()
    printfn $"""%s{label}: %d{result} ({Stopwatch.GetElapsedTime(startTime).TotalMilliseconds.ToString("N2")}ms)"""

measureTime "前" (fun _ -> solve operatorsPart1) // A: 1985268524462
measureTime "後" (fun _ -> solve operatorsPart2) // B: 150077710195188
