open System
open System.Diagnostics

let text = System.IO.File.ReadAllText("input/2024/05.txt").TrimEnd()

type Rule = { Left: int; Right: int }

let rules, dataRows =
    let toInts arr = arr |> Array.map Int32.Parse
    let toRules (arr: int array) = { Left = arr[0]; Right = arr[1] }

    text.Split(Environment.NewLine + Environment.NewLine)
    |> Array.map _.Split(Environment.NewLine)
    |> (fun x ->
        (x[0] |> Array.map _.Split('|') // Rules
              |> Array.map toInts
              |> Array.map toRules
              |> Array.toList,
         x[1] |> Array.map _.Split(',') // Data
              |> Array.map toInts))

let ruleApplies row rule =
    (Array.contains rule.Left row) && (Array.contains rule.Right row)
let isRuleFollowed row rule =
    Array.IndexOf(row, rule.Left) < Array.IndexOf(row, rule.Right)

let isValid row =
    rules
    |> List.forall (fun rule ->
        if ruleApplies row rule
        then isRuleFollowed row rule
        else true)

let middleValue (row: 'a array) = row[row.Length / 2]

module Puzzle1 =
    let solve () =
        dataRows
        |> Array.filter isValid
        |> Array.sumBy middleValue

module Puzzle2 =
    let rec private fixRow row rules =
        let fix row rule =
            if (ruleApplies row rule) && not(isRuleFollowed row rule)
            then
                row[Array.IndexOf(row, rule.Left)] <- rule.Right
                row[Array.IndexOf(row, rule.Right)] <- rule.Left
                row
            else row

        let rec loop row remainingRules =
            match isValid row with
            | true -> row
            | _ ->
                match remainingRules with
                | [] -> loop row rules
                | next::other -> loop (fix row next) other

        loop row rules

    let solve () =
        dataRows
        |> Array.filter (fun r -> not(isValid r))
        |> Array.map (fun r -> fixRow r rules)
        |> Array.sumBy middleValue

let measureTime label f =
    let startTime = Stopwatch.GetTimestamp()
    let result = f ()
    printfn $"""%s{label}: %d{result} ({Stopwatch.GetElapsedTime(startTime).TotalMilliseconds.ToString("N2")}ms)"""

measureTime "前" Puzzle1.solve // 5091
measureTime "後" Puzzle2.solve // 4681 -- nailed it on the first try! 🥳
