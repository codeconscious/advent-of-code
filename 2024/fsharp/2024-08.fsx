open System
open type System.Environment

let grid =
    System.IO.File.ReadAllLines("input/2024/08.txt")
    |> Array.map _.ToCharArray()
    |> array2D

type Cell = { Y: int; X: int }

let antennaCells =
    [| for y in 0..(Array2D.length1 grid)-1 do
       for x in 0..(Array2D.length2 grid)-1 do
       let ch = grid[y,x]
       if Char.IsDigit(ch) || Char.IsLetter(ch)
       then yield { Y = y; X = x } |]

let isOutOfBounds cell =
    cell.Y < 0 ||
    cell.X < 0 ||
    cell.Y >= Array2D.length1 grid ||
    cell.X >= Array2D.length2 grid

module Part1 =
    let verifyAntiNode (control: Cell, test: Cell) =
        if control = test
        then None
        elif grid[control.Y, control.X] <> grid[test.Y, test.X]
        then None
        else
            let yOffset, xOffset = (test.Y - control.Y, test.X - control.X)
            let antiNodeCell = { Y = control.Y - yOffset; X = control.X - xOffset }
            if isOutOfBounds antiNodeCell
            then None
            else Some antiNodeCell

    (antennaCells, antennaCells)
    ||> Array.allPairs
    |> Array.choose verifyAntiNode
    |> Array.distinct
    |> _.Length
    |> printfn "%d" // 359

module Part2 =
    let generateAntiNodes (control: Cell, test: Cell) =
        let generator yOffset xOffset operator cell =
             if isOutOfBounds cell
             then None
             else Some (cell, { Y = operator cell.Y yOffset
                                X = operator cell.X xOffset })

        if control = test
        then None
        else
            let yOffset, xOffset = test.Y - control.Y, test.X - control.X
            let generateViaOffsets = generator yOffset xOffset
            let backwardCells = control |> Array.unfold (generateViaOffsets (-))
            let forwardCells = control |> Array.unfold (generateViaOffsets (+))
            Some [| control; test; yield! backwardCells; yield! forwardCells |]

    antennaCells
    |> Array.groupBy (fun c -> grid[c.Y, c.X])
    |> Array.map (fun (_, c) -> (c, c) ||> Array.allPairs |> Array.choose generateAntiNodes)
    |> Array.collect (fun a -> a |> Array.collect id)
    |> Array.distinct
    |> _.Length
    |> printfn "%d" // 1293