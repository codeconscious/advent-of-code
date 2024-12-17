open System

let grid = // Grid indexing references columns first -- e.g., grid[y,x].
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

let toDuplicateArrayPair x = (x, x) ||> Array.allPairs

module Part1 =
    let verifyAntinode (control: Cell, test: Cell) =
        if control = test then None
        elif grid[control.Y, control.X] <> grid[test.Y, test.X] then None
        else
            let yOffset, xOffset = (test.Y - control.Y, test.X - control.X)
            let antiNodeCell = { Y = control.Y - yOffset; X = control.X - xOffset }
            if isOutOfBounds antiNodeCell
            then None
            else Some antiNodeCell

    antennaCells
    |> toDuplicateArrayPair
    |> Array.choose verifyAntinode
    |> Array.distinct
    |> _.Length
    |> printfn "%d" // 359

module Part2 =
    let generatePairAntinodes (control: Cell, test: Cell) =
        let generator yOffset xOffset operator cell =
             if isOutOfBounds cell
             then None
             else Some (cell, { Y = operator cell.Y yOffset
                                X = operator cell.X xOffset })

        if control = test
        then None
        else
            let yOffset, xOffset = test.Y - control.Y, test.X - control.X
            let generatorWithOffsets = generator yOffset xOffset
            let antinodes =
                [ (-); (+) ]
                |> List.map (fun operator -> control |> List.unfold (generatorWithOffsets operator))
                |> List.collect id
            Some [| control; test; yield! antinodes |]

    let generateAntinodes (_, cells) =
        cells
        |> toDuplicateArrayPair
        |> Array.choose generatePairAntinodes
        |> Array.collect id

    antennaCells
    |> Array.groupBy (fun c -> grid[c.Y, c.X])
    |> Array.map generateAntinodes
    |> Array.collect id
    |> Array.distinct
    |> _.Length
    |> printfn "%d" // 1293
