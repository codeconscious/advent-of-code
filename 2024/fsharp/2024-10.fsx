open System

#r "nuget: CodeConscious.Startwatch, 0.0.3"

let toGrid (lines: string array) : int array2d =
    lines
    |> Array.map _.ToCharArray()
    |> Array.map (fun cs -> cs |> Array.map (fun c -> Int32.Parse (c.ToString())))
    |> array2D

let grid = toGrid (System.IO.File.ReadAllLines("input/2024/10.txt"))

type Point = { Row: int; Column: int }
type Offset = { Y: int; X: int }

module Logic =
    let cellHeight point = grid[point.Row, point.Column]

    let originPoints : Point list =
        [ for y in 0..(Array2D.length1 grid)-1 do
          for x in 0..(Array2D.length2 grid)-1 do
              if grid[y, x] = 0
              then yield { Row = y; Column = x } ]

    let generateOffsets point target : Point list =
        let offsets =
            [ { Y = -1; X = 0 }
              { Y = 1; X = 0 }
              { Y = 0; X = -1 }
              { Y = 0; X = 1 } ]

        let isInBounds points =
            points.Row >= 0 &&
            points.Column >= 0 &&
            points.Row < Array2D.length1 grid &&
            points.Column < Array2D.length2 grid

        [ for i in 0..offsets.Length-1 do
          let offsetPoint = { Row = point.Row + offsets[i].Y
                              Column = point.Column + offsets[i].X }
          if isInBounds offsetPoint && (cellHeight offsetPoint = target)
          then yield offsetPoint ]

    let rec walk (summitTrails: Point list list) (trail: Point list) : Point list list =
        let highestPoint = trail.Head
        if cellHeight highestPoint = 9
        then trail :: summitTrails
        else
            let nextHeight = cellHeight highestPoint + 1
            let offsets = nextHeight |> generateOffsets highestPoint
            if offsets.IsEmpty
            then summitTrails
            else offsets |> List.fold (fun acc x -> walk acc (x :: trail)) summitTrails

module Puzzles =
    open Logic

    let solve puzzleLogic =
        originPoints
        |> List.collect (fun x -> walk [] [x])
        |> List.groupBy List.last // Group by origin points
        |> List.map puzzleLogic
        |> List.sum

    let puzzleLogic1 = fun (_, x) ->
        x
        |> List.map (fun y -> y |> List.head)
        |> List.distinct
        |> _.Length

    let puzzleLogic2 = fun (_, x) ->
        x
        |> List.distinct
        |> _.Length

open Puzzles

let measureTime label f =
    let watch = Startwatch.Library.Watch()
    let result = f ()
    printfn $"""%s{label}: %d{result}  ({watch.ElapsedFriendly})"""

measureTime "前" (fun _ -> solve puzzleLogic1)
measureTime "後" (fun _ -> solve puzzleLogic2)
