open System
open System.Diagnostics
open type System.Environment

let text = System.IO.File.ReadAllText("input/2024/06.txt").TrimEnd()
let lines = text.Split NewLine
let grid = lines |> Array.map _.ToCharArray() |> array2D

let origin =
    let textIndex = text.Replace(NewLine, String.Empty).IndexOf("^")
    let column = textIndex / lines[0].Length
    let row = textIndex % lines[0].Length
    (column, row)

module SharedLogic =
    [<Literal>]
    let Obstacle = '#'
    let EmptyCell = '.'

    type Direction = Up | Down | Left | Right
    type Coords = { Column: int; Row: int }

    type State = { Coords: Coords
                   Facing: Direction
                   Visited: Coords Set
                   SeenObstacles: (Coords * Direction) list }

    let nextChar state =
        let isOutOfBounds column row =
            column < 0 ||
            row < 0 ||
            column >= Array2D.length1 grid ||
            row >= Array2D.length2 grid

        let column, row =
            match state.Facing with
            | Up -> (state.Coords.Column - 1, state.Coords.Row)
            | Down -> (state.Coords.Column + 1, state.Coords.Row)
            | Left -> (state.Coords.Column, state.Coords.Row - 1)
            | Right -> (state.Coords.Column, state.Coords.Row + 1)

        if isOutOfBounds column row
        then None
        else Some grid[column, row]

    let moveToNext state =
        let nextCoords =
            match state.Facing with
            | Up -> { state.Coords with Column = state.Coords.Column - 1 }
            | Down -> { state.Coords with Column = state.Coords.Column + 1 }
            | Left -> { state.Coords with Row = state.Coords.Row - 1 }
            | Right -> { state.Coords with Row = state.Coords.Row + 1 }

        { state with Coords = nextCoords
                     Visited = state.Visited.Add nextCoords }

    let turnRight state =
        match state.Facing with
        | Up -> { state with Facing = Right }
        | Down -> { state with Facing = Left }
        | Left -> { state with Facing = Up }
        | Right -> { state with Facing = Down }

    let initialState =
        let startCoords = { Column = fst origin; Row = snd origin }

        { Coords = startCoords
          Facing = Up
          Visited = Set.ofList [startCoords]
          SeenObstacles = [] }

module Puzzle1 =
    open SharedLogic

    let rec private check state =
        match nextChar state with
        | None -> state
        | Some Obstacle -> state |> turnRight |> check
        | Some _ -> state |> moveToNext |> check

    let run = check initialState

module Puzzle2 =
    open SharedLogic

    let private logObstacle state =
        { state with SeenObstacles = (state.Coords, state.Facing) :: state.SeenObstacles }

    let private isLooping state =
        state.SeenObstacles |> List.contains (state.Coords, state.Facing)

    let rec private check state =
        match nextChar state with
        | None -> false
        | Some Obstacle ->
            if isLooping state
            then true
            else state |> logObstacle |> turnRight |> check
        | Some _ -> state |> moveToNext |> check

    let private checkGridVariant coords =
        grid[coords.Column, coords.Row] <- Obstacle // Mutation for performance.
        let result = check initialState
        grid[coords.Column, coords.Row] <- EmptyCell // Must revert it back!
        result

    let run visitedCoords =
        visitedCoords
        |> Set.filter checkGridVariant
        |> _.Count

let measureTime f =
    let startTime = Stopwatch.GetTimestamp()
    let result = f ()
    printfn $"""Done in {Stopwatch.GetElapsedTime(startTime).TotalMilliseconds.ToString("N2")}ms"""
    result

let solvePuzzles () =
    let puzzle1State = Puzzle1.run

    puzzle1State
    |> _.Visited
    |> _.Count
    |> printfn "%d" // 5534

    puzzle1State
    |> _.Visited
    |> Puzzle2.run
    |> printfn "%d" // 2262

solvePuzzles |> measureTime
