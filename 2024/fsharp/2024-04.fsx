open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("input/2024/04.txt")

type Point = { X: int; Y: int }

module Puzzle1 =
    let xPoints lines =
        lines
        |> Array.mapi (fun i line ->
            Regex.Matches(line, "X")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> { X = m.Index; Y = i })
            |> Seq.toArray)
        |> Array.collect id

    let offsets = ([|-1..1|], [|-1..1|]) ||> Array.allPairs |> Array.removeAt 4 // Discard (0, 0)

    let isValid (point, offsets) (lines: string array) =
        let offsetPointElements points offset =
            if offset = 0
            then [| points |] |> Array.replicate 4 |> Array.concat
            else [| points .. offset .. points + offset * 3 |]

        let xs = offsetPointElements point.X (fst offsets)
        let ys = offsetPointElements point.Y (snd offsets)
        let points = (xs, ys) ||> Array.map2 (fun x y -> (x, y))

        let isOutOfBounds point =
            let isAxisOutOfBounds x upperBound = x < 0 || x >= upperBound

            (isAxisOutOfBounds (fst point) lines[0].Length) ||
            (isAxisOutOfBounds (snd point) lines.Length)

        if points |> Array.exists isOutOfBounds
        then false
        else
            points
            |> Array.map (fun p -> lines[snd p][fst p]) // `snd p` must be first!
            |> Array.forall2 (=) [| 'X'; 'M'; 'A'; 'S' |]

    let run lines =
        offsets
        |> Array.allPairs (xPoints lines)
        |> Array.filter (fun pointOffset -> isValid pointOffset lines)
        |> _.Length

module Puzzle2 =
    let aPoints (lines: string array) =
        let isValid point =
            point.X > 0 && point.X < lines[0].Length-1 &&
            point.Y > 0 && point.Y < lines.Length-1

        lines
        |> Array.mapi (fun i line ->
            Regex.Matches(line, "A")
            |> Seq.cast<Match>
            |> Seq.map (fun m -> { X = m.Index; Y = i })
            |> Seq.toArray)
        |> Array.collect id
        |> Array.filter isValid

    let countValid (lines: string array) points =
        points
        |> Array.filter (fun p ->
            let pair1 = [lines[p.Y-1][p.X-1]; lines[p.Y+1][p.X+1]]
            let pair2 = [lines[p.Y+1][p.X-1]; lines[p.Y-1][p.X+1]]
            (pair1 = ['M'; 'S'] || pair1 = ['S'; 'M']) &&
            (pair2 = ['M'; 'S'] || pair2 = ['S'; 'M']))
        |> Array.length

    let run lines =
        lines
        |> aPoints
        |> countValid lines

Puzzle1.run lines |> printfn "%d" // 2536
Puzzle2.run lines |> printfn "%d" // 1875
