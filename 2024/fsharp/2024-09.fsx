open System
open System.Diagnostics
open System.Linq

#r "nuget: CodeConscious.Startwatch, 0.0.3"

let input = System.IO.File.ReadAllText("input/2024/09.txt").TrimEnd()

type Block = Empty | Data of string

let toBlocks (source: string) : Block array =
    [| for i in 0..source.Length-1 do
       let count = Int32.Parse(source[i].ToString())

       if Int32.IsEvenInteger i
       then yield! Enumerable.Repeat(Data((i/2).ToString()), count)
       else yield! Enumerable.Repeat(Empty, count) |]

let checksum blocks =
    blocks
    |> Array.mapi (fun i x -> int64 i, x)
    |> Array.map (fun (i, x) ->
        match x with
        | Data d -> i * Int64.Parse d
        | Empty -> 0)
    |> Array.sum

module Part1 =
    let defragSingle (blocks: Block array) : Block array =
        let rec loop (blocks: Block array) =
            let firstEmptyIndex = blocks |> Array.tryFindIndex _.IsEmpty
            let lastDigitIndex =
                blocks
                |> Array.findIndexBack (fun x ->
                    match x with
                    | Data x -> match Int32.TryParse x with isDigit, _ -> isDigit
                    | Empty -> false)

            match firstEmptyIndex, lastDigitIndex with
            | None, _ -> blocks // Cannot act without empty locations.
            | Some emptyIdx, digitIdx ->
                if emptyIdx > digitIdx
                then blocks
                else
                    blocks[emptyIdx] <- blocks[digitIdx]
                    blocks[digitIdx] <- Empty
                    loop blocks

        loop blocks

module Part2 = // This one took a while and has poor performance. Might revisit it sometime.
    type Range = { Start: int; End: int } // End is intended to be inclusive.

    let defragGroupwise (blocks: Block array) : Block array =
        let lastDataGroupRange (blocks: Block array) (exclusions: Block Set) : Range option =
            let last : int option =
                blocks
                |> Array.tryFindIndexBack (fun x ->
                    x.IsData &&
                    not (exclusions |> Set.exists (fun y -> y = x))) // exists faster than contains?

            match last with
            | None -> None
            | Some l ->
                let first = blocks |> Array.findIndex (fun x -> x = blocks[l])
                Some { Start = first; End = l }

        let measure range = range.End - range.Start + 1

        let firstEmptyRange length blocks : Range option =
            let rangeStartingEmptyBlocks (blocks: Block array) =
                blocks
                |> Array.mapi (fun i x -> i, x)
                |> Array.filter (fun (i, x) ->
                    (i = 0 && x.IsEmpty) || (i > 0 && blocks[i-1].IsData && x.IsEmpty))
                |> Array.map fst

            let toRange (blocks: Block array) startIndex : Range =
                let length =
                    blocks[startIndex..]
                    |> Array.takeWhile (fun x -> x.IsEmpty)
                    |> Array.length

                { Start = startIndex; End = startIndex + length - 1 }

            blocks
            |> rangeStartingEmptyBlocks
            |> Array.map (toRange blocks)
            |> Array.tryFind (fun x -> measure x >= length)

        let rec defrag (blocks: Block array) (processedBlocks: Block Set) : Block array =
            match lastDataGroupRange blocks processedBlocks with
            | None -> blocks
            | Some dataRange ->
                let length = measure dataRange
                match firstEmptyRange length blocks with
                | None -> blocks
                | Some emptyRange ->
                    let dataBlock = blocks[dataRange.Start]
                    if emptyRange.Start > dataRange.Start ||
                       processedBlocks |> Set.contains dataBlock
                    then blocks
                    else
                        Array.fill blocks emptyRange.Start length dataBlock
                        Array.fill blocks dataRange.Start length Empty
                        defrag blocks (Set.add dataBlock processedBlocks)

        defrag blocks Set.empty

// let test = "1313165" // Helpful custom test case discovered online.

let measureTime label f =
    let watch = Startwatch.Library.Watch()
    let result = f ()
    printfn $"""%s{label}: %d{result} ({watch.ElapsedFriendly})"""

measureTime "前" (fun _ ->
    input
    |> toBlocks
    |> Part1.defragSingle
    |> checksum) // 6519155389266

measureTime "後" (fun _ ->
    input
    |> toBlocks
    |> Part2.defragGroupwise
    |> checksum) // 6547228115826

let watch = Startwatch.Library.Watch
