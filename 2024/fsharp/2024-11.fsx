open System

#r "nuget: CodeConscious.Startwatch, 1.0.0"
open System.Collections.Generic

let input =
    IO.File.ReadAllText("input/2024/11.txt").TrimEnd().Split(' ')
    |> List.ofArray
    |> List.map int64

let digitCount (n: int64) =
    if n < 0L then failwith "Input must be a non-negative number"
    else
        let rec countDigits value count =
            if value < 10L then count
            else countDigits (value / 10L) (count + 1)
        countDigits n 1

let isEven i = i % 2 = 0

let memo = Dictionary<int64, int64 list>(10000000)

let checkStone (stone: int64) =
    if stone < 0 then failwith $"A negative value was passed in. This is bad."

    if memo.ContainsKey stone
    then
        memo.GetValueOrDefault stone
    else
        let result =
            match stone with
            | 0L -> [1L]
            | s when isEven(digitCount s) ->
                let halfway = digitCount s / 2
                let l, r = Math.DivRem(s, pown 10 halfway)
                [l; r]
            | s -> [s * 2024L]
        memo.Add(stone, result)
        result

// let checkStone (stone: int64) =
//     if stone < 0 then failwith $"A negative value was passed in. This is bad."

//     if memo.ContainsKey stone
//     then
//         memo.GetValueOrDefault stone
//     else
//         let result =
//             match stone with
//             | 0L -> List [1L]
//             | s when isEven(digitCount s) ->
//                 let halfway = digitCount s / 2
//                 let l, r = Math.DivRem(s, pown 10 halfway)
//                 List [l; r]
//             | s -> List [s * 2024L]
//         memo.Add(stone, result)
//         result

// let combinedCollect times list =
//     printfn $"Iterations: " // TODO: Delete after debugging.
//     List.fold (fun acc i ->
//         printf $"#{i} " // TODO: Delete after debugging.
//         List.collect checkStone acc) list [1..times]

let combinedCollect times list =
    let mutable current = list
    printfn $"Iterations: " // TODO: Delete after debugging.

    for i in 1 .. times do
        printf $"#{i} " // TODO: Delete after debugging.
        // Create a mutable builder to accumulate the next result.
        let builder = System.Collections.Generic.List<_>()
        for item in current do
            builder.AddRange(checkStone item)
        // Convert the builder (a sequence) back to an F# list.
        current <- List.ofSeq builder
    current

let measureTime label f =
    let watch = Startwatch.Library.Watch()
    let result = f ()
    printfn $"""%s{label}: %d{result} ({watch.ElapsedFriendly})"""

measureTime "前" (fun _ -> input |> combinedCollect 25 |> _.Length)
measureTime "後" (fun _ -> input |> combinedCollect 75 |> _.Length)
