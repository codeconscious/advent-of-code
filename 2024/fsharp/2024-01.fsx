// https://adventofcode.com/2024/day/1

let input = System.IO.File.ReadAllLines("input/2024/01.txt")

let prepareInput (input: string array) : int array array =
    input
    |> Array.map (fun l -> [| int l[..4]; int l[8..12] |])

let puzzle1 (input: int array array) =
    input
    |> Array.transpose
    |> Array.map Array.sort
    |> Array.transpose
    |> Array.sumBy (fun a -> abs (a[0] - a[1]))

input
|> prepareInput
|> puzzle1
|> printfn "%d" // 2166959

let puzzle2 (data: int array array) : int =
    let left = data[0]
    let right = data[1]

    let countOccurrences (target: int) (searchArr: int array) : int =
        searchArr
        |> Array.filter (fun i -> i = target)
        |> Array.length
        |> (*) target

    left |> Array.sumBy (fun i -> countOccurrences i right)

input
|> prepareInput
|> Array.transpose
|> puzzle2
|> printfn "%d" // 23741109
