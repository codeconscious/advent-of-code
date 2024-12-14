// https://adventofcode.com/2024/day/3

open System
open System.Text.RegularExpressions

let testInput1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let testInput2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let input = System.IO.File.ReadAllText("input/2024/03.txt").TrimEnd()

Regex.Matches(input, "mul\((\d{1,3}),(\d{1,3})\)")
|> Seq.map (fun m -> Int32.Parse m.Groups[1].Value *
                     Int32.Parse m.Groups[2].Value)
|> Seq.sum
|> printfn "%d" // 167090022

type Instruction = Disable | Enable | Pair of int * int
type Memo = { Enabled: bool; Total: int }

let parseGroup (groups: GroupCollection) =
    match groups with
    | g when g[0].Value = "don't()" -> Disable
    | g when g[0].Value = "do()" -> Enable
    | g -> Pair (Int32.Parse g[1].Value, Int32.Parse g[2].Value)

let folder acc = function
    | Disable -> { acc with Enabled = false }
    | Enable -> { acc with Enabled = true }
    | Pair (l, r) ->
        if acc.Enabled
        then { acc with Total = acc.Total + l * r }
        else acc

Regex.Matches(input, "don't\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\)")
|> Seq.map _.Groups
|> Seq.map parseGroup
|> Seq.fold folder { Enabled = true; Total = 0 }
|> _.Total
|> printfn "%A" // 89823704


