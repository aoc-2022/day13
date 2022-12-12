open System.IO

let input = File.ReadAllLines "/tmp/aoc/input.t" |> Array.toList

input |> List.map (printfn "%A")