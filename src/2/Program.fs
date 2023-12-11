open System.IO

let possible = function
    | ("red", x) when x <= 12 -> true
    | ("blue", x) when x <= 14 -> true
    | ("green", x) when x <= 13 -> true
    | _ -> false

let selection (s : string) =
    let splits = s.Split(" ")
    (Array.last splits, Array.head splits |> int) |> possible

let hand s = s |> Array.map selection |> Array.reduce (&&)

let sets (game : string) =
    game.Split("; ")
    |> Array.map (fun (set : string) -> set.Split(", ") |> hand)
    |> Array.reduce (&&)

let game (x : string) =
    let splits = x.Split(": ")
    let index (s : string) = s.Split(' ') |> Array.last |> int 
    Array.head splits |> index, Array.last splits |> sets 

let partOne fileName =
    File.ReadAllLines(fileName)
    |> Array.map game
    |> Array.filter (fun (_,possible) -> possible = true)
    |> Array.sumBy fst

partOne "src/2/input.txt"

let p2selection (s : string) =
    let splits = s.Split(" ")
    (Array.last splits, Array.head splits |> int)

let setsp2 (game : string) =
    game.Split("; ")
    |> Array.map (fun (set : string) -> set.Split(", ") |> Array.map p2selection)
    |> Array.collect id
    |> Array.groupBy fst
    |> Array.map (fun x -> x |> snd |> Array.maxBy snd |> snd)
    |> Array.reduce (*)

let partTwo fileName =
    File.ReadAllLines(fileName)
    |> Array.map (fun x -> x.Split(": ") |> Array.last |> setsp2) 
    |> Array.sum

partTwo "src/2/input.txt"
