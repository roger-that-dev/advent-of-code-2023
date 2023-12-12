open System.IO
open System.Text.RegularExpressions

let toIntSet (s:string) = 
    Regex.Split(s, "[\s]+") 
    |> Array.map System.Int32.TryParse
    |> Array.map snd
    |> Set

let card (line:string) = 
    let numberLists = (line.Split(": ") |> Array.last).Split(" | ") 
    let winningNumbers = Array.head numberLists |> toIntSet
    let ourNumbers = Array.last numberLists |> toIntSet
    Set.intersect winningNumbers ourNumbers |> Set.toList

let partOne =
    File.ReadAllLines("src/4/input.txt") 
    |> Array.map card
    |> Array.map (fun x -> pown 2 (List.length x-1))
    |> Array.sum

partOne