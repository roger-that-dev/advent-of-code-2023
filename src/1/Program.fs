open System.IO
open System

let calibrationValue (x:string) = 
    x.ToCharArray() 
    |> Array.filter Char.IsDigit 
    |> fun x -> [| Array.head x; Array.last x |]
    |> String
    |> int

let partOne fileName =
    File.ReadAllLines(fileName)
    |> Array.map calibrationValue
    |> Array.reduce (+)

partOne "src/1/input.txt"
