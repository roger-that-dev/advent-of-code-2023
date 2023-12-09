open System.IO
open System.Text.RegularExpressions

type Number = { Number: int; Line: int; Start: int; End: int}
type Symbol = { Symbol: string; Line: int; Index: int}

let findAll lines (regex : Regex) deserialiser = 
    let parseLine lineNum line = regex.Matches(line) |> Seq.map (deserialiser lineNum)
    lines |> Array.mapi parseLine |> Seq.collect id

let partOne fileName =
    let lines = File.ReadAllLines(fileName)
    let numbersRegex = Regex("[0-9]+")
    let symbolsRegex = Regex("[^.\d]")
    let parseNumber lineNum (m : Match) =
        { Number = int(m.Value); 
          Line = lineNum; 
          Start = m.Index;
          End = m.Index + m.Length - 1 }
    let parseSymbol lineNum (m : Match) = 
        { Symbol = m.Value; 
          Line = lineNum; 
          Index = m.Index }
    let numbers = findAll lines numbersRegex parseNumber
    let symbols = findAll lines symbolsRegex parseSymbol
    Seq.toList numbers, Seq.toList symbols

partOne "src/3/example.txt"