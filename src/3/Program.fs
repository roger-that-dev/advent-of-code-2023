open System.IO
open System.Text.RegularExpressions

type Number = { Number: int; Line: int; Start: int; End: int}
type Symbol = { Symbol: string; Line: int; Index: int}

let isAdjacent (number:Number) (symbol:Symbol) = 
    let lineRange = Set.ofList [number.Line - 1 .. number.Line + 1]
    let indexRange = Set.ofList [number.Start - 1 .. number.End + 1]
    lineRange.Contains symbol.Line && indexRange.Contains symbol.Index

let isAdjToSymbol symbols number = 
    symbols |> Seq.exists (isAdjacent number)

let isGear numbers symbol = 
    let gearNums = numbers |> Seq.filter (fun x -> isAdjacent x symbol) |> Seq.map (fun x -> x.Number)
    if Seq.length gearNums = 2 then Seq.reduce (*) gearNums else 0

let findAll lines (regex : Regex) deserialiser = 
    let parseLine lineNum line = regex.Matches(line) |> Seq.map (deserialiser lineNum)
    lines |> Array.mapi parseLine |> Seq.collect id

let setup fileName symbolsRegex=
    let lines = File.ReadAllLines(fileName)
    let numbersRegex = Regex("[0-9]+")
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
    numbers, symbols

let partOne =
    let symbolsRegex = Regex("[^.\d]")
    let numbers, symbols = setup "src/3/input.txt" symbolsRegex
    numbers 
    |> Seq.filter (isAdjToSymbol symbols) 
    |> Seq.sumBy (fun x -> x.Number)

let partTwo =
    let symbolsRegex = Regex("\*")
    let numbers, symbols = setup "src/3/input.txt" symbolsRegex
    symbols
    |> Seq.map (isGear numbers)
    |> Seq.sum