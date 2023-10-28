open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> Array.toSeq

type Elf = {
  From: int
  To: int
}

type ElfPair = {
  One: Elf
  Two: Elf
}

let parseElf (part:string) =
  let [| fromSection; toSection |] = part.Split("-") |> Array.map int
  { From = fromSection; To = toSection }

let parseElfPair (line:string) =
  let [| onestr; twostr |] = line.Split(",")  
  { One = (parseElf onestr ); Two = (parseElf twostr)}

let processPair (pair:ElfPair) =
  match pair with
  | p when p.One.From >= p.Two.From && p.One.From <= p.Two.To -> 1
  | p when p.One.To >= p.Two.From && p.One.To <= p.Two.To -> 1
  | p when p.Two.From >= p.One.From && p.Two.From <= p.One.To -> 1
  | p when p.Two.To >= p.One.From && p.Two.To <= p.One.To -> 1
  | _ -> 0

input
|> Seq.map parseElfPair
|> Seq.map processPair
|> Seq.sum
|> printfn "%d"
