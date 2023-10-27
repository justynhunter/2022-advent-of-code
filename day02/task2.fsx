open System
open System.IO

let input = File.ReadAllLines "./input.txt" |> Array.toList

type Outcome =
| Win = 6
| Draw = 3
| Loss = 0

type Shape =
| Rock = 1
| Paper = 2
| Scissors = 3

type Round = { Them: Shape; Me: Shape }

let convertToShape shape =
  match shape with
  | "A" -> Some Shape.Rock
  | "B" -> Some Shape.Paper
  | "C" -> Some Shape.Scissors
  | "X" -> Some Shape.Rock
  | "Y" -> Some Shape.Paper
  | "Z" -> Some Shape.Scissors
  | _ -> None

let scoreMatch (round: Round) =
  let outcome =
    match (round.Them, round.Me) with
    | (Shape.Scissors, Shape.Rock)
    | (Shape.Rock, Shape.Paper)
    | (Shape.Paper, Shape.Scissors) -> Outcome.Win
    | (t, m) when t = m -> Outcome.Draw
    | _ -> Outcome.Loss

  (int outcome) + (int round.Me)

let parseLine (line:string) =
  let [| them; me |] = line.Split(" ")

  let themShape = 
    match them with
    | "A" -> Some Shape.Rock
    | "B" -> Some Shape.Paper
    | "C" -> Some Shape.Scissors
    | _ -> None

  let meShape = 
    match (themShape, me) with
    | (t, "X") -> 
      match t with 
      | Some Shape.Rock -> Some Shape.Scissors
      | Some Shape.Paper -> Some Shape.Rock
      | Some Shape.Scissors -> Some Shape.Paper
      | _ -> None
    | (t, "Y") -> themShape
    | (t, "Z") -> 
      match t with 
      | Some Shape.Rock -> Some Shape.Paper
      | Some Shape.Paper -> Some Shape.Scissors
      | Some Shape.Scissors -> Some Shape.Rock
      | _ -> None
    | _ -> None
  
  match (themShape, meShape) with 
  | (Some t, Some m) -> Some {Them = t; Me = m}
  | _ -> None


input
|> List.map parseLine
|> List.choose id
|> List.map scoreMatch
|> List.sum
|> printfn "answer: %d"

