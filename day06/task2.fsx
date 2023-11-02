open System.IO

"./input.txt"
|> File.ReadAllText
|> Seq.toList
|> List.windowed 14
|> List.mapi (fun i  window -> (i + 14, set window))
|> List.find (fun (_, window) -> window |> Set.count = 14)
|> fst
|> printfn "%d"
