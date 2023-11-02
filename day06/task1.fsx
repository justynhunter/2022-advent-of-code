open System.IO

"./input.txt"
|> File.ReadAllText
|> Seq.toList
|> List.windowed 4
|> List.mapi (fun i  window -> (i + 4, set window))
|> List.find (fun (_, window) -> window |> Set.count = 4)
|> fst
|> printfn "%d"
