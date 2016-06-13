// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Main

open System
open Util

[<EntryPoint>]
let main argv = 
    let iterations, numCoins, numFlips = 100000, 1000, 10

    let _, baseTime = getExecutionTime (fun _ -> BaseSolution.GetResult iterations numCoins numFlips)

    let tests = [("Base", fun _ -> BaseSolution.GetResult iterations numCoins numFlips);
                 ("Matthew Mcveigh", fun _ -> MatthewMcveighSolution.getResult iterations numCoins numFlips)
                 ("Fyodor Soikin", fun _ -> FyodorSoikinSolution.GetResult iterations numCoins numFlips)
                 ("GuyCoder", fun _ -> GuyCoderSolution.runExperiements iterations numCoins numFlips)
                 ("GuyCoder MathNet Numerics", fun _ -> GuyCoderNumericsSolution.runExperiements iterations numCoins numFlips)
                 ("TheQuickBrownFox", fun _ -> TheQuickBrownFoxSolution.GetResult iterations numCoins numFlips)]

    let results = 
        tests
        |> Seq.map (fun (description, f) -> runAndPrintResults (description, f, baseTime))
        |> Seq.fold (fun r s -> r + s + System.Environment.NewLine) ""

    let resultsPath = System.IO.Path.Combine(System.Environment.CurrentDirectory, "results.txt")
    System.IO.File.WriteAllText(resultsPath, results)

    printfn "%s" results

    let a = Console.ReadKey()

    0 // return an integer exit code
