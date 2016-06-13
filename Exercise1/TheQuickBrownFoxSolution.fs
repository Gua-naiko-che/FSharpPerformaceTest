module TheQuickBrownFoxSolution

let rnd = System.Random()
let FlipCoin() = rnd.NextDouble() > 0.5
let FlipCoinNTimes N = Array.init N (fun _ -> FlipCoin())
let FlipMCoinsNTimes M N = Array.init M (fun _ -> FlipCoinNTimes N)

let ObtainFrequencyOfHeads tosses = 
    let heads = tosses |> Array.fold (fun state t -> if t then state + 1 else state) 0
    float heads / float (Array.length (tosses))

let GetFirstRandMinHeadsFraction allCoinsLaunchs = 
    let first = ObtainFrequencyOfHeads(Array.head (allCoinsLaunchs))
    let randomCoin = Array.item (rnd.Next(Array.length (allCoinsLaunchs))) allCoinsLaunchs
    let random = ObtainFrequencyOfHeads(randomCoin)

    let min = 
        allCoinsLaunchs
        |> Array.map (fun coin -> ObtainFrequencyOfHeads coin)
        |> Array.min
    (first, random, min)

let GetResult iterations numCoins flipsPerCoin =     
    Seq.init iterations (fun _ -> FlipMCoinsNTimes numCoins flipsPerCoin)
    |> Seq.map (fun oneExperiment -> GetFirstRandMinHeadsFraction oneExperiment)
    |> Seq.map (fun (first, random, min) -> min)
    |> Seq.average

