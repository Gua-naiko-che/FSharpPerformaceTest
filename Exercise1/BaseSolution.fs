module BaseSolution

let rnd = System.Random()
let FlipCoin() = rnd.NextDouble() > 0.5
let FlipCoinNTimes N = List.init N (fun _ -> FlipCoin())
let FlipMCoinsNTimes M N = List.init M (fun _ -> FlipCoinNTimes N)

let ObtainFrequencyOfHeads tosses = 
    let heads = tosses |> List.filter (fun toss -> toss = true)
    float (List.length (heads)) / float (List.length (tosses))

let GetFirstRandMinHeadsFraction allCoinsLaunchs = 
    let first = ObtainFrequencyOfHeads(List.head (allCoinsLaunchs))
    let randomCoin = List.item (rnd.Next(List.length (allCoinsLaunchs))) allCoinsLaunchs
    let random = ObtainFrequencyOfHeads(randomCoin)

    let min = 
        allCoinsLaunchs
        |> List.map (fun coin -> ObtainFrequencyOfHeads coin)
        |> List.min
    (first, random, min)

let GetResult iterations numCoins flipsPerCoin =     
    Seq.init iterations (fun _ -> FlipMCoinsNTimes numCoins flipsPerCoin)
    |> Seq.map (fun oneExperiment -> GetFirstRandMinHeadsFraction oneExperiment)
    |> Seq.map (fun (first, random, min) -> min)
    |> Seq.average

