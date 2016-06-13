module MatthewMcveighSolution

let private rnd = System.Random()
let flipCoin () = rnd.NextDouble() > 0.5

let frequencyOfHeads flipsPerCoin = 
    let rec countHeads numHeads i =
        if i < flipsPerCoin then
            let isHead = flipCoin ()
            countHeads (if isHead then numHeads + 1 else numHeads) (i + 1)
        else
            float numHeads

    countHeads 0 0 / float flipsPerCoin

let getFirstRandMinHeadsFraction numCoins flipsPerCoin = 
    let randomCoinI = rnd.Next numCoins

    let rec run first random min i =
        if i < numCoins then
            let frequency = frequencyOfHeads flipsPerCoin
            let first = if i = 0 then frequency else first
            let random = if i = randomCoinI then frequency else random
            let min = if min > frequency then frequency else min

            run first random min (i + 1)
        else
            (first, random, min)

    run 0.0 0.0 System.Double.MaxValue 0

let getResult iterations numCoins flipsPerCoin = 
    let getMinFromExperiment () =
        let (_, _, min) = getFirstRandMinHeadsFraction numCoins flipsPerCoin
        min

    let rec sumMinFromExperiments i sumOfMin =
        if i < iterations then
            sumMinFromExperiments (i + 1) (sumOfMin + getMinFromExperiment ())
        else
            sumOfMin

    let sum = sumMinFromExperiments 0 0.0
    sum / float iterations


