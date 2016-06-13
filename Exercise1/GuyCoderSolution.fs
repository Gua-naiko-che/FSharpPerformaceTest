module GuyCoderSolution

let rnd = System.Random()
let randomPick (limit : int) : int = rnd.Next(limit)   // [0 .. limit) it's a Python habit

let numberOfCoins = 1000
let numberOfFlips = 10
let numberOfExperiements = 100000

let coinTest (numberOfFlips : int) : int =
    let rec countHeads (flips : int) bitIndex (headCount : int) : int =
        if bitIndex < 0 then headCount
        else countHeads (flips >>> 1) (bitIndex-1) (headCount + (flips &&& 0x01))
    countHeads (randomPick ((pown 2 numberOfFlips) - 1)) numberOfFlips 0

let runExperiement (numberOfCoins : int) (numberOfFlips : int) : (int * int * int) =
    let (randomCoin : int) = randomPick numberOfCoins
    let rec testCoin coinIndex (cFirst, cRnd, cMin, cFirstDone, cRanDone, cMinDone) : (int * int * int) =
        if (coinIndex < numberOfCoins) then
            if (not cFirstDone || not cRanDone || not cMinDone) then
                if (cFirstDone && cMinDone && (coinIndex <> randomCoin)) then
                     testCoin (coinIndex+1) (cFirst, cRnd, cMin, cFirstDone, cRanDone, cMinDone)
                else
                    let headsTotal = coinTest numberOfFlips 
                    let (cFirst, cRnd, cMin, cFirstDone, cRanDone, cMinDone) =
                        let cFirst = if coinIndex = 0 then headsTotal else cFirst
                        let cRnd = if coinIndex = randomCoin then headsTotal else cRnd
                        let cMin = if headsTotal < cMin then headsTotal else cMin
                        let cRanDone = if (coinIndex >= randomCoin) then true else cRanDone
                        let cMinDone = if (headsTotal = 0) then true else cMinDone
                        (cFirst, cRnd, cMin, true, cRanDone, cMinDone)
                    testCoin (coinIndex+1) (cFirst, cRnd, cMin, cFirstDone, cRanDone, cMinDone)
            else
                (cFirst, cRnd, cMin)
        else
            (cFirst, cRnd, cMin)
    testCoin 0 (-1,-1,10, false, false, false)

let runExperiements (numberOfExperiements : int) (numberOfCoins : int) ( numberOfFlips : int) =
    let rec accumateExperiements index aOne aRnd aMin : (int * int * int) =
        let (cOne,cRnd,cMin) = runExperiement numberOfCoins numberOfFlips
        if index > numberOfExperiements then (aOne, aRnd, aMin)
        else accumateExperiements (index + 1) (aOne + cOne) (aRnd + cRnd) (aMin + cMin)
    let (aOne, aRnd, aMin) = accumateExperiements 0 0 0 0
    let (vOne : double) = (double)(aOne) / (double)numberOfExperiements / (double)numberOfFlips
    let (vRnd : double) = (double)(aRnd) / (double)numberOfExperiements / (double)numberOfFlips
    let (vMin : double) = (double)(aMin) / (double)numberOfExperiements / (double)numberOfFlips
    vMin