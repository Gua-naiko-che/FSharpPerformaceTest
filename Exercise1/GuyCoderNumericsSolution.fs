module GuyCoderNumericsSolution

open MathNet.Numerics.LinearAlgebra

let rnd = System.Random()
let flipCoin() = 
    let head = rnd.NextDouble() > 0.5
    if head then 1.0 else 0.0

let numberOfCoins = 1000
let numberOfFlips = 10
let numberOfExperiements = 100000
let numberOfValues = 3

let randomPick (limit : int) : int = rnd.Next(limit)   // [0 .. limit) it's a Python habit
let headCount (m : Matrix<float>) (coinIndex : int) : int = 
    System.Convert.ToInt32((m.Row coinIndex).Sum())

let minHeads (m : Matrix<float>) (numberOfCoins : int) (numberOfFlips : int) : int =
    let rec findMinHeads currentCoinIndex minHeadsCount minHeadsIndex =
        match currentCoinIndex,minHeadsCount with
        | -1,_ -> minHeadsCount
        | _,0 -> minHeadsCount  // Can't get less than zero so stop searching.
        | _ ->
            let currentMinHeadCount = (headCount m currentCoinIndex)
            let nextIndex = currentCoinIndex - 1
            if currentMinHeadCount < minHeadsCount 
            then findMinHeads nextIndex currentMinHeadCount currentCoinIndex
            else findMinHeads nextIndex minHeadsCount minHeadsIndex
    findMinHeads (numberOfCoins - 1) numberOfFlips -1

// Return the values for cOne, cRnd, and cMin as int values. 
// Will do division on final sum of experiments instead of after each experiment.
let runExperiement (numberOfCoins : int) (numberOfFlips : int) : (int * int * int) =        
    let (flips : Matrix<float>) = DenseMatrix.init numberOfCoins numberOfFlips (fun i j -> flipCoin())
    let cOne = headCount flips 0
    let cRnd = headCount flips (randomPick numberOfCoins)
    let cMin = minHeads flips numberOfCoins numberOfFlips
    (cOne,cRnd,cMin)

let runExperiements (numberOfExperiements : int) (numberOfCoins : int) (numberOfFlips : int) =
    let (cOneArray : int[]) = Array.create numberOfExperiements 0
    let (cRndArray : int[]) = Array.create numberOfExperiements 0
    let (cMinArray : int[]) = Array.create numberOfExperiements 0
    for i = 0 to (numberOfExperiements - 1) do
        let (cOne,cRnd,cMin) = runExperiement numberOfCoins numberOfFlips
        cOneArray.[i] <- cOne 
        cRndArray.[i] <- cRnd 
        cMinArray.[i] <- cMin 
    let (vMin : double) = (double)(Array.sum cMinArray) / (double)numberOfExperiements / (double)numberOfFlips
    vMin