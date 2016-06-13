module Util

let sw = System.Diagnostics.Stopwatch()
let getExecutionTime f =
    sw.Restart()
    let result = f()
    sw.Stop()
    let time = sw.Elapsed
    (result, time)

let runAndPrintResults (description, f, baseTime:System.TimeSpan) =
    let result, time = getExecutionTime f
    let improvement = float(baseTime.Ticks) / float(time.Ticks)
    sprintf "%s - result: %f, time elapsed: %A, improvement: %.2f x" description result time improvement