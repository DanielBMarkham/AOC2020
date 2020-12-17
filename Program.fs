open System
open TestData
open CoolUtils
open Microsoft.FSharp.Collections

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 11
//

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day 11"



    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime

    0