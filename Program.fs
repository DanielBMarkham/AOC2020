open System
open TestData
open Microsoft.FSharp.Collections



let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 3 :38.03
//


[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day "

    // let printIfSumMatches (lis:array<'a>)=
    //   if lis|>Array.sum=magicNumber
    //     then printfn "%A" lis
    //     else ()
    seats|>seatPeople|>seatPeople
    //let foo= m|>allContiguousSegments printIfSumMatches
    //printM numArr
//    ans m


    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;