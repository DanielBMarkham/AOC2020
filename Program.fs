open System
open TestData
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

    let rec seatingLoop i sts oldTotal =
        let newI=i+1
        if i>Int32.MaxValue then failwith "i too big" else ()
        showSeats sts
        printfn ""
        let n=seatPeople(sts)
        let m=n|>Seq.cast<SeatType>|>Seq.filter(fun x->x=Occupied) |> Seq.length
        if oldTotal<>m
            then seatingLoop newI n m
            else oldTotal

    let equibSeatCount=seatingLoop 0 seats -1
    printfn "%A" equibSeatCount


    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;