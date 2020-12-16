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

    let runTest inData targetOut=
        let outData=seatPeople(inData)
        let testResult=outData=targetOut
        printfn " %A" testResult
        let discrepencyGrid:string[,] =
            outData
            |>Array2D.mapi(fun r c ele->
                let cll=
                    if ele=targetOut.[r,c]
                        then "Y"
                        else "N"
                cll
                )
        printfn "Target cell success or failures"
        printfn "%A" discrepencyGrid
        printfn ""
        let disagreements =
            let badMatches =
                discrepencyGrid
                |>Array2D.mapi(fun r c ele->
                    if ele="Y" then None else Some (r,c)
                )
            let seqBM=
                badMatches |> Seq.cast<option<int*int>>
            seqBM |> Seq.choose id |> Seq.toArray
        let firstDisagreement = disagreements.[0]
        printfn "first error found at %A " firstDisagreement
        let firstBadCellrow=fst firstDisagreement
        let firstBadCellcol=snd firstDisagreement
        printfn "The cell starts %A" inData.[firstBadCellrow,firstBadCellcol]
        printfn "The cell ends up %A when it is supposed to be %A"  outData.[firstBadCellrow,firstBadCellcol] targetOut.[firstBadCellrow,firstBadCellcol]
        let v=(cellsFromHereToTheBorderOrFirstSeat firstBadCellrow firstBadCellcol inData)
        printfn "cells from here to the end"
        printfn "%A" v
        let t=List.concat v
        let peopleICanSeeCount=t|>List.filter(fun x->x=Occupied) |> List.length
        printfn "people or empty chair count %A" peopleICanSeeCount
        let cellsAroundMe=neighbors firstBadCellrow firstBadCellcol inData
        printfn "Cells around me"
        printfn "%A" cellsAroundMe
        let peopleDirectlyAdjacentToMe =
            neighbors firstBadCellrow firstBadCellcol inData
            |> Seq.toArray
            |> Array.filter(fun x->x=Occupied)
        let peopleAdjacentToMeCount = peopleDirectlyAdjacentToMe.Length
        printfn "Occupied seats adjacent to me count %A" peopleAdjacentToMeCount



    let rec seatingLoop i sts oldTotal =
        let newI=i+1
        if i>Int32.MaxValue then failwith "i too big" else ()
        let n=seatPeople(sts)
        printfn "%A" i
        //showSeats n
        let m=n|>Seq.cast<SeatType>|>Seq.filter(fun x->x=Occupied) |> Seq.length
        //printfn "%A" oldTotal
        if oldTotal<>m
            then seatingLoop newI n m
            else m
    let equibSeatCount=seatingLoop 1 seats -1
    printfn "%A" equibSeatCount

    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime

    0