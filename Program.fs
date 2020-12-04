open System
open TestData
open Microsoft.FSharp.Collections

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 2 TIMING 0.51 seconds
//

let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray 
let rec repeat items =
  seq { yield! items
        yield! repeat items }

let rowPattern1 = [|4;1;3|]
let rowPattern2 = [|9;5;3|]
let rowPAttern3 = [|8;3;0|]

let row1Gen = repeat [1;0;0;1]
let row2Gen = (repeat [0;1;0])
let row3Gen = (repeat [1;0])
let row4Gen = (repeat [1;0;0;1;1;0;0;1])

let gr = Seq.initInfinite(fun x->
    let r1 =
        row1Gen
        |> Seq.chunkBySize 64
        |> Seq.skip x
        |> Seq.head

    let r2 =
        row2Gen
        |> Seq.chunkBySize 64
        |> Seq.skip x
        |> Seq.head

    let r3 =
        row3Gen
        |> Seq.chunkBySize 64
        |> Seq.skip x
        |> Seq.head

    let r4 =
        row4Gen
        |> Seq.chunkBySize 64
        |> Seq.skip x
        |> Seq.head
    [r1;r2;r3;r4]
    )

let infiniteGrids=seq {yield! gr}


[<EntryPoint>]
let main argv =
    //printfn "Advent of Code Puzzle Output"
    printfn "Day 2"
    let foo=infiniteGrids|>Seq.take 3

    foo |> Seq.iter(fun x->
        let bar=array2D x
        printfn ""
        printfn "%A" bar
        printfn ""
        printfn ""
        )
    printfn ""
    printfn "%A" 0//(grid()).[*,1]

    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;