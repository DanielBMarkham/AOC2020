open System
open TestData
open Microsoft.FSharp.Collections

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 3 ??
//

let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray
let rec repeat items =
  seq { yield! items
        yield! repeat items }

let rowGeneratorArray =[|
    repeat [1;0;0;1]
    ;repeat [0;1;0]
    ;repeat [1;0]
    ;repeat [1;0;0;1;1;0;0;1]
    |]
let columns=Seq.initInfinite(fun i->
    rowGeneratorArray|>Seq.map(fun x->
        x|>Seq.skip i|>Seq.head
        )|>Seq.toList
    )
let processColumn(downIndex,column) =
    let ret=column|>List.mapi(fun i x->
        if i=downIndex+1 then x+2 else x
        )
    ret
let nextMove=(2,1)//seq{while true do yield (1,1)}
let processAMove(cols,iCount) =
    cols
    |> Seq.skip (fst nextMove*iCount)
    |>Seq.truncate (fst nextMove)
    |>Seq.mapi(fun i x->
        let di=(snd nextMove*iCount)
        if i<>(fst nextMove-1) then x else processColumn(di,x)
        )
    |>Seq.toList

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day 2"

    let flipArray(arr) = Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])

    let n1=processAMove(columns,0)
    let n2=processAMove(columns,1)
    let n3=processAMove(columns,2)
    let n4=processAMove(columns,2)
    let n5=processAMove(columns,2)

    let samp1=columns|>Seq.truncate (fst nextMove*5)
    let samp12D=flipArray(array2D samp1)
    let samp2=[n1;n2;n3;n4;n5] |> List.concat
    let samp22D=flipArray(array2D samp2)

    let mutable index=(-1)
    let totalMoves = seq{
        while (snd nextMove*index)<(rowGeneratorArray.Length)
            do
            index<-index+1
            yield processAMove(columns,index)
    }

    let samp4=totalMoves|>Seq.concat
    let samp42D=flipArray(array2D samp4)

    printfn "%A" samp12D
    printfn ""
    printfn ""
    printfn ""
    printfn "%A" samp22D
    printfn ""
    printfn ""
    printfn ""
    printfn "%A" samp42D

    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;