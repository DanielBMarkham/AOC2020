open System
open TestData
open Microsoft.FSharp.Collections

let flipArray(arr):'A[,]= Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])
let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray
///
/// Many thanks to Tomas Petricek posting code on SO
///
let rec repeat items =
  seq { yield! items
        yield! repeat items }

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 3 :38.03
//


[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day "

    let printIfSumMatches (lis:array<'a>)=
      if lis|>Array.sum=magicNumber
        then printfn "%A" lis
        else ()

    let foo= m|>allContiguousSegments printIfSumMatches
    //printM numArr
//    ans m


    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;