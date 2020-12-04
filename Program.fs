open System
open TestData
open Microsoft.FSharp.Collections

let flipArray(arr):'A[,]= Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])
let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray
///
/// Many thanks to Tomas Petricek
///
let rec repeat items =
  seq { yield! items
        yield! repeat items }

let stopWatch = System.Diagnostics.Stopwatch()
stopWatch.Start()
//
// DAY 3 ??
//

let translateLine (line:string):int[] =
    line.ToCharArray()|>Array.map(fun x->if x='.' then 0 else 1)
let translateGenerators (dat:string list) =
    dat|>List.map (translateLine)
let rowGenerators =
    let t=part1Data |> translateGenerators
    t|>List.map repeat


let columns=Seq.initInfinite(fun i->
    //rowGeneratorArray|>Seq.map(fun x->
    rowGenerators|>Seq.map(fun x->
        x|>Seq.skip i|>Seq.head
        )|>Seq.toList
    )
let originalColumns=columns
let processColumn(downIndex,column) =
    let ret=column|>List.mapi(fun i x->
        if i=downIndex+1 then x+2 else x
        )
    ret
let nextMove=(3,1)//seq{while true do yield (1,1)}
let processAMove(cols,iCount) =
    cols
    |> Seq.skip (fst nextMove*iCount)
    |>Seq.truncate (fst nextMove)
    |>Seq.mapi(fun i x->
        let di=(snd nextMove*iCount)
        if i<>(fst nextMove-1) then x else processColumn(di,x)
        )
    |>Seq.toList

let printHill(samp:int [,] ) =
    //[0..(samp.[*,0]).Length-1]
    [1..samp|>Array2D.length2]
    |>List.iteri(fun i x->
        samp.[*,i]
        |> Array.iter(fun y->
            let c=
                match y with
                    |0->"."
                    |1->"#"
                    |2->"O"
                    |3->"X"
            printf "%s " c)
        printfn ""
        )
let foo:list<int>=
    let thisCol=columns|>Seq.head
    processColumn((-1),thisCol)
let bar:list<list<int>>=[foo]
let bell:seq<list<list<int>>>=seq{bar}

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day 2"

    
    let mutable index=(-1)
    let totalMoves = seq{
        //yield processAMove(columns, 1);
        //yield bar;
        yield [foo]
        let newcol=(columns|>Seq.skip 1)
        while (snd nextMove*index)<(rowGenerators.Length)
            do
            index<-index+1
            yield processAMove(newcol,index)
    }

    let samp4=totalMoves|>Seq.concat
    //let samp4arr=array2D samp4
    let samp42D=array2D samp4
    //let samp42D=flipArray(samp4arr)
    let origArrayCols=originalColumns|>Seq.take samp42D.[0,*].Length
    let origHill=array2D origArrayCols
    let boo(samp:int [,]) =
        [0..(samp.[*,0]).Length-1]
        |>List.mapi(fun i x->
            origHill.[*,1]
        )
    printHill(origHill)
    printfn ""
    printfn ""
    printfn ""
    printHill(flipArray (flipArray samp42D))
    printfn ""
    printfn ""
    printfn ""
    let mop=samp42D |> Seq.cast<int>|>Seq.toArray
    printfn "asdf %A" (mop.Length)
    let bop=mop|>Array.filter(fun x->x=3)
    printfn "TOTAL TREES HIT %A" (bop.Length) // (boo |> Seq.length)
    printfn ""
    printfn ""

    printfn ""

    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;