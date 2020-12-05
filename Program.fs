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
let processColumn(downIndex,column):list<int> =
    let ret=column|>List.mapi(fun i x->
        if i+1=downIndex then x+2 else x
        )
    ret
let movementSteps=seq{(1,1);(3,1);(5,1);(7,1);(1,2)}
let processAMove((mv:(int*int)),(iCount:ref<int>)) =
    let ic=(!iCount)
    columns
    |> Seq.skip (fst mv*ic)
    |>Seq.truncate (fst mv)
    |>Seq.mapi(fun i x->
        match (fst mv*(!iCount-1)+i) with
            |0->
                iCount:=(!iCount+1)
                x
            |_->
                let di=1+(snd mv*(!iCount-1))
                if i<>(fst mv-1) then x else processColumn(di,x)
        )
    |>Seq.toList

let printHill(samp:int [,] ) =
    //[0..(samp.[*,0]).Length-1]
    [0..10]//  samp|>Array2D.length2]
    |>List.iter(fun x->
        let sampleSack=Math.Min(10,samp|>Array2D.length2)
        samp.[0..sampleSack,x]
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

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day 2"

    let originalColumns=columns
    let totalMoves(mv:(int*int)) = seq{
        let index= ref 1
        let dog=fst mv
        while (snd mv*(!index))<(rowGenerators.Length)
            do
            index:=!index+1
            yield processAMove(mv,ref (!index-1))
    }
    let totals=
        movementSteps |> Seq.mapi(fun ii mv->
            let samp4=totalMoves(mv)|>Seq.concat
            let samp42D=array2D samp4

            let origArrayCols=originalColumns|>Seq.take samp42D.[0,*].Length
            let origHill=array2D origArrayCols

            printfn "%s." (string (ii+1))
            printHill(origHill)
            printfn ""
            printfn "         ---"
            printfn ""

            let fixedArray:int[,]=samp42D
            printHill(fixedArray)

            printfn ""
            printfn "------------------------------"
            let mop=samp42D |> Seq.cast<int>|>Seq.toArray
            printfn "Solution %s Tree Count %A" (mv.ToString()) (mop.Length)
            let bop=mop|>Array.filter(fun x->x=3)
            printfn "Rows: %s Cols: %s" (string (fixedArray |> Array2D.length1)) (string (fixedArray |> Array2D.length2))
            printfn "TOTAL TREES HIT %A" (bop.Length) // (boo |> Seq.length)
            printfn "------------------------------"
            printfn ""
            bop.Length
        ) |> Seq.toList
    let grandTotal = totals |> List.sum
    let grandMult = totals |> Seq.reduce(fun x y->x*y)
    printfn ""
    printfn "######################3"
    printfn "TOTAL TREES %A" grandTotal
    printfn "TOTAL PRODUCT OFTREES %A" grandMult
    printfn "######################3"
    printfn ""
    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;