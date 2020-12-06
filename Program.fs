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




let FrameSize=part1Data.Length
let initArray x y = 
  let temp=part1Data.[y].Substring(x,1)
  temp
let inline addTuples (a,b) (c,d)=(a+c,b+d)
let mountainArray=Array2D.init part1Data.[0].Length part1Data.Length initArray
let inline move (previousPosition:int*int) (move:int*int) (mountainArr:string [,]):int*int=
  
  let newPos=addTuples previousPosition move
  let cl=
    match fst newPos >= part1Data.[0].Length,snd newPos >= part1Data.Length with
      |false,true-> "OVERSHOOT Y"
      |true,false-> "OVERSHOOT X"
      |true,true-> "OVERSHOOT BOTH"
      |_,_->mountainArr.[fst newPos,snd newPos]
  //printfn "Old Pos: %A Move: %A New Pos:%A Cell: %A" previousPosition move newPos cl
  newPos
let inline checkForTree(position:int*int,(mountainArr:string [,])):int =
    //let mountainLine = mountainArr.[*,snd position]
    //printfn "Check position %A Row being checked: %A" position mountainLine
    if mountainArr.[fst position,snd position]="#" 
      then
        //printfn "Tree found at %A" position
        1
      else 0
let initialPosition=(0,0)
let rec skiMoves (mv:int*int) (position:int*int) (mountainArr:string[,]) (treeCount:int)=
  let newPosition=move position mv mountainArr
  match newPosition with
    |(x,y) when y>=FrameSize->
      printfn "Tree Count: %A" treeCount
      treeCount
    |(x,y) when x>=part1Data.[0].Length->
      let newMountain=mountainArray
      let newX=x-part1Data.[0].Length
      let newPos=(newX,y)
      skiMoves mv newPos newMountain (treeCount+checkForTree(newPos,newMountain))
    |(_,_)->
      skiMoves mv newPosition mountainArr (treeCount+checkForTree(newPosition,mountainArr))

[<EntryPoint>]
let main argv =
    printfn "Advent of Code Puzzle Output"
    printfn "Day 2"
    let movementSteps=seq{(1,1);(3,1);(5,1);(7,1);(1,2)}
    let movementTotals = movementSteps|>Seq.map(fun x->skiMoves x (0,0) mountainArray 0) |> Seq.toList
    printfn "%A" movementTotals
    let ans = movementTotals |> List.reduce(fun x y->x*y)
    printfn "Answer %A" ans
    let m2 = movementTotals |> List.map(fun x->bigint x)
    let a2 =
      m2
      |> List.fold(fun acc x->
        x*acc
        ) (bigint 1)
    printfn "%A " a2
    let ts=stopWatch.Elapsed
    let elapsedTime =
        String.Format("{0:00}:{1:00}:{2:00}.{3:00}", ts.Hours, ts.Minutes, ts.Seconds, ts.Milliseconds / 10);
    printfn "Puzzle completed in %s seconds" elapsedTime
    0;