module CoolUtils
open System.Collections.Generic

let doesRuleMatchPart1 (minCount:int, maxCount:int,character:char,stringToTest:string) =
    let matchCount=(stringToTest|>String.filter(fun x->x=character)).Length
    (matchCount>=minCount)&&(matchCount<=maxCount)
let doesRuleMatch (firstIndex:int, secondIndex:int,character:char,stringToTest:string) =
    try
        (stringToTest.[firstIndex-1]=character)<>(stringToTest.[secondIndex-1 ]=character)
    with |_->false


let inline orderPair(x,y)=if x>y then (x,y) else (y,x)
let inline orderTriplet(x,y,z)=
    let (a,b)=orderPair(x,y)
    let (c,d)=orderPair(a,z)
    let (i,j)=orderPair(d,b)
    (c,i,j)
let inline tripletToHash(a,b,c)=
    string a.GetHashCode
    + string b.GetHashCode
    + string c.GetHashCode


let allContiguousSegments (f:array<'a>->unit) (incomingSequence:array<'a>)  =
  incomingSequence |> Array.iteri(fun i x->
    let upToI= max 0 (i-1)
    let numbersUpToI=incomingSequence.[0..upToI]
    numbersUpToI|>Array.iteri(fun j k->
      let upToJ= max 0 (j-1)
      let numbersUpToJ=numbersUpToI.[0..upToJ]
      numbersUpToJ|>Array.iteri(fun k z->
        let lft=max 0 (k-1)
        f numbersUpToI.[k..j]
        )
      )
    )
  ()
let crossproduct l1 l2 =
  seq { for el1 in l1 do
          for el2 in l2 do
            yield el1, el2 };;
let crossproductNoSameExact l1 l2 =
  crossproduct l1 l2 |> Seq.filter(fun x->fst x<>snd x)
let crossproductNoSameAnyPairOrder l1 l2 =
  crossproduct l1 l2 |> Seq.filter(fun x->fst x<>snd x)|>Seq.map orderPair |> Seq.distinct

let flipArray(arr):'A[,]= Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])
let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray

let rec repeat items =
  seq { yield! items
        yield! repeat items }


let inline addTuple ((a,b):'a*'a) ((c,d):'a*'a)=(a+c,b+d)

let neighbors r c (A:'a[,]) =
    [
    let rowLength:int=Array2D.length1 A - 1
    let colLength:int=Array2D.length2 A - 1
    let inline inbounds(r,c) = (r>(-1)) && (c>(-1)) && (r<=rowLength) && (c<=colLength)
    let neighborArr=[|(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)|]
    let validNeighborOffsets=neighborArr|>Array.filter(fun x->inbounds (addTuple (r,c) x) )
    let validNeighbors = validNeighborOffsets|>Array.map(fun (a,b)->
      let r2=a+r
      let c2=b+c
      A.[r2,c2])
    yield! validNeighbors]

let jumpBy r c deltaRow deltaCol (A:'a[,]) =
    [
    if not (deltaRow=0 && deltaCol=0) then
      let rowLength:int=Array2D.length1 A - 1
      let colLength:int=Array2D.length2 A - 1
      let inline inbounds(checkRow,checkCol) = (checkRow>(-1)) && (checkCol>(-1)) && (checkRow<=rowLength) && (checkCol<=colLength)
      let rec validJumps testRow testCol acc =
        let newRow=testRow+deltaRow
        let newCol=testCol+deltaCol
        if inbounds(newRow,newCol)
          then
            let newAcc=Array.append acc [|(newRow,newCol)|]
            if inbounds(newRow+deltaRow,newCol+deltaCol)=true
              then (validJumps newRow newCol newAcc)
              else newAcc
          else acc
      let validSpots=validJumps r c [||]
      let valids=validSpots|>Array.map(fun (a,b)->
        A.[a,b])
      yield! valids
      ]


/// Takes while predicate is false. Takes first item where predicate is true
let takeUntil predicate (s:seq<_>) =
  /// Iterates over the enumerator, yielding elements and
  /// stops after an element for which the predicate does not hold
  let rec loop (en:IEnumerator<_>) = seq {
    if en.MoveNext() then
      // Always yield the current, stop if predicate does not hold
      yield en.Current
      if predicate en.Current then
        yield! loop en }

  // Get enumerator of the sequence and yield all results
  // (making sure that the enumerator gets disposed)
  seq { use en = s.GetEnumerator()
        yield! loop en }

