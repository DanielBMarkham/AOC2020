module CoolUtils


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
///
/// Many thanks to Tomas Petricek posting code on SO
///
let rec repeat items =
  seq { yield! items
        yield! repeat items }


let inline addTuple ((a,b):'a*'a) ((c,d):'a*'a)=(a+c,b+d)