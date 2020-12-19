module CoolUtils
open System.Collections.Generic

let inline addTuple ((a,b):'a*'a) ((c,d):'a*'a)=(a+c,b+d)
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
type System.Random with
    /// Generates an infinite sequence of random numbers within the given range.
    member this.GetValues(minValue, maxValue) =
        Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))


type List<'t> with
  static member CrossProduct l1 l2=
    [ for el1 in l1 do
      for el2 in l2 do
        yield el1, el2 ]
type 'a ``[]`` with
    member x.RandomItem =
        let rnd = System.Random()
        let idx = rnd.Next(x.Length)
        x.[idx]
type 'a ``[,]`` with
  member self.Neighbors r c =
    [
    let rowLength:int=Array2D.length1 self - 1
    let colLength:int=Array2D.length2 self - 1
    let inline inbounds(r,c) = (r>(-1)) && (c>(-1)) && (r<=rowLength) && (c<=colLength)
    let neighborArr=[|(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)|]
    let validNeighborOffsets=neighborArr|>Array.filter(fun x->inbounds (addTuple (r,c) x) )
    let validNeighbors = validNeighborOffsets|>Array.map(fun ((a,b):int*int)->
      let r2=a+r
      let c2=b+c
      self.[r2,c2]
      )
    yield! validNeighbors]



  member self.JumpBy r c deltaRow deltaCol =
    [
      if not (deltaRow=0 && deltaCol=0) then
        let rowLength:int=Array2D.length1 self - 1
        let colLength:int=Array2D.length2 self - 1
        let inline inbounds(checkRow,checkCol) = (checkRow>(-1)) && (checkCol>(-1)) && (checkRow<=rowLength) && (checkCol<=colLength)
        let rec validJumps testRow testCol acc =
          let newRow=testRow+deltaRow
          let newCol=testCol+deltaCol
          if inbounds(newRow,newCol)
            then
              let newAcc=Array.append acc [|(newRow,newCol)|]
              if inbounds(newRow+deltaRow,newCol+deltaCol)
                then (validJumps newRow newCol newAcc)
                else newAcc
            else acc
        let validSpots=validJumps r c [||]
        let valids=validSpots|>Array.map(fun (a,b)->
          self.[a,b])
        yield! valids
    ]

let rec repeat items =
  seq { yield! items
        yield! repeat items }

let crossProduct l1 l2=
  [|  for el1 in l1 do
      for el2 in l2 do
        yield el1, el2 |]

let crossproduct l1 l2 =
  seq { for el1 in l1 do
          for el2 in l2 do
            yield el1, el2 };;


let crossproductNoSameExact l1 l2 =
  crossproduct l1 l2 |> Seq.filter(fun x->fst x<>snd x)
let crossproductNoSameAnyPairOrder l1 l2 =
  crossproduct l1 l2 |> Seq.filter(fun x->fst x<>snd x)|>Seq.map orderPair |> Seq.distinct


let doesRuleMatchPart1 (minCount:int, maxCount:int,character:char,stringToTest:string) =
    let matchCount=(stringToTest|>String.filter(fun x->x=character)).Length
    (matchCount>=minCount)&&(matchCount<=maxCount)
let doesRuleMatch (firstIndex:int, secondIndex:int,character:char,stringToTest:string) =
    try
        (stringToTest.[firstIndex-1]=character)<>(stringToTest.[secondIndex-1 ]=character)
    with |_->false


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

let flipArray(arr):'A[,]= Array2D.init (arr |> Array2D.length2) (arr |> Array2D.length1) (fun r c -> arr.[c,r])
let inline flatten (A:'a[,]) = A |> Seq.cast<'a>
let inline getColumn c (A:_[,]) = flatten A.[*,c..c] |> Seq.toArray
let inline getRow r (A:_[,]) = flatten A.[r..r,*] |> Seq.toArray


//let inline (+) ((a,b):'a*'a) ((c,d):'a*'a):('a*'a) = (a+b,c+d)
//let inline (+) ((a,b,c):'a*'a*'a) ((d,e,f):'a*'a*'a):('a*'a*'a) = (a+d,b+e,c+f)


let neighbors r c (A:'a[,]) =
    [
    let rowLength:int=Array2D.length1 A - 1
    let colLength:int=Array2D.length2 A - 1
    let inline inbounds(r,c) = (r>(-1)) && (c>(-1)) && (r<=rowLength) && (c<=colLength)
    let neighborArr=[|(-1,-1);(-1,0);(-1,1);(0,-1);(0,1);(1,-1);(1,0);(1,1)|]
    let validNeighborOffsets=neighborArr|>Array.filter(fun x->inbounds (addTuple (r,c) x) )
    let validNeighbors = validNeighborOffsets|>Array.map(fun ((a,b):int*int)->
      let r2=a+r
      let c2=b+c
      A.[r2,c2]
      )
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
            if inbounds(newRow+deltaRow,newCol+deltaCol)
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




type System.String with
  member x.ContainsAny (possibleMatches:string[]) =
    let ret = possibleMatches |> Array.tryFind(fun y->
        x.Contains(y)
        )
    ret.IsSome
  member x.ContainsAnyRegex(possibleRegexMatches:string[]) =
    let ret = possibleRegexMatches |> Array.tryFind(fun y->
        let rg = System.Text.RegularExpressions.Regex(y)
        rg.IsMatch(x)
        )
    ret.IsSome
  member x.ContainsRegex(regexMatch:string) =
    let rg = System.Text.RegularExpressions.Regex(regexMatch)
    rg.IsMatch(x)
  member x.ReplaceWithRegex (regexMatchString:string) (replacementString:string) =
    System.Text.RegularExpressions.Regex.Replace(x, regexMatchString, replacementString)
  member x.ReplaceAny (charactersToReplace:char []) (characterToUse:char) =
    let sb = System.Text.StringBuilder(x)
    let newString =
      charactersToReplace
      |> Array.fold(fun (acc:System.Text.StringBuilder) x->
        acc.Replace(x,characterToUse)
        ) sb
    newString.ToString()
  member x.CountOccurences (token:string) =
    let mts = x.Split([|token|], System.StringSplitOptions.None)
    if isNull mts then 0 else mts.Length
  member x.CountOccurencesRegex (regexMatchString:string) =
    let mts = System.Text.RegularExpressions.Regex.Matches(x, regexMatchString)
    if isNull mts then 0 else mts.Count
  member this.GetRight (iLen:int) =
    try
      this.Substring(this.Length - iLen, iLen)
    with |_ -> ""
  member this.GetLeft (iLen:int) =
    try
      this.Substring(0, iLen)
    with |_ -> ""
  member this.TrimLeft (iCount:int) =
    this.Substring(iCount, this.Length - iCount)
  member this.TrimRight (iCount:int) =
    this.Substring(0, this.Length - iCount)
  member this.TrimBoth (iLeft:int) (iRight:int) =
    if iLeft + iRight > this.Length
      then
        ""
      else
        (this.TrimLeft iLeft) |> (fun x-> x.TrimRight iRight)
  member this.TrimTo (desiredLength:int) =
    if this.Length <= desiredLength
      then
        this
      else
        this.GetLeft desiredLength
  /// adds the number of spaces to the beginning of the string
  member this.AddSpaces (numSpaces) =
    let prefix = System.String(' ', numSpaces)
    prefix+this
/// Centers text using spaces given a certain line length
  member this.PadBoth (len:int) =
    let leftPadCount = len/2 + this.Length/2
    this.PadLeft(leftPadCount).PadRight(len)
  member this.ToSafeFileName() =
    let temp=this.ToLower().ToCharArray() |> Array.map(fun x->
      let badChar=System.IO.Path.GetInvalidFileNameChars()|>Seq.exists(fun y->y=x)
      if badChar || x=' ' then '-' else x
    )
    System.String(temp)


type System.Text.StringBuilder with
    /// Write a line ending with the current OS newline character
    member x.wl (stringToWrite:string) =
        x.Append(stringToWrite + System.Environment.NewLine) |> ignore
    /// Write a line at a certain tab level ending with the current OS newline character
    member x.wt (level:int) (content:string) =
        let prefix = new System.String(' ', level*2)
        x.Append(prefix+content + System.Environment.NewLine) |> ignore
    /// Centers text across line using spaces on both sides. Default 80-character line can be overridden
    member x.wc (content:string) (?lineLength:int) =
        if lineLength.IsSome
            then
                x.Append((content.PadBoth lineLength.Value) + System.Environment.NewLine) |> ignore
            else
                x.Append((content.PadBoth 80) + System.Environment.NewLine) |> ignore
type System.IO.TextWriter with
    /// Shorter version of WriteLine
    member x.wl (stringToWrite:string) =
        x.WriteLine(stringToWrite)
    /// WriteLine at a certain tab level
    member x.wt (level:int) (content:string) =
        let prefix = new System.String(' ', level*2)
        x.WriteLine(prefix+content)
    /// Centers text across line using spaces on both sides. Default 80-character line can be overridden
    member x.wc (content:string) (?lineLength:int) =
        if lineLength.IsSome
            then
                x.WriteLine(content.PadBoth lineLength.Value)
            else
                x.WriteLine(content.PadBoth 80)

type System.Collections.Generic.Dictionary<'A, 'B> with
    member x.stringValueOrEmptyForKey n =
        if x.ContainsKey n then x.Item(n).ToString() else ""
    member x.TryFind n =
        let x,(y:'B) = x.TryGetValue n
        if x then Some y else None
//type Microsoft.FSharp.Collections.List<'T when 'T : equality> with
//    member this.IntersectionWithOtherList (b:List<'T> when 'T : equality) = this |> List.filter (fun x -> not (List.contains x b))
type System.Text.RegularExpressions.MatchCollection with
    member this.toSeq =
        seq {for i = 0 to this.Count - 1 do yield this.[i]}
    member this.toArray =
        [|for i = 0 to this.Count - 1 do yield this.[i] |]
type System.Text.RegularExpressions.Match with
    member this.lastGroup =
        this.Groups.[this.Groups.Count-1]
    member this.lastIndex =
        this.lastGroup.Index + this.lastGroup.Length

