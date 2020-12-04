module CoolUtils

//
// Day 2

//
let doesRuleMatchPart1 (minCount:int, maxCount:int,character:char,stringToTest:string) =
    let matchCount=(stringToTest|>String.filter(fun x->x=character)).Length
    (matchCount>=minCount)&&(matchCount<=maxCount)
let doesRuleMatch (firstIndex:int, secondIndex:int,character:char,stringToTest:string) =
    try
        (stringToTest.[firstIndex-1]=character)<>(stringToTest.[secondIndex-1 ]=character)
    with |_->false
// let validPasswords=(Seq.filter doesRuleMatch firstTestData)|>Seq.toList
//let numberThatPass=validPasswords|>Seq.length

//
// Day 1
//
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

//let dataInTriples = (Seq.allPairs (Seq.allPairs newData newData) newData)  |> Seq.map(fun ((x,y),z)->(x,y,z))
//
// 
//
//let crossSumIs2020 = dataInTriples |> Seq.filter(fun (x,y,z)->x+y+z=2020)
//let expensesThatWork = 
//    crossSumIs2020 
//    |> Seq.map orderTriplets
//    |> Seq.distinctBy tripleTupleHash