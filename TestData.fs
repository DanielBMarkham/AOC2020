module TestData
open System
open System.Numerics
open CoolUtils

let stringSeats=array2D [|
  [|"L";".";"L";"L";".";"L";"L";".";"L";"L"|]
  ;[|"L";"L";"L";"L";"L";"L";"L";".";"L";"L"|]
  ;[|"L";".";"L";".";"L";".";".";"L";".";"."|]
  ;[|"L";"L";"L";"L";".";"L";"L";".";"L";"L"|]
  ;[|"L";".";"L";"L";".";"L";"L";".";"L";"L"|]
  ;[|"L";".";"L";"L";"L";"L";"L";".";"L";"L"|]
  ;[|".";".";"L";".";"L";".";".";".";".";"."|]
  ;[|"L";"L";"L";"L";"L";"L";"L";"L";"L";"L"|]
  ;[|"L";".";"L";"L";"L";"L";"L";"L";".";"L"|]
  ;[|"L";".";"L";"L";"L";"L";"L";".";"L";"L"|]
|]
type SeatType =
  |Empty
  |Occupied
  |Floor
let typeToLetter (s:SeatType)=
  match s
    with
    |Empty->"L"
    |Occupied->"#"
    |Floor->"."
let showSeats (str:SeatType[,])=
  [0..str |>Array2D.length1]
  |> List.iteri(fun i x->
    let row:SeatType array=getRow i str
    row
    |>Array.iter(fun (y:SeatType)->
        let ltr=(typeToLetter y)
        printf "%s" ltr
        ()
      )
    printfn ""
    )
let seats=

  stringSeats
  |>Array2D.map (fun x->
    match x.Substring(0,1) with
      |"L"->Empty
      |"."->Floor
      |"#"->Occupied
      |_->Floor
    )
let seatPeople (sts:SeatType[,])=
  let ret=
    sts
    |>Array2D.mapi(fun x y thisSeat->
      let peopleToMyLeft=sts.[..x,y]|>Seq.filter(fun z->z=Occupied)|>Seq.length
      let peopleToMyRight=sts.[x..,y]|>Seq.filter(fun z->z=Occupied)|>Seq.length
      let totalRowCount=peopleToMyLeft+peopleToMyRight
      printfn "  -  %A" totalRowCount
      let newSeat=
        match sts.[x,y] with
          |Empty when totalRowCount=0->Occupied
          |Occupied when totalRowCount>3->Empty
          |_->sts.[x,y]
      newSeat
    )
  ret
