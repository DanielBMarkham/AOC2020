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
  [0..((str |>Array2D.length1)-1)]
  |> List.iter(fun x->
    let row:SeatType array=str.[x,0..]
    row
    |>Array.iter(fun (y:SeatType)->
        let ltr=(typeToLetter y)
        printf "%s" ltr
        ()
      )
    printfn ""
    )
let adjacentSeats x y (seatArray:SeatType[,]) =
  let getSeat(x,y)=
    let l1=(seatArray |> Array2D.length1)
    let l2=(seatArray |> Array2D.length2)
    if x<0 || y<0 || x>l1-1 || y>l2-1 then None else Some seatArray.[x,y]

  let topLeft=getSeat(x-1,y-1)
  let topCenter=getSeat(x,y-1)
  let topRight=getSeat(x+1,y-1)
  let lft=getSeat(x-1,y)
  let rgt=getSeat(x+1,y)
  let bottomLeft=getSeat(x-1,y+1)
  let bottomCenter=getSeat(x,y+1)
  let bottomRight=getSeat(x+1,y+1)
  [|
    topLeft;topCenter;topRight;lft;rgt;bottomLeft;bottomCenter;bottomRight
  |] |> Array.choose id


let seats=

  stringSeats
  |>Array2D.map (fun x->
    match x.Substring(0,1) with
      |"L"->Empty
      |"."->Floor
      |"#"->Occupied
      |_->Floor
    )
let seatPeople(sts:SeatType[,])=
  let ret=
    sts
    |>Array2D.mapi(fun x y thisSeat->
      let seatsAroundMe = adjacentSeats x y sts
      let totalRowCount=seatsAroundMe|>Seq.filter(fun z->z=Occupied)|>Seq.length
      let newSeat=
        match sts.[x,y] with
          |Empty when totalRowCount=0->Occupied
          |Occupied when totalRowCount>3->Empty
          |_->sts.[x,y]
      newSeat
    )
  ret
