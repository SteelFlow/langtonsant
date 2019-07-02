module LangtonsAnt

type Direction = 
    | North
    | West
    | South
    | East

type Pos = {x : int; y : int}

type State = { pos: Pos; dir: Direction; matrix: bool [,] }

let cellValue state = Array2D.get state.matrix state.pos.x state.pos.y

let private turn state =
    match (state.dir, (cellValue state)) with
    | North, false -> {state with dir = East}
    | East, false -> {state with dir = South}
    | South, false -> {state with dir = West}
    | West, false -> {state with dir = North}
    
    | North, true -> {state with dir = West}
    | East, true -> {state with dir = North}
    | South, true -> {state with dir = East}
    | West, true -> {state with dir = South}

let private switch state =
    let currentCell = cellValue state
    let newMatrix = Array2D.copy state.matrix
    Array2D.set newMatrix state.pos.x state.pos.y (not currentCell)
    {state with matrix = newMatrix}

let private move state =
    match state.dir with
    | West -> {state with pos = {x = state.pos.x - 1; y = state.pos.y}}
    | North -> {state with pos = {x = state.pos.x; y = state.pos.y - 1}}
    | East -> {state with pos = {x = state.pos.x + 1; y = state.pos.y}}
    | South -> {state with pos = {x = state.pos.x; y = state.pos.y + 1}}

let next (state : State) =
    state
    |> turn
    |> switch
    |> move
