module LangtonsAnt

type Direction = 
    | North
    | West
    | South
    | East

type Pos = {x : int; y : int}

type CellColor = White | Black

type State = { pos: Pos; dir: Direction; matrix: CellColor [,] }

let private getCellColor state = Array2D.get state.matrix state.pos.x state.pos.y

let private swapCellColor color =
    match color with
    | White -> Black
    | Black -> White

let private turn state =
    match (state.dir, (getCellColor state)) with
    | North, White -> {state with dir = East}
    | East, White -> {state with dir = South}
    | South, White -> {state with dir = West}
    | West, White -> {state with dir = North}
    
    | North, Black -> {state with dir = West}
    | East, Black -> {state with dir = North}
    | South, Black -> {state with dir = East}
    | West, Black -> {state with dir = South}

let private switch state =
    let currentCell = getCellColor state
    let newMatrix = Array2D.copy state.matrix
    Array2D.set newMatrix state.pos.x state.pos.y (swapCellColor currentCell)
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
