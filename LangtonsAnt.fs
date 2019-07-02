module LangtonsAnt

type Direction = 
    | North
    | West
    | South
    | East

type Pos = {x : int; y : int}

type State = { pos: Pos; dir: Direction; matrix: bool [,] }

let next (state : State) =
    let pos = state.pos
    let cell = Array2D.get state.matrix pos.x pos.y
    let (nextPost, nextDir) =
        match (state.dir, cell) with
        | North, false -> ({pos with x = pos.x + 1}, East)
        | East, false -> ({pos with y = pos.y + 1}, South)
        | South, false -> ({pos with x = pos.x - 1}, West)
        | West, false -> ({pos with y = pos.y - 1}, North)
        
        | North, true -> ({pos with x = pos.x - 1}, West)
        | East, true -> ({pos with y = pos.y - 1}, North)
        | South, true -> ({pos with x = pos.x + 1}, East)
        | West, true -> ({pos with y = pos.y + 1}, South)
    
    let newMatrix = Array2D.copy state.matrix
    Array2D.set newMatrix pos.x pos.y (not cell) // Tror denne er mutable

    { pos = nextPost; dir = nextDir; matrix = newMatrix }
