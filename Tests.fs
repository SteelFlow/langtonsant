module LangtonsAntTests

open Expecto
open LangtonsAnt

let blackMatrix = (Array2D.create 3 3 Black)
let whiteMatrix = (Array2D.create 3 3 White)
let init = { pos = {x = 1; y = 1}; dir = West; matrix = whiteMatrix }

[<Tests>]
let directionTests =
    testList "Direction tests" [
        testList "Call next on White cell - should turn 90° right" [
            test "West -> North" {
                let state = {init with dir = West} 
                Expect.equal (next state).dir North "Direction should be North"
            }

            test "North -> East" {
                let state = {init with dir = North}
                Expect.equal (next state).dir East "Direction should be East"
            }

            test "East -> South" {
                let state = {init with dir = East}
                Expect.equal (next state).dir South "Direction should be South"
            }

            test "South -> West" {
                let state = {init with dir = South}
                Expect.equal (next state).dir West "Direction should be West"
            }    
        ]

        testList "Call next on Black cell - should turn 90° left" [
            test "West -> South" {
                let state = {init with dir = West; matrix = blackMatrix} 
                Expect.equal (next state).dir South "Direction should be South"
            }

            test "South -> East" {
                let state = {init with dir = South; matrix = blackMatrix} 
                Expect.equal (next state).dir East "Direction should be East"
            }

            test "East -> North" {
                let state = {init with dir = East; matrix = blackMatrix} 
                let result = next state
                Expect.equal result.dir North "Direction should be North"
            }

            test "North -> West" {
                let state = {init with dir = North; matrix = blackMatrix} 
                let result = next state
                Expect.equal result.dir West "Direction should be West"
            }    
        ]
    ]

[<Tests>]
let movementTests = 
    testList "Movement tests" [
        testList "Call next on White cell - should move one to the right" [
            test "West -> Move North" {
                let state = {init with dir = West} 
                Expect.equal (next state).pos { x=1; y=0 } "Sould move north"
            }

            test "North -> Move East" {
                let state = {init with dir = North}
                Expect.equal (next state).pos { x=2; y=1 } "Should move East"
            }

            test "East -> Move South" {
                let state = {init with dir = East}
                Expect.equal (next state).pos { x=1; y=2 } "Should move South"
            }

            test "South -> Move West" {
                let state = {init with dir = South}
                Expect.equal (next state).pos { x=0; y=1 } "Should move West"
            }    
        ]

        testList "Call next on Black cell - should turn 90° left" [
            test "West -> South" {
                let state = {init with dir = West; matrix = blackMatrix} 
                Expect.equal (next state).pos { x=1; y=2 } "Should move South"
            }

            test "South -> East" {
                let state = {init with dir = South; matrix = blackMatrix} 
                Expect.equal (next state).pos { x=2; y=1 } "Should move East"
            }

            test "East -> North" {
                let state = {init with dir = East; matrix = blackMatrix} 
                Expect.equal (next state).pos { x=1; y=0 } "Should move North"
            }

            test "North -> West" {
                let state = {init with dir = North; matrix = blackMatrix} 
                Expect.equal (next state).pos { x=0; y=1 } "Should move West"
            }    
        ]
    ]

[<Tests>]
let colorSwitchTests =
    testList "Color switch tests" [
        test "Call next on White cell - should set color to Black" {
            let state = init
            let nextState = next state
            Expect.equal (Array2D.get nextState.matrix 1 1) Black "Previous state position should be Black" 
        }

        test "Call next on Black cell - should set color to White" {
            let state = {init with matrix = blackMatrix}
            let nextState = next state
            Expect.equal (Array2D.get nextState.matrix 1 1) White "Previous state position should be White" 
        }
    ]
