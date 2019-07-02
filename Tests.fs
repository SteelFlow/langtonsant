module LangtonsAntTests

open Expecto
open LangtonsAnt

let trueMatrix = (Array2D.create 3 3 true)
let falseMatrix = (Array2D.create 3 3 false)
let init = { pos = {x = 1; y = 1}; dir = West; matrix = falseMatrix }

[<Tests>]
let directionTests =
    testList "Direction tests" [
        testList "Call next on false cell - should turn 90° right" [
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

        testList "Call next on true cell - should turn 90° left" [
            test "West -> South" {
                let state = {init with dir = West; matrix = trueMatrix} 
                Expect.equal (next state).dir South "Direction should be South"
            }

            test "South -> East" {
                let state = {init with dir = South; matrix = trueMatrix} 
                Expect.equal (next state).dir East "Direction should be East"
            }

            test "East -> North" {
                let state = {init with dir = East; matrix = trueMatrix} 
                let result = next state
                Expect.equal result.dir North "Direction should be North"
            }

            test "North -> West" {
                let state = {init with dir = North; matrix = trueMatrix} 
                let result = next state
                Expect.equal result.dir West "Direction should be West"
            }    
        ]
    ]

[<Tests>]
let movementTests = 
    testList "Movement tests" [
        testList "Call next on false cell - should move one to the right" [
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

        testList "Call next on true cell - should turn 90° left" [
            test "West -> South" {
                let state = {init with dir = West; matrix = trueMatrix} 
                Expect.equal (next state).pos { x=1; y=2 } "Should move South"
            }

            test "South -> East" {
                let state = {init with dir = South; matrix = trueMatrix} 
                Expect.equal (next state).pos { x=2; y=1 } "Should move East"
            }

            test "East -> North" {
                let state = {init with dir = East; matrix = trueMatrix} 
                Expect.equal (next state).pos { x=1; y=0 } "Should move North"
            }

            test "North -> West" {
                let state = {init with dir = North; matrix = trueMatrix} 
                Expect.equal (next state).pos { x=0; y=1 } "Should move West"
            }    
        ]
    ]

[<Tests>]
let colorSwitchTests =
    testList "Color switch tests" [
        test "Call next on false cell - should set cell to true" {
            let state = init
            let nextState = next state
            Expect.equal (Array2D.get nextState.matrix 1 1) true "Previous state position should be true" 
        }

        test "Call next on true cell - should set cell to false" {
            let state = {init with matrix = trueMatrix}
            let nextState = next state
            Expect.equal (Array2D.get nextState.matrix 1 1) false "Previous state position should be false" 
        }
    ]
