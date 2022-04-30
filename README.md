# Fable.YALPS
This provides F# / Fable bindings for [**Yet Another Linear Programming Solver (YALPS)**](https://github.com/Ivordir/YALPS). Check it out for more information. The corresponding F# API translates naturally, but you may ignore anything regarding objects for fields on the model (see [Differences](#Differences) for more information). One thing to note is that equality between constraint and variable keys are tested using JS strict equality and not structural equality.

## Examples
```fsharp
open YALPS
open YALPS.Operators

let constraints =
  [ "wood", Constraint.lessEq 300.0
    "labor" <== 110.0 // you can use the convenience operators instead
    "storage" <== 400.0 ]

let variables =
  [ "table", [ "wood", 30.0; "labor", 5.0; "profit", 1200.0; "storage", 30.0 ]
    "dresser", [ "wood", 20.0; "labor", 10; "profit", 1600.0; "storage", 50.0 ] ]

let model = Model.createAllInteger Maximize "profit" constraints variables
let solution = Solver.solve model
// { status = Optimal
//   result = 14400.0
//   variables: [| "table", 8; "dresser", 3 |] }
```

With custom options:
```fsharp
let model = Model.createAllInteger Maximize "profit" constraints variables
let solution = model |> Solver.solveWith { solver.defaultOptions with timeout = 100.0 }
```

## Differences
To fit better with F# code, the ability for sequences to be substituted with objects (serving as a key-value map) was dropped. The underlying JS code still supports this, so dynamic casting will work:
```fsharp
let constraints =
  {| wood = Constraint.lessEq 300.0
     labor = {| max = 110.0 |}
     storage = {| max = 400.0 |} |}

let variables =
  {| table = {| wood = 30.0; labor = 5.0; profit = 1200.0; storage = 30.0 |}
     dresser = {| wood = 20.0; labor = 10.0; profit = 1600.0; storage = 50.0 |} |}

open Fable.Core.JsInterop
let model = Model.createAllInteger Maximize "profit" !!constraints !!variables
```

Here is the simplified model type used in Fable.YALPS (only sequences and no objects).
```fsharp
type Model<'VariableKey, 'ConstraintKey> =
  abstract direction: Direction
  abstract objective: 'ConstraintKey
  abstract constraints: ('ConstraintKey * Constraint) seq
  abstract variables: ('VariableKey * ('ConstraintKey * float) seq) seq
  abstract integers: U2<bool, 'VariableKey seq>
  abstract binaries: U2<bool, 'VariableKey seq>
```
