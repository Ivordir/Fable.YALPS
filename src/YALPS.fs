namespace YALPS

open Fable.Core

/// Specifies the bounds for the total of a value.
type Constraint =
  /// The total should be equal to this number.
  /// In the case that `min` or `max` are also defined,
  /// this is used instead.
  abstract equal: float option

  /// The total should be greater paket than or equal to this number.
  /// Can be specified alongside `max`.
  abstract min: float option

  /// The total should be less than or equal to this number.
  /// Can be specified alongside `min`.
  abstract max: float option

/// Contains functions for creating `Constraint`s.
module Constraint =
  /// Returns a `Constraint` that specifies something should be less than or equal to `value`.
  /// Equivalent to `!!{| max = value |}`.
  let [<Import("lessEq", "yalps")>] lessEq (value: float): Constraint = jsNative

  /// Returns a `Constraint` that specifies something should be greater than or equal to `value`.
  /// Equivalent to `!!{| min = value |}`.
  let [<Import("greaterEq", "yalps")>] greaterEq (value: float): Constraint = jsNative

  /// Returns a `Constraint` that specifies something should be exactly equal to `value`.
  /// Equivalent to `!!{| equal = value |}`.
  let [<Import("equalTo", "yalps")>] equalTo (value: float): Constraint = jsNative

  /// Returns a `Constraint` that specifies something should be between `lower` and `upper` (inclusive).
  /// Equivalent to `!!{| min = lower; max = upper |}`.
  let [<Import("inRange", "yalps")>] inRange (lower: float, upper: float): Constraint = jsNative

/// Contains convenience operators for creating `Constraint`s.
module Operators =
  open Constraint

  /// `constraint <== value` is equivalent to `(constraint, lessEq value)`.
  let inline (<==) ``constraint`` value = ``constraint``, lessEq value

  /// `constraint >== value` is equivalent to `(constraint, greaterEq value)`.
  let inline (>==) ``constraint`` value = ``constraint``, greaterEq value

  /// `constraint === value` is equivalent to `(constraint, equalTo value)`.
  let inline (===) ``constraint`` value = ``constraint``, equalTo value

/// Indicates whether to `Maximize` or `Minimize` the objective.
type [<StringEnum>] Direction =
  | Maximize
  | Minimize

/// The model representing a LP problem.
/// Note that equality between constraint and variable keys are tested using
/// JS strict equality (===) and not structural equality.
type Model<'VariableKey, 'ConstraintKey> =
  /// Indicates whether to `Maximize` or `Minimize` the objective.
  abstract direction: Direction

  /// <summary>
  /// The key of the value to maximize or minimize.
  /// </summary>
  /// <example>
  /// Note that constraints can be placed upon the objective itself. Maximize up to a certain point:
  /// ```
  /// {| direction = Maximize
  ///    objective = "obj"
  ///    constraints = [ "obj", lessEq 100; ... ]
  ///    variables = [ ... ] |}
  /// ```
  /// </example>
  abstract objective: 'ConstraintKey

  /// <summary>
  /// The constraints of the LP problem.
  /// Duplicate keys are not ignored.
  /// Rather, the bounds of the duplicate constraints
  /// are merged to become the most restrictive.
  /// </summary>
  /// <seealso cref="Constraint"/>
  /// <seealso cref="Operators"/>
  /// <example>
  /// ```
  /// {| constraints = [ "a", lessEq 7; "b", equalTo 22; "c" &lt;== 5 ]; ... |}
  /// ```
  /// </example>
  abstract constraints: ('ConstraintKey * Constraint) seq

  /// <summary>
  /// The variables of the LP problem.
  /// The inner `seq` represents the coefficients of the variable for some (sub)set of constraints.
  /// For these coefficients, the last entry is used in the case of duplicate `'ConstraintKey`s.
  /// Duplicate `'VariableKey`s, however, are not ignored.
  /// The order of variables is preserved in the solution,
  /// but variables that end up with a value of `0` are excluded from the solution.
  /// </summary>
  /// <example>
  /// ```
  /// {| variables =
  ///      [ "x", [ "a", 2; "b", 11 ]
  ///        "y", [ "a", 3; "c", 22 ] ]
  ///    ... |}
  /// ```
  /// </example>
  abstract variables: ('VariableKey * ('ConstraintKey * float) seq) seq

  /// A `seq` of variable keys that should be treated as integer.
  /// `integers` can instead be a `boolean`, indicating whether all variables are integer or not.
  abstract integers: U2<bool, 'VariableKey seq>

  /// An `seq` of variable keys that should be treated as binary
  /// (can only be 0 or 1 in the solution).
  /// `binaries` can also be a `boolean`, indicating whether all variables are binary or not.
  abstract binaries: U2<bool, 'VariableKey seq>

/// The model representing a LP problem.
/// Note that equality between constraint and variable keys are tested using
/// JS strict equality (===) and not structural equality.
type Model = Model<string, string>

/// Contains functions to create `Model`s.
module Model =
  /// Creates a model with no integer or binary variables.
  let create
    (direction: Direction)
    (objective: 'ConstraintKey)
    (constraints: ('ConstraintKey * Constraint) seq)
    (variables: ('VariableKey * ('ConstraintKey * float) seq) seq) =
    {| direction = direction
       objective = objective
       constraints = constraints
       variables = variables |}
    |> JsInterop.(!!) : Model<'VariableKey, 'ConstraintKey>

  /// Creates model with all variables indicated as integer.
  let createAllInteger
    (direction: Direction)
    (objective: 'ConstraintKey)
    (constraints: ('ConstraintKey * Constraint) seq)
    (variables: ('VariableKey * ('ConstraintKey * float) seq) seq) =
    {| direction = direction
       objective = objective
       constraints = constraints
       variables = variables
       integers = true |}
    |> JsInterop.(!!) : Model<'VariableKey, 'ConstraintKey>

  /// Creates a model with all variables indicated as binary.
  let createAllBinary
    (direction: Direction)
    (objective: 'ConstraintKey)
    (constraints: ('ConstraintKey * Constraint) seq)
    (variables: ('VariableKey * ('ConstraintKey * float) seq) seq) =
    {| direction = direction
       objective = objective
       constraints = constraints
       variables = variables
       binaries = true |}
    |> JsInterop.(!!) : Model<'VariableKey, 'ConstraintKey>

  /// Creates a model, marking the provided `integers` and `binaries` variables
  /// as integer and binary respectively.
  let createInteger
    (direction: Direction)
    (objective: 'ConstraintKey)
    (constraints: ('ConstraintKey * Constraint) seq)
    (variables: ('VariableKey * ('ConstraintKey * float) seq) seq)
    (integers: 'VariableKey seq)
    (binaries: 'VariableKey seq) =
    {| direction = direction
       objective = objective
       constraints = constraints
       variables = variables
       integers = integers
       binaries = binaries |}
    |> JsInterop.(!!) : Model<'VariableKey, 'ConstraintKey>

/// <summary>This indicates what type of solution, if any, the solver was able to find.</summary>
/// <seealso cref="Solution"/>
type [<StringEnum>] SolutionStatus =
  | Optimal
  | Infeasible
  | Unbounded
  | Timedout
  | Cycled

/// The solution returned by the solver.
/// It includes the status of the solution,
/// the final objective result (if any),
/// and the variable amounts.
type Solution<'VariableKey> =
  /// `status` indicates what type of solution, if any, the solver was able to find.
  ///
  /// `Optimal` indicates everything went ok, and the solver found an optimal solution.
  ///
  /// `Infeasible` indicates that the problem has no possible solutions.
  /// `result` will be `NaN` in this case.
  ///
  /// `Unbounded` indicates a variable, or combination of variables, are not sufficiently constrained.
  /// As such, the `result` of the solution will be +-infinity.
  /// `variables` in the solution might contain a variable,
  /// in which case it is the variable that the solver happened to finish on.
  /// This may be the unbounded variable or one of the combination of variables that are unbounded.
  ///
  /// `Timedout` indicates that the solver exited early for an integer problem.
  /// This may happen if the solver takes too long and exceeds the `timeout` option.
  /// Similarly, the number of branch and cut iterations may exceeed `maxIterations` as set in the options.
  /// In both of these cases, the current sub-optimal solution/result, if any, is returned.
  /// If `result` is `NaN`, then this means no integer solutions were found before the solver timed out.
  ///
  /// `Cycled` indicates that the simplex method cycled and exited.
  /// This case is rare, but `checkCycles` can be set to `true` in the options to check for it.
  /// Otherwise, if `maxPivots` (as set in the options) is reached by the simplex method,
  /// then it is assumed that a cycle was encountered.
  /// `result` will be `NaN` in this case.
  abstract status: SolutionStatus

  /// The final, maximized or minimized value of the objective.
  /// It may be `NaN` in the case that `status` is `Infeasible`, `Cycled`, or `Timedout`.
  /// It may also be +-infinity in the case that `status` is `Unbounded`.
  abstract result: float

  /// An array of variables and their coefficients that add up to `result`
  /// while satisfying the constraints of the problem.
  /// Variables with a coefficient of `0` are not included in this.
  /// In the case that `status` is `Unbounded`,
  /// `variables` may contain one variable which is (one of) the unbounded variable(s).
  abstract variables: ('VariableKey * float) array

/// The solution returned by the solver.
/// It includes the status of the solution,
/// the final objective result (if any),
/// and the variable amounts.
type Solution = Solution<string>

// Options is declared as a record to allow record-update syntax.

/// The options for the solver.
type Options = {
  /// Numbers equal to or less than the provided precision are treated as zero.
  /// Similarly, the precision determines whether a number is sufficiently integer.
  /// The default value is `1E-8`.
  precision: float

  /// In rare cases, the solver can cycle.
  /// This is usually the case when the number of pivots exceeds `maxPivots`.
  /// Alternatively, setting this to `true` will cause
  /// the solver to explicitly check for cycles.
  /// (The solution will have the `Cycled` status if a cycle is detected.)
  /// The default value is `false`.
  checkCycles: bool

  /// This determines the maximum number of pivots allowed within the simplex method.
  /// If this is exceeded, then it assumed that the simplex method cycled,
  /// and the returned solution will have the `Cycled` status.
  /// If your problem is *very* large, you *may* have to set this option higher.
  /// The default value is `4096`.
  maxPivots: int

  /// This setting applies to integer problems only.
  /// If an integer solution is found within `(1 +- tolerance) *
  /// {the problem's non-integer solution}`,
  /// then this approximate integer solution is returned.
  /// This is helpful for large integer problems where
  /// the most optimal solution becomes harder to find,
  /// but other solutions that are relatively close to
  /// the optimal one may be much easier to find.
  /// The default value is `0` (only find the most optimal solution).
  tolerance: float

  /// This setting applies to integer problems only.
  /// It specifies, in milliseconds, the maximum amount of time
  /// the main branch and cut portion of the solver may take before timing out.
  /// In the case the solver does time out,
  /// the returned solution will have the `Timedout` status.
  /// The current sub-optimal solution, if any, is returned as well in this case.
  /// The default value is infinity (no timeout).
  timeout: float

  /// This setting applies to integer problems only.
  /// It determines the maximum number of iterations
  /// for the main branch and cut algorithm.
  /// It can be used alongside or instead of `timeout`
  /// to prevent the algorithm from taking too long.
  /// The default value is `4096`.
  maxIterations: int
}

/// Contains the main solve function(s) and the default options.
module Solver =
  // Convert regular JS object into a record.
  let [<Import("defaultOptions", "yalps")>] private importedOptions: Options = jsNative

  /// <summary>
  /// The default options used by the solver.
  /// You can use record-update syntax to easily change one or more of the options.
  /// </summary>
  /// <example>
  /// ```
  /// solveWith { defaultOptions with timeout = 100.0 }
  /// ```
  /// </example>
  let defaultOptions = { importedOptions with precision = importedOptions.precision } // trigger object copy into record

  let [<Import("solve", "yalps")>] private importedSolve (model: Model<'VarKey, 'ConKey>, options: Options option): Solution<'VarKey> = jsNative

  /// <summary>Runs the solver on the given model using the default options.</summary>
  /// <seealso cref="Model"/>
  /// <seealso cref="solveWith"/>
  /// <seealso cref="Solution"/>
  let solve model = importedSolve (model, None)

  /// <summary>Runs the solver on the given model and using the given options.</summary>
  /// <example>
  /// ```
  /// solveWith { defaultOptions with timeout = 100.0 }
  /// ```
  /// </example>
  /// <seealso cref="Model"/>
  /// <seealso cref="Options"/>
  /// <seealso cref="Solution"/>
  let solveWith options model = importedSolve (model, Some options)
