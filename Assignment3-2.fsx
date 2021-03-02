type aExp =
    | N of int
    | V of string
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)


type state = Map<string, int>

let rec arithEvalState (aExp: aExp) (s:state) =
    match aExp with
    | N n ->  n
    | V v when (s.ContainsKey(v)) -> s.Item(v)
    | V _ -> 0
    | Add(a, b) -> (+) (arithEvalState a s) (arithEvalState b s)
    | Sub(a, b) -> (-) (arithEvalState a s) (arithEvalState b s)
    | Mul(a, b) ->  (arithEvalState a s) * (arithEvalState b s)


// let a6 = V "x";;
// let a7 = N 4 .+. (V "y" .-. V "z");;

// arithEvalState a6 (Map.ofList [("x", 5)]);;
// arithEvalState a6 (Map.ofList [("y", 5)]);;
// arithEvalState a7 (Map.ofList [("x", 4); ("y", 5)]);;
// arithEvalState a7 (Map.ofList [("y", 4); ("z", 5)]);;