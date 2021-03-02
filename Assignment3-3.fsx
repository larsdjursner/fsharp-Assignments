type word = (char * int) list

type squareFun = word -> int -> int -> int

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

type state = Map<string, int>

let hello : word =
    [
        'H', 4
        'E', 1
        'L', 1
        'L', 1
        'O', 1
    ]


let rec arithEval (a: aExp) (w:word) (s:state) = 
    match a with
    | N n -> n
    | V v when s.ContainsKey(v) -> s.Item(v)
    | V _ -> 0
    | WL -> w.Length
    | PV pv -> snd(w.[arithEval pv w s])
    | Add(a, b) -> (+) (arithEval a w s) (arithEval b w s)
    | Sub(a, b) -> (-) (arithEval a w s) (arithEval b w s)
    | Mul(a, b) ->  (arithEval a w s) * (arithEval b w s)

// arithEval WL [] Map.empty
// arithEval WL hello Map.empty
// arithEval (PV (N 0)) hello Map.empty
// arithEval (PV (N 1)) hello Map.empty
// arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_",0)])
// arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_",42)])
// arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_",0)])
// arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_",42)])
// arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_",0)])


type cExp =
   | C  of char      (* Character value *)
   | ToUpper of cExp (* Converts lower case to upper case character, non-characters unchanged *)
   | ToLower of cExp (* Converts upper case to lower case character, non characters unchanged *)
   | CV of aExp      (* Character lookup at word index *)

let rec charEval (c: cExp) (w: word) (s: state) =
    match c with 
    | C c -> c
    | ToUpper c -> System.Char.ToUpper(charEval c w s )
    | ToLower c -> System.Char.ToLower(charEval c w s)
    | CV c -> fst(w.[arithEval c w s])

// charEval (C 'H') [] Map.empty
// charEval (ToLower (CV (N 0))) hello Map.empty
// charEval (ToUpper (C 'h')) [] Map.empty
// charEval (ToLower (C '*')) [] Map.empty
// charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)])



type bExp =             
   | TT                   (* true *)
   | FF                   (* false *)

   | AEq of aExp * aExp   (* numeric equality *)
   | ALt of aExp * aExp   (* numeric less than *)

   | Not of bExp          (* boolean not *)
   | Conj of bExp * bExp  (* boolean conjunction *)

   | IsLetter of cExp     (* check for letter *)
   | IsDigit  of cExp     (* check for constant *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
   
let (.=.) a b = AEq (a, b)   
let (.<.) a b = ALt (a, b)   
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)


let rec boolEval (b: bExp) (w: word) (s:state) = 
    match b with 
    | b when b = TT -> true
    | b when b = FF -> false
    | AEq (a,b) -> (arithEval a w s) = (arithEval b w s)
    | ALt (a,b) -> (arithEval a w s) < (arithEval b w s)
    | Not b -> not (boolEval b w s)
    | Conj (a,b) -> (boolEval a w s) && (boolEval b w s)
    | IsLetter b -> System.Char.IsLetter(charEval b w s)
    | IsDigit b -> System.Char.IsDigit(charEval b w s)


// boolEval TT [] Map.empty;;
// boolEval FF [] Map.empty;;
// boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]);
// boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x"))[] (Map.ofList [("x", 5); ("y", 7)]);;
// boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)]);;
// boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);;
// boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)]);;
// boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]);;


type stmnt =
   | Skip                        (* does nothing *)
   | Ass of string * aExp        (* variable assignment *)
   | Seq of stmnt * stmnt        (* sequential composition *)
   | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
   | While of bExp * stmnt       (* while statement *)

let rec evalStmnt (stm: stmnt) (w: word) (s: state) = 
    match stm with
    | Skip -> s
    | Ass (x, a) -> s.Add(x, (arithEval a w s))
    | Seq (stm1, stm2) -> (evalStmnt stm1 w s) |> (evalStmnt stm2 w)
    | ITE (g, stm1, stm2) when (boolEval g w s) -> (evalStmnt stm1 w s) 
    | ITE (_, stm1, stm2) -> (evalStmnt stm2 w s)
    | While(g, stm) when (boolEval g w s) -> (evalStmnt stm w s) |> (evalStmnt (While(g,stm)) w)
    | While _ -> s

    
// evalStmnt Skip [] Map.empty
// evalStmnt (Ass ("x", N 5)) [] Map.empty
// evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty
// evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty
// evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty
// evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"),Ass ("x", V "x" .+. N 1)))) hello Map.empty
// evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)])


       

let stmnt2SquareFun stm =
    fun w pos acc ->  (evalStmnt stm w (Map.ofList[("_pos_", pos); ("_acc_", acc)])).["_result_"]

let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))

let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))

let containsNumbers : squareFun = 
    stmnt2SquareFun 
        (Seq (Ass ("_result_", V "_acc_"),
              While (V "i" .<. WL,
                     ITE (IsDigit (CV (V "i")),
                          Seq (Ass ("_result_", V "_result_" .*. N -1),
                               Ass ("i", WL)),
                          Ass ("i", V "i" .+. N 1)))))

                 
// singleLetterScore hello 0 0
// doubleLetterScore hello 0 0
// tripleLetterScore hello 0 0
// singleLetterScore hello 0 42
// doubleLetterScore hello 0 42
// tripleLetterScore hello 0 42
// containsNumbers hello 5 50
// containsNumbers (('0', 100)::hello) 5 50
// containsNumbers (hello @ [('0', 100)]) 5 50


type square2 = (int * stmnt) list

let SLS = [(0, Ass ("_result_", arithSingleLetterScore))]
let DLS = [(0, Ass ("_result_", arithDoubleLetterScore))]
let TLS = [(0, Ass ("_result_", arithTripleLetterScore))]

let DWS = [(1, Ass ("_result_", arithDoubleWordScore))] @ SLS
let TWS = [(1, Ass ("_result_", arithTripleWordScore))] @ SLS

let calculatePoints2 (slist: list<square2>) (word: word) =
    slist
        |> List.mapi(fun i elem -> 
            elem |> List.map(fun el -> (fst(el), stmnt2SquareFun (snd(el)) word i)))
        |> List.fold( List.append ) []
        |> List.sortBy(fun el -> fst(el))
        |> List.map(fun elem -> snd(elem))
        |> List.fold ( >> ) id 
        <| 0

// calculatePoints2 [DLS; SLS; TLS; SLS; DWS] hello;;
// // val it : int = 28
// calculatePoints2 [DLS; DWS; TLS; TWS; DWS] hello;;
// // val it : int = 168










//  scrap



// let stmnt2SquareFun (stm: stmnt) : squareFun =
//     fun w pos acc ->  (evalStmnt stm w (Map.ofList[("_pos_", pos); ("_acc_", acc)])).["_result_"]

// let singleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithSingleLetterScore))
// let doubleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleLetterScore))
// let tripleLetterScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleLetterScore))

// let doubleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithDoubleWordScore))
// let tripleWordScore : squareFun = stmnt2SquareFun (Ass ("_result_", arithTripleWordScore))

// let containsNumbers : squareFun = 
//     stmnt2SquareFun 
//         (Seq (Ass ("_result_", V "_acc_"),
//               While (V "i" .<. WL,
//                      ITE (IsDigit (CV (V "i")),
//                           Seq (Ass ("_result_", V "_result_" .*. N -  1),
//                                Ass ("i", WL)),
//                           Ass ("i", V "i" .+. N 1)))))

// singleLetterScore hello 0 0;;