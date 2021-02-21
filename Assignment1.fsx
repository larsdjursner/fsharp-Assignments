

// 1.1
let sqr x = x*x

// 1.2 curry
let pow x y = System.Math.Pow(x, y)

// 1.3
let rec sum =
    function
    | 0 -> 0
    | n -> n + sum(n-1)

//1.4
let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n-1) + fib(n-2)

//1.5
// 1 (float * int) eller error?
// 2 int -> int -> int
// 3 (float * int) -> float
// 4 ((float * int -> float) * (int -> int)) -> (float * int)


// let rec power =
//     function
//     | (x,0) -> 1.0
//     | (x,n) -> x * power(x,n-1)

//1.6
let dup (x: string) = x+x


// 1.7
let rec dupn a b =
    match b with 
    | 0 -> ""
    | b -> a + dupn a (b-1)

// let uncurryd dupn (a, b) =
//     dupn a b

// uncurryd dupn ("hey", 4)


// 1.8
let rec bin (n, k) =
    match (n, k) with
    | (_, 0) -> 1
    | (_, _) when n = k -> 1
    | (_, _) -> bin((n-1), (k-1)) + bin(n-1, k)


//  1.9
// 1    (int * int) -> int
// 2    none
// 3    f(2, 3)
//      f(1, 2*3)
//      f(0, 1*6) 
//      = 6
// 4    f(x, y)

// let rec f =
//  function
//  | (0,y) -> y
//  | (x,y) -> f(x-1, x*y)

let rec fact =
    function
    | 0 -> 1
    | n -> n * fact(n-1)


let test(c,e) = if c then e else 0
// 1.10
    // 1. (bool * int) -> int
    // 2. stack overflow
    // 3. returns 0
let test1 = if false then fact -1 else 0


// 1.11

// let curryl f = fun a -> fun b -> f(a,b)
// let uncurryl f = fun (a,b) -> f a b

let curry f a b = f(a,b)
let uncurry f (a, b) = f a b



// 1.12

let empty pair =
    fun pos -> pair

let letterA : int -> char * int = empty('A', 1);;

let empty2 (pair : char*int) (pos : int) =
    pair

let letterA2 = empty2('A', 1);;

// letterA 0;;
// letterA 42;;
// letterA -756;;
// letterA "ab";; compile error works as expected

// 1.13

// let add newPos pair word = 
//     fun pos ->
//         if (pos = newPos) then pair
//         else word pos

let add newPos pair word = 
    fun pos ->
        match pos with
        | _ when newPos = pos -> pair
        | _ -> word pos

let lettersAB = add 1 ('B', 3) letterA;;

// lettersAB 0;;
// lettersAB 1;;
// lettersAB 42;;

// 1.14
let hello =
    empty('0', 0)
    |> add 0 ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1)

// hello 0;;
// hello 1;;
// hello 2;;
// hello 3;;
// hello 4;;

// 1.15
let singleLetterScore (word : int -> char * int) (pos : int) : int =
    let _,w = word pos
    w

let doubleLetterScore (word : int -> char * int) (pos : int) : int =
    let _,w = word pos
    w*2

let trippleLetterScore (word : int -> char * int) (pos : int) : int =
    let _,w = word pos
    w*3

singleLetterScore hello 0;;
doubleLetterScore hello 0;;
trippleLetterScore hello 0;;


let square (mult: int) (word : int -> char * int) (pos : int) : int = 
       let _,w = word pos
       w*mult 

let singleLetterScore2 = square 1;;
let doubleLetterScore2 = square 2;;
let trippleLetterScore2 = square 3;;

singleLetterScore2 hello 0;;
doubleLetterScore2 hello 0;;
trippleLetterScore2 hello 0;;









