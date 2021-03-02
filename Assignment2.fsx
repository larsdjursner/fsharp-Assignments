// exercise 2.1
let rec downto1 i =
    if (i <= 0) then
        []
    else
        i :: downto1 (i - 1)

// downto1 5
// downto1 10
// downto1 0
// downto1 -42


let rec downto2 i =
    match i with
    | i when i <= 0 -> []
    | i -> i :: downto2 (i - 1)

// downto2 5
// downto2 10
// downto2 0
// downto2 -42


// exercise 2.2


let removeOddIdx xs =
    xs 
        |> List.indexed
        |> List.filter(fun (x,_) -> x % 2 = 0)
        |> List.map snd

// removeOddIdx ([ 1; 2; 3; 4; 5; 6 ])
// removeOddIdx ["Marry"; "had"; "a"; "little"; "lamb"; "its"; "fleece";
//  "was"; "white"; "as"; "snow"]

// removeOddIdx ([ 1; 2; 3; 4; ])


//exercise 2.3
let rec combinePair xs =
    match xs with
    | [] -> []
    | h :: t when t.IsEmpty -> []
    | h :: h2 :: t -> (h, h2) :: combinePair (t)
    | xs -> []

combinePair ([]: int list)
combinePair [ true; false ]

// combinePair [ "Marry"
//               "had"
//               "a"
//               "little"
//               "lamb"
//               "its"
//               "fleece"
//               "was"
//               "white"
//               "as"
//               "snow" ]



// exercise 2.4
type Complex  = (float * float)

let mkComplex x y = Complex(x, y)

// let c = mkComplex 1.0 2.0

let complexToPair c =
    let (a, b) = c
    (a, b)

// let c2 = complexToPair (mkComplex 2.0 2.0)


//addition and mult

let (|+|) (x: Complex) (y: Complex) =
    let (a, b) = complexToPair (x)
    let (c, d) = complexToPair (y)
    mkComplex (a + c) (b + d)

let (|*|) (x: Complex) (y: Complex) =
    let (a, b) = complexToPair (x)
    let (c, d) = complexToPair (y)
    mkComplex (a * c - b * d) (b * c + a * d)

// let plus = (|+|) (mkComplex 1.0 2.0) (mkComplex 1.0 2.0)
// let times = (|*|) (mkComplex 1.0 2.0) (mkComplex 1.0 2.0)


// (|+|) (mkComplex 1.0 2.0) (mkComplex 1.0 2.0)

// subtraction and division

let (|-|) (x: Complex) (y: Complex) =
    let (a, b) = complexToPair (x)
    let (c, d) = complexToPair (y)
    mkComplex (a-c) (b-d)


// (|-|) (mkComplex 1.0 2.0) (mkComplex 1.0 1.0)


let (|/|) (x: Complex) (y: Complex) =
    let (a, b) = complexToPair (y)

    match a, b with
    | a, b when a = 0.0 || b = 0.0 -> mkComplex 0.0 0.0
    | a, b ->  (|*|) (mkComplex (a / ((a ** 2.0) + (b ** 2.0))) (-b / ((a ** 2.0) + (b ** 2.0)))) x

// (|/|) (mkComplex -3.3 10.3)(mkComplex -3.2 -2.0)



// exercise 2.7
let explode1 (s: string) = List.ofArray (s.ToCharArray())

// explode1 "Hello World!"

let rec explode2 s =
    match s with
    | "" -> []
    | s -> s.Chars(0) :: explode2 (s.Remove(0, 1))

// explode2 "Hello World!"


// exercise 2.8
let implode (cs: char list) =
    List.foldBack (fun elem acc -> elem.ToString() + acc) cs ""

// implode [ 'H'
//           'e'
//           'l'
//           'l'
//           'o'
//           ' '
//           'W'
//           'o'
//           'r'
//           'l'
//           'd'
//           '!' ]


let implodeRev (cs: char list) =
    List.fold (fun acc elem -> elem.ToString() + acc) "" cs

// implodeRev [ 'H'
//              'e'
//              'l'
//              'l'
//              'o'
//              ' '
//              'W'
//              'o'
//              'r'
//              'l'
//              'd'
//              '!' ]


// exercise 2.9
//piping
let toUpper s =
    s
    |> explode1
    |> List.map (System.Char.ToUpper)
    |> implode

// toUpper "hej 123 goddag"

//func comp
let toUpperFuncComp =
    explode1
    >> List.map (System.Char.ToUpper)
    >> implode

// toUpperFuncComp "hej 123 goddag"

//exercise 2.10
let rec ack (m: int, n: int) =
    match m, n with
    | m, _ when m = 0 -> n + 1
    | m, n when m > 0 && n = 0 -> ack (m - 1, 1)
    | m, n when m > 0 && n > 0 -> ack (m - 1, ack (m, n - 1))
    | _, _ -> 0




// exercise 2.11
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

// time (fun () -> ack(3, 11))



let timeArg1 f a =
    time (fun () -> f a)

// timeArg1 ack (3, 11);;


// exercise 2.12

//using matching
// let downto3 f n e =
//     match n with
//     | n when n > 0 -> 
//         let items = [1 .. n] 
//         List.foldBack f items e
//     | _ -> e

// conditionals
let downto3 f n e = 
    if(n > 0) then
        let list = [1 .. n]
        List.foldBack f list e
    else 
        e;

let fac n =
    downto3 (( * )) n 1


//conditionals
let range g n = 
    if (n > 0 ) then
        let items = [ 1 .. n]
        List.map( g ) items
    else 
        [];

// range fac 0
// (downto3 (fun _ x-> x*5) 3 1)

// fac 0



// exercise 2.13
type Word = (char * int) list

let hello : Word=
    [
        'H', 4
        'E', 1
        'L', 1
        'L', 1
        'O', 1
    ]

//2.14
type SquareFun = Word -> int -> int -> int


let singleLetterScore : SquareFun =
    fun wo pos acc -> 
        let _,w = wo.[pos]
        acc + (w)


let doubleLetterScore : SquareFun =
    fun wo pos acc ->
        let _,w = wo.[pos]
        acc + (2*w)

let tripleLetterScore : SquareFun =
    fun wo pos acc ->
        let _,w = wo.[pos]
        acc + (3*w)
// tripleLetterScore hello 0 0;;

//assignemnt 2.15

let doubleWordScore : SquareFun =
    fun wo pos acc -> (acc * 2)

let tripleWordScore : SquareFun =
    fun wo pos acc -> (acc * 3)    


// doubleWordScore hello 12345 42;;
// tripleWordScore hello 12345 42;;


//assignemnt 2.16
let containsNumbers: SquareFun =
    fun wo pos acc ->
        match wo with
        | wo when List.exists (fun elem -> System.Char.IsNumber(fst elem)) wo -> acc * (-1)
        | _ -> acc

// containsNumbers hello 1 42
// containsNumbers (('1', 10)::hello) 12345 42

// optionals
type Square = (int * SquareFun) list


let SLS : Square = [(0, singleLetterScore)]
let DLS : Square = [(0, doubleLetterScore)]
let TLS : Square = [(0, tripleLetterScore)]

let DWS : Square = SLS @ [(1, doubleWordScore)]
let TWS : Square = SLS @ [(1, tripleWordScore)]


let calculatePoints (squaresList: list<Square>) (w: Word)  =
    squaresList 
        |> List.mapi (fun i elem -> 
            elem |> List.map(fun x -> fst(x), snd(x) w i)
        )
        |> List.fold( List.append ) []
        // |> List.sortBy(fun elem -> fst(elem))
        // |> List.map(fun elem -> snd(elem))
        // |> List.fold ( >> ) id 
        // <| 0

let list = calculatePoints [DLS; SLS; TLS; SLS; DWS] hello;;
// (calculatePoints2 [DLS; SLS; TLS; (2, containsNumbers)::SLS; DWS] hello)
// (calculatePoints [DLS; DWS; TLS; (2, containsNumbers)::TWS; DWS] (('4', 23)::hello))