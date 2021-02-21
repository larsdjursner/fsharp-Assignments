let empty pair =
    fun pos -> pair

let letterA : int -> char * int = empty('A', 1);;

letterA 0;;


// Create a function add : int -> (char * int) -> (int -> char * int) -> (int -> char *
// int) (note that the middle set of parentheses are needed in this case and are not just there for
// readability) that given a position pos , a pair containing a character and a point value cv , and a
// word represented by the function word returns another function that behaves exactly like word
// except that it returns cv when its position argument is equal to pos .
// Hint: Here are two different correct ways that you can start the add function


let add newPos pair word = 
    fun pos ->
    if (newPos = pos) then pair
    else word pos 

let LettersAB = add 1 ('B', 3) letterA;;

// LettersAB 0;;
// LettersAB 1;;
// LettersAB 42;;

// let lettersABC = add 2 ('C', 4) LettersAB;;

// lettersABC 0;;
// lettersABC 1;;
// lettersABC 2;;

// 1.14

let hello =
    empty('0', 0)
    |> add 0 ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1)

hello;;