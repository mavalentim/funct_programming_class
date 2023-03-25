(*exercise 3*)

(*exercise 3.1*)
(*the first one is not a recursive function, it just uses an f# in built operator that creates a list
starting from the specified n, using -1 as a step and ending at 1. I kinda forced it to use if else by clarifying 
the n case. However that n case is needed cause it'd have trouble doing that using only the else statement.*)
let downTo n = 
    if n = 0 then []
    elif n<0 then failwith "negative numbers" 
    else [n .. -1 .. 1]
downTo -5

(*this second function is a recursive function that just matches an integer to itself concatenated in front of the n-1 
version of itself. By doing n-1 it goes through the values. When this goes down until meeting a 0, the function
just returns an empty list. This feature is used throughout this hand in: concatenating something to an empty list
is really useful in these recursive functions as it not only stops the execution of the function but it also does not 
interferes with the final ouput: [a;b]::[] = [a;b]*)
let rec downTo2 n = 
    match n with
    | 0 -> []
    | n when n> 0 -> n :: downTo2(n-1)
    | _ -> failwith "Negative number"

downTo2 -5


//quick evaluation to check it:

// downTo2(3)
// 3::downTo2(2)
// 3::2::downTo2(1)
// 3::2::




(*exercise 3.2*)
(* this one is another recursive function, it reacts depending on what it gets:
- if it gets an at least three element value, it will take the 1st and third element, and reapply the
function on the third element. If it gets a two value list. it will just keep both. I thought as I was writting,
that this could prolly be done with two clauses, where one is the one that takes the odd number out and the other
just returns what it got but by the end of the sentence I realized that matching [] and x are different, 
so you could still make that list clearer but youd still prolly need 3 cases. I think this is proof because after
popping the stuff there will be either a odd number or an even number of stuff left and because of the way 
the function pops stuff out, there wont be a situation where the function is ill prepared for the input.*)

let rec removeOddIdx lista =
    match lista with
    | [] -> []
    | head::head2::tail -> head::removeOddIdx(tail)
    | x-> x
    

removeOddIdx [1;2;3;4]

(*exercise 3.3*)

(*This function basically has 3 cases to work its way through the list making pairs out of two consecutive values.
It does that mainly in the third case, where when finding a list, it takes out the head, a x2, which ill refer as 
the second head, and then the tail of the list. It pairs x1,x2 and appends that to a recursive application of the same 
function. It basically pops pairs out of the list, until it sees one value remaining or nothing, and then it just
returns an empty list. This way it ends and ends on an empty list. *)

let rec combinePair lista =
    match lista with
    | [] -> []
    | [x] -> []
    | x1::x2::xs -> [(x1,x2)]@combinePair(xs)
    

combinePair [2;4;3;2;3;4;7]

(*
this functions evaluation
combinePair([1;2;3;4;5])=
(1,2)::combinePair([3;4;5]) =
(1,2)::(3,4)::combinePair([5]) = 
(1,2)::(3,4)::[] =
*)


(*exercise 3.4*)
(*I separated the cases in this exercise, that is i did one set of infix operators 
not using type/record and another where i first defined a generic record and then used it.
The addition function uses a reeorg function that returns a 2 uple so that it knows what it
should pass it foward and what should remain in the same unit. The subtraction function passes
everything to pence.*)

//helper functions
let reeorg (c: int) (div :int) = 
    let remainder = c%div
    let tonext = c/div
    (tonext,remainder)

let first (a,b)=
    a

let second(a,b)=
    b


//Money tuple addition
let ( ^+^ ) (g1,s1,c1) (g2,s2,c2) =  //function takes 2 3-uples and returns a 3-uple
    let cooper = reeorg (c1+c2) 12
    let silver = reeorg(s1+s2) 20
    let gold = (g1+g2)
    (gold+first(silver) , first(cooper)+second(silver), second(cooper)) 
    

(0,20,11)^+^(0,0,1) //testing it!




//infix function for subtracting those values without records - this function takes two 3-uples and returns a 3-uple 


//Money tuple subtraction
let (^-^) (g1,s1,c1) (g2,s2,c2) =
    let value1 = (g1*240)+(s1*12)+c1
    let value2 = (g2*240)+(s2*12)+c2
    let total_in_pence = value1-value2
    (0,0,total_in_pence)^+^(0,0,0)^+^(0,0,0) //now we redistribute it for the other values
    

(2,3,4)^-^(1,4,4)




type Money = {pound :int ;shilling :int; pence:int}

// Record money tuple addition
let ( |+| ) x  {pound = a; shilling=b; pence=c } y {pound = a1; shilling=b1; pence=c1 } = 
    x^+^y 

//Record money tuple subtraction
let (|-|) x{pound=a; shilling=b; pence=c}  y{pound = a; shilling=b; pence=c }=
    x^-^y






(*exercise 3.5*)

(*In this exercise I declared my own infix operators to perform the specific operations required,
I assumed addition on these numbers works paralleled to a-b = a+(-b) and summed with the inverse,
I also assumed that division is the multiplication by the multiplicative inverse. I studied linear alg
in a summer school and in the bachelor but i am not 100% that every division is the multiplication by the inverse.*)

//addition
let (.+) (a,b) (c,d)= (a+c,b+d) 

//multiplication
let ( .* ) (a,b) (c,d)= (a*c-b*d,b*c+a*d) 


//subtraction
let ( .- ) (a,b) (c,d) = (a,b).+(-c,-d)

//division
let square a = a*a
let denominator (a,b) = square(a)+square(b)

//i decided to declare square and denominator to help the compiler was complaining

let( ./ ) (a,b) (c,d) = (a,b) .* (c/denominator(c,d),-d/denominator(c,d))


(*exercise 3.6*)

(*this function just goes through the list and subtracts the tail using the function recursively on it.
What is bound to happen is that, as the function progresses the - will become + with other - because of the
changing of the iteration, so it will first be even and so - then odd and +. 
This took me 4 hours and a friend had to help giving me that idea that it was simpler than I thought.
But i didnt manage to finish it*)

(*the evaluation would be
altsum [1;2;3;4]=
1 - altsum[2;3;4] = 
1 - (2 -altsum[3;4]) =
1 - (2 -(3 - altsum[4]))

*)

let rec altsum = function 
    | x0::xs -> x0 - (altsum xs) 
    | []-> 0
   









(*
downto2(3) = 
3::downto2(3-1)
3::(2::downto(2-1))
3::(2::(1::(downTo2(1-1))))
3::(2::(1::[]))
3::(2::([]))
3::([2])

*)

(*not part of the exercise but i will keep as comment for future usage
let downto3 n = 
    let rec aux n acc =
        match n with
        | 0 -> acc 
        | n -> aux (n-1) (acc@[n])
    aux n []

downto3 5
*)


(*
    []
    [5]
    [5;4]
    [5;4;3;2;1]



*)