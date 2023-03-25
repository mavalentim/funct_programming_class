(*exercises*)
(*exercise 1.1*)
let sqr x = x*x 
(* This function takes an integer and returns 
an integer, squaring the initial integer*)

(*exercise 1.2*)
let pow x n = System.Math.Pow(x,n) 
(*this function takes two floats and returns a float
by raising the first float to the power of the second float and returning
a float as response*)


(*exercise 1.3*)
let g n = n+4 
(*this function takes an integer and returns an integer. It does so by 
summing 4 to the inputted integer, which naturally returns an integer*)

(*exercise 1.4*)
let h x y = System.Math.Pow((System.Math.Pow(x,2) + System.Math.Pow(y,2)),2)
(*this function takes a float and another float and returns a float. As it is not
written as a pair though, it actually takes one float at a time, allowing for partial
evaluation! The parameters go inside of another function in the body, 
provided by System.Math.Pow, but that is ok! *)


(*exercise 1.5*)
let rec f = function 
    |0->0 
    |n -> n+f(n-1) 

f 4
(*this function takes an integer and returns an integer. It has two cases,
in one, if get the value 0, it gives back 0. However, it has the rec keyword
which allows it to use it on itself in one of the cases. This allows to 
repeat its usage on itself, and this way if cruises through the n's which are reducing.
Below, there is a evaluation of the function on the value 4

f 4=
4 + f(3) =
4 + 3 + f(2)=
4 + 3 + 2 + f(1)=
4 + 3 + 2 + 1 + f(0)=  only case where the first case is used
4 + 3 + 2 + 1 + 0 = 
10

 *)

(*exercise 1.6*)
let rec fibonacci = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibonacci(n-2)+fibonacci(n-1)

fibonacci 4;;
(*
    this function takes int and returns ints. it has three cases if the int is 0 or 1,
    it just returns itself, however, if its something else, the function will enter
    a recursive state in which it is itself applied to the two previous values,
    which then are summed. This leads to a rabbit hole as applying them goes 
    all the way back to f evaluated in 0 and in 1. It is important to define three cases
    precisely so that the function stops at some point.

    An evaluation of the fibonacci function on value 4 is provided:

    fibonnaci(4)=
    fibonacci(2)+fibonacci(3)=
    fibonacci(0)+fibonacci(1)+fibonacci(1)+fibonacci(2)= first time the other cases are used
    0+1+1+fibonacci(0)+fibonacci(1)=
    0+1+1+0+1=
    3

*)

(*exercise 1.7*)
let rec sum = function
    | (m, 0) -> m
    | (m,n) -> m+n+sum(m,n-1)

sum(1,1)

(*
    this function takes two integers as a pair and returns an integer. In this sense
    it is not partially evaluatable: it doesn't really go m->n->int. It goes (m,n)-> int.
    It does so in two cases. The first one is for the case where n is equal to zero. when that happens 
    the function is no longer recursive. The other case is for the situation where n is still not zero.
    N plays an interesting role in this function: it kinda resembles an index from while loops. Only resembles
    as in fact this structure is not an iteration. The function reproduces itself in recursive fashion until n=0.


*)

(*exercise 1.8*)
(*
The answers are:
    (System.Math.PI, fact -1) is (float * Undefined)
    fact(fact 4) is int
    power(System.Math.PI, fact 2) is float
    (power, fact) is (float -> float -> float * int -> int)

some more comments:
the first one has a function that tries to
evaluat something outside its domain. it results
in an error, therefore. We should not define it
as an int as it is not evaluated in an int.
However, I dont see it as a function only either,
as it tries to evaluate something.

the fourth one depends a little on how pow is designed
if pow takes a pair as an argument, then it would be
(float,float)->float. The way we defined pow, earlier
in the exercises it takes two arguments in a spaced manner.

*)

(*exercise 1.9*)


(*
the environment is a collection of bindings. Hence,
 a -> 5;;
 f -> to a function, the function takes an argument a and its body is to a + 1;;
 g -> to a function, it takes an argument b and its body is (f b) + a;;
this collection is the environment
f 3 leads to 3+1, which is 4
g 3 leads to (f 3) + a, a is 5 so it actually takes to (3+1)+5 = 9
*)

(*exercise 10*)
let dup x :string = x+x

(*in this one, we just use a similar function to 
adding something to itself, but we force the arguments
to be a string. with that, the function dup is 
string->string*)

(*exercise 11*)
let rec dupn2 = function 
    | (x :string, 0)-> "" 
    | (x :string, y :int)-> x+dupn2(x,y-1)  

(*in this one, we use two cases and each gets a pair
(x,something). The x one remains and the something
has to be reduced everytime. In every 'round' we 
use the very string we passed x, and concatenate it
with the very function applied to the same string and 
a integer which is one less than the previous one*)

