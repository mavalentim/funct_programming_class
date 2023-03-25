(*exercises*)

(*exercise 2.1*)
(*
let timediff (h1,m1) (h2,m2) = 
    if h1<12 && h2>12 then (((h1-(h2-12))*60)+(m1-m2))/60
    else if h1>12 && h2<12 then ((((h1-12)-h2)*60)+(m1-m2))/60
    else (((h1-h2)*60)+(m1-m2))/60
*)

let timediff (h1,m1) (h2,m2) = ((h1*60)+m1)-((h2*60)+m2)

(*this function will get the difference of the two hours and multiply that by 60, generating the value in minutes
it then sums the minutes of each hour and takes the difference between this operation. The idea is seeing hours+minutes
as just minutes and then taking the difference between two of that.

there is a HUGE limitation to this function which is if you calculate the difference between 23:40 and
02:30 it should be 2 hours and 20 minutes. Instead, it will understand the difference between these hours 
in the same day. 

It turns out its a little more complicated than I thought to make a function that gives the difference to whichever is
closer. I left a sketch above if that is what you guys mean in the exercise, but I didnt finish because I prioritized 
algorithms! *)


(*exercise 2.2*)
let minutes (h,m) = timediff(h,m) (00,00) 
minutes (23,1)

(*this function justs produces the amount of minutes until you get to midnight. It does that by using the timediff function
but fixing the parameters at 24 00. *)

(*exercise 2.3 = difference from ex 11 from first list?*)
let rec pow = function
    | (s :string,0)->""
    | (s :string, n :int)-> s+pow(s,n-1)

pow("a",10) //how i feel in fp haha
(*i was unsure if you wanted us to include the little dot,
but the function above takes a string and a n and then just 
replicates that string n times*)


(*exercise 2.4*)
let rec triangle2 (n,k) =
    if k =0 || n=k then 1
    else triangle2(n-1, k-1) + triangle2(n-1,k)

(*this is a rec function that just represents the binomial formula really
there is a condition in which if k is equal to 0 or if n=k then 1 is the value,
just like a binomial (n n) or (n 1). *)


(*exercise 2.5*)
(*
f is a function that takes two ints and returns an int. Because of that the function's type is int -> int -> int.

the function terminates running when x is 0, and y is any value.
This menas that any momment x becomes 0 during the evaluation, 
f will not run itself again, and hence terminate.

3) the evaluation steps for f(2,3) would be:

f(2,3) =

x = 2, y =3 and so it goes to the second case
where f(2-1, 2*3), hence

f(1,6) = 

x= 1, y=6 and so it goes again to the second case
where f(1-1, 1*6) and hence

f(0,6)=

as x=0 the function now will go to the first 
case and will output y, as its explicit in the first case

6

4) the mathematical meaning of f(x,y) is
the function f being evaluated with the arguments
x and y. The idea of the function is to multiply these
arguments and then decrease x and then multiply by y again.


*)

(*exercise 2.6
test is of type (bool*int)->int 

let test(c,e) = if c then e else 0 results in an eternal recursion
which will crash the program. The function test() has another
function as one of its arguments, so it first has to evaluate such
function. That function is not ready to take in negative values.
Hence, before test can even run, the e argument will go into infinite
recursion.


if false then triangle2(-1,-2) else 0 returns 0. This is what the book
refers to as lazy evaluation or short circuit evaluation. The expression
triangle2(-1,-2) will never be run, it skips directly to produce 0:
the expression would only be run if there was true in the begginig.

*)

(*exercise 2.7*)
let curry (f : 'a * 'b -> 'c) (x : 'a) (y : 'b) : 'c = f (x, y)


(*i have not fully comprehended the line that explains which function leads to what so i just wrote this function that would 
forcefully follow the types that were given. As far as i understand curry takes a function f, a whole value x, a whole value y 
and the c comes from applying the function we gave with the values we provided
*)

let uncurry (f: 'a -> 'b -> 'c) (x :'a, y: 'b) = f x y

(*I did something similar here as I found the declaration of the functions to be very confunsing. The main difference
in this one is that it should use the function f with parameters x and y each one at a time, uncurried. These parameters
are given as a pair, to fit with the type requirement of the exercise*)

(*With understanding I could point which f, g and h and x and y would fill in these functions*)