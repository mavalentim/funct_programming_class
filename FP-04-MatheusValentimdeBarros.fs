module a2
(*
exercise 1 was basically using inbuilt functions and then i decided to use a recursive function that mimics the pattern matching
of a list, but for strings. So I used s[0] instead of head of list (x) and s.Remove(0,1) for the tail of the list (xs)

exercise 2 i just mimic the sum recursive function as if it was with integers, but now it is with strings. The idea is the same
and i could have used reduce now thinking about it. On the second part I just use the rev to make it the opposite.

exercise 3 is just exploding changing stuff and imploding it again, and then doing it in different ways. 

exercise 4 is just creating two locals using the previous values and comparing them.

I didnt have time to attend exercise 5,6,7 but I will make them in the do overs. Sorry for this delays, I only had today to do
this list due to work and health conditions.


*)


(*Ive marked the ones that yall gave feedback with the feedback comment. Ive naturally also redone the code to take
the feedback into consideration*)


// Exercise 4.1 - explode - string to char list
let explode (s:string) = 
    let a = s.ToCharArray()
    List.ofArray(a)


//feedback was: Incorrect. explode2 has type string -> string list but should have type string -> char list.
let rec explode2 s = 
    if s = "" then []
    else s[0]::explode2(s.Remove(0,1))
    




// Exercise 4.2 - implode - char list to string

//Incorrect. You should use List.foldBack and List.fold for 
//this exercise. Furthermore, your functions have type string list -> string but should have char list -> string


(*previous implementations
let rec implode = function
    |x::xs -> x+implode(xs)
    |[] -> ""

let rec implodeRev x = 
    let xinv = List.rev(x)
    implode(xinv) 
*)

//Normal order implode --> ['a';'b';'c'] becomes "abc"

let implode (lista :char list) = 
    List.foldBack (fun c acc -> string c + acc) lista ""



//Reverse order implode --> ['a';'b';'c'] becomes "cba"
let char_acc a b = 
    let stringa = string a
    let stringb = string b
    stringb+stringa

let implodeRev (lista :char list) = 
    List.fold (char_acc) "" lista

implodeRev ['a';'b';'c']














// Exercise 4.3 - toUpper
let toUpper (s:string) =
    let list_chars = explode(s)
    list_chars |> List.map (System.Char.ToUpper) |> List.map (string) |> implode
    

// explode >> map >> implode
let toUpper1 (s:string) = 
    (explode >> List.map (System.Char.ToUpper) >> List.map (string) >> implode) s


// let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (Char.ToUpper(x))));;
let toUpper2 (s:string) = 
    (List.map (string) << List.map (System.Char.ToUpper) << explode) s |> implode

toUpper2("chega")


// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
let rec palindrome (s:string) = 
    let oneway = s |> toUpper
    let otherway = s |> toUpper |> explode |> List.map (string) |> implodeRev
    otherway = oneway





// Exercise 4.5 - ack
let rec ack (m,n) = 
    if m=0 then n+1
    else if (m>0 && n = 0) then ack(m-1,1)
    else if m>0 && n>0 then ack(m-1,ack(m,n-1))
    else failwith "negative number are not allowed"

    // ack(3, 11) = 16381

ack(3,11)


// Exercise 4.6 - time
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);


//Function I was told to write:

let timeArg1 f a =
    let start = System.DateTime.Now in
    let res = f (a) in
    let finish = System.DateTime.Now in
    (res, finish - start);

(*
    I didnt fully understand what you guys mean with the tip, but as it was a tip I assume it was not obligatory
let timeArg1_beta f a =
    let start = System.DateTime.Now in
    fun 
    let finish = System.DateTime.Now in
    (res, finish - start);
*)


// Exercise 4.7 - HR 5.4 - downTo2 f n e


//I created this auxiliary function as the exercise really pushed towards using fold
let auxiliary customFunction customList customAcc = 
    List.fold customFunction customAcc customList

let downto1 f (n,e) =
    let innerList = [1..n]
    if n>0 then auxiliary f innerList e 
    else e

downto1 sum (4,2)



// factorial function using downto1 for recursion.
//I created a prod auxiliary function just to multiply things
let prod n y=
    n*y

let fact n =
    if n<0 then failwith "n cant be smaller than 0"
    else if n = 0 then 1
    else downto1 prod (n,1)
    

//build a list [g(1)..g(n)] struggled with this one due to time
let append x y =
    let lista = x @ [y]
    lista

append 2 3

downto1

let buildList g n = 


