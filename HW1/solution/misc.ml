(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * This function is used for add every elements in a given list. And return an int value.
 * There are two situations: 
 * 1.If the list l is empty, then return 0;
 * 2.Else if l not empty, get the head of the list l and add term of recursion. In this term, we will
 *   get a head from the shorter and shorter tail every time until it is empty. The sum of them is equal 
 *   to the sum of the list.
 *) 

let rec sumList l = 
	match l with
	| [] -> 0
	| h::t -> h+(sumList t)


(* digitsOfInt : int -> int list 
 * With the help of functions reverse and getList, we can get a list from given number with right
 * order. e.g. (digitsOfInt 12345) is [1;2;3;4;5]
 * 
 * reverse 'a list -> 'a list -> 'a list
 * This function is used for reverse the order of a list (l2).
 * First we see if l2 is empty, if yes, return l1;
 * If not empty, we get the head of l2 and set it as l1's head.(When we use this function, l1 will
 * be empty list.) Then we recursive call the reversive function, give (h::l1) and t as variable, 
 * it will get the head of the tail t everytime in the recursion, and then link with the tail we 
 * got before. Finlly, we will get reverse l2 as former part linked with l1. 
 * e.g. (reverse [] [1;2;3]) is [3;2;1] 
 * 
 * getList : int -> int list
 * This function can get a list from given number in reverse order.
 * 1.If n/10 = 0, means this n only has one digit, now we return [n], which will be the final tail 
 *   of the list.
 * 2.If n/10 not equals to 0, which means there are more than 1 digits in n. At this time we get 
 *   n mod 10 (n mod 10 equals to the last digit of the number) as the head of List. Then transfer 
 *   n/10 to the recursive term which means cut the last digit of the given number. Within the 
 *   recursion, we will get the last digit every time and link them with the tail which we already 
 *   got before. Finally, we will get the inverse outcome. e.g. (getList 123) is [3;2;1].
 * (see the digits function below for an example of what is expected)
 *)
let rec digitsOfInt n = 
    let rec reverse l1 l2 =
	match l2 with
	| [] -> l1
	| h::t -> reverse (h::l1) t in (*to reverse list l2's order*)

	let rec getList n =
	if n/10 = 0 then [n]
else (n mod 10)::(getList (n/10)) in  (*if input 123, then output [3;2;1]*)

    reverse [] (getList n)  (*use [] as tail, then can reverse l*)


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int
 * In this function we return the number of times every digits of the number can be added (end with n<10).
 * If n is greater than or equal to 10, means we have not get the final value and need recursion, we add 1 to 
 * the recursive term repersents this time we operate. In the recursive term (additivePersistence (sumList(digitsOfInt n)))):
 * n is an int which is given number, (giditsOfInt n) will be a list which elements are every digits of given number in the 
 * right order, then (simList(giditsOfInt n)) will be an int which is the sum of every emements of (giditsOfInt n). 
 * Then the next n will be the sum of every digits of former n.
 * If n less than 10, means we can end the operation. Then return 0, means add 0 to former terms.
 *)

let rec additivePersistence n = 
	if n>=10 then (1 + (additivePersistence (sumList(digitsOfInt n))))
else 0

(* digitalRoot : int -> int
 * In this function we return the final number from adding every digits of the number(end with n<10).
 * If n is greater than or equal to 10, means we have not get the final value and need recursion, we call the 
 * term of recursion (digitalRoot (sumList(digitsOfInt n))). In this term, n is an int which is given number, 
 * (giditsOfInt n) will be a list which elements are every digits of given number in the right order, then 
 * (simList(giditsOfInt n)) will be an int which is the sum of every emements of (giditsOfInt n). 
 * If n less than 10, means we can end the operation. Then return n, because n is the final result we want.
 *)

let rec digitalRoot n = 
	if n>=10 then digitalRoot (sumList(digitsOfInt n))
else n

(* listReverse : 'a list -> 'a list
 * With the help of function reverse, we can reverse the order of given list.
 * e.g. (listReverse [1;2;3;4]) is [4;3;2;1]
 * 
 * reverse 'a list -> 'a list -> 'a list
 * This function is used for reverse the order of a list (l2).
 * First we see if l2 is empty, if yes, return l1;
 * If not empty, we get the head of l2 and set it as l1's head.(When we use this function, l1 will
 * be empty list.) Then we recursive call the reversive function, give (h::l1) and t as variable, 
 * it will get the head of the tail t everytime in the recursion, and then link with the tail we 
 * got before. Finlly, we will get reverse l2 as former part linked with l1. 
 * e.g. (reverse [] [1;2;3]) is [3;2;1] 
 *)

let rec listReverse l = 
	let rec reverse l1 l2 =
	match l2 with
	| [] -> l1
	| h::t -> reverse (h::l1) t in
	reverse [] l  (*use [] as tail, then can reverse l*)

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool
 * This function is used for test whether given string is palindrome.
 * With the help of function of explode, we can get a list from given string. And with the help of 
 * function of listReverse, we can reverse this list and see whether two strings are equal. If yes,
 * then string w is palindrome. And vice versa.
 *)

let palindrome w = 
	if (explode w = listReverse (explode w)) then true
else false

(************** Add Testing Code Here ***************)
