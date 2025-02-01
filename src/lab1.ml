open Util (* see util.ml *)

(******************)
(** Starter Code **)
(******************)

exception IndexError

(* Return the i'th element of a list
 * raise IndexError if i is out of bounds
 * *)
let rec nth (i : int) (l : 'a list) : 'a =
  match l with
  | [] -> raise IndexError  (* Empty list, index is out of bounds *)
  | x :: _ when i = 0 -> x  (* We found the element at index i *)
  | _ :: xs -> nth (i - 1) xs  (* Recurse on the tail with i-1 *)

(* Append two lists *)
let rec append (l1 : 'a list) (l2: 'a list) : 'a list =
  match l1 with 
  | [] -> l2 (* If the first list is empty, return the second list *)
  | x:: xs -> 
    let r = append xs l2 in (* Recursively append the rest of l1 to l2 *)
    x::r (* Prepend the first element of l1 to the result *)

(* Reverse a list *)
let reverse (l : 'a list) : 'a list =
  let rec h l r = 
    match l with 
    | [] -> r (* If l is empty, return the reversed list r *)
    | x:: xs -> h xs (x::r) (* Recursively moves the head of l to the front of r *)
  in h l [] (* Initializes r as empty list and calls the helper function*)

(* Length of a list*) 
(* Do not change 'let lentgth' to 'let rec length'.
 * Instead, define a helper function inside `length` 
 * that uses recursion to calculate the length of the list 
*)
let rec length (l : 'a list) : int =
  match l with
  | [] -> 0  (* If the list is empty, the length is 0 *)
  | _ :: xs -> 1 + length xs  (* Otherwise, count 1 and recurse on the tail *)

(* Return the part of list l beginning at index 0 and ending at index iend
 * raises IndexError exception when out of bounds*)
let rec list_prefix (iend : int) (l : 'a list) : 'a list =
  if iend < 0 then raise IndexError
  else if iend = 0 then []
  else
    match l with
      [] -> if iend = 0 then []
            else raise IndexError
    | a::d ->
       a :: list_prefix (iend-1) d

(* Return the part of list l beginning at istart and running through
   the end of the list *)
let rec list_suffix (istart : int) (l : 'a list) : 'a list =
  if istart < 0 then raise IndexError  (* If the index is negative, raise an error *)
  else match l with
    | [] -> if istart = 0 then []  (* If the list is empty and istart is 0, return empty list *)
            else raise IndexError  (* If istart is positive, it's out of bounds *)
    | a :: d -> if istart = 0 then l  (* If we've reached the start index, return the rest of the list *)
                else list_suffix (istart - 1) d  (* Otherwise, skip the head and recurse on the tail *)


(* Merge sorted lists l1 and l2 based on cmp.  The result is a sorted
   list containing all elements from both l2 and l2. *)
let rec merge (cmp : 'a->'a->bool) (l1 : 'a list) (l2 : 'a list) : 'a list =
  match l1, l2 with
  | [], l | l, [] -> l  (* If either list is empty, return the other *)
  | x1 :: xs1, x2 :: xs2 -> (* Decomposes both lists into their heads and tails *)
    if cmp x1 x2 
    then x1 :: merge cmp xs1 l2  (* Choose x1 if it satisfies cmp, then recurse *)
    else x2 :: merge cmp l1 xs2  (* Otherwise, choose x2 and recurse *)
  

(* Sort list l via mergesort

   mergesort takes two inputs: a cmp function and a list l to sort.

   cmp is a function that compares two elements of list l.  When cmp
   returns true, its first argument comes first in the sorted lest.
   When cmp returns false, its second argument comes first in the
   sorted list.  *)

let rec mergesort (cmp : 'a->'a->bool) (l:'a list) : 'a list =
  let len = length l in
  if len <= 1 then l
  else
    let half = len / 2 in
    let prefix = list_prefix half l in
    let suffix = list_suffix half l in
    merge cmp (mergesort cmp prefix) (mergesort cmp suffix)

(***********)
(** Tests **)
(***********)

(* See description in testing.ml *)

let nth_tests =
  ("Nth", (*name of the test group*)
   (fun (i,l)->nth i l), (*function to be tested *)
   (=), (*what operator to use for equal *)
   (=), (*what operator to use for equality of exceptions*)
   Some((fun x -> str_pair string_of_int str_int_list x), string_of_int),
        (*pretty printer for input and output*)
        (*the lambda function makes a string pair from the input types of the tested function
         * e.g.here the input parameters are int list, see "nth i l" *)

        (*below are the test cases as a list of tuples
         * each tuple is: testname, input, expectedOuput
         * where input is a tuple having all parameters
         * and expected output is
         * either Ok expectedResult  or Error expectedException*)
   [
     (Some("simple list"), (0, [1;2;3;4;5]), Ok 1);
     (Some("error"), (-1, [1;2;3;4;5]), Error IndexError);
     (Some("element at index 2"), [1; 2; 3; 4; 5], Ok 3);
     (Some("element at index 0"), [10; 20; 30], Ok 10);
     (Some("element at index 1"), [100; 200; 300], Ok 200);
     (Some("out of bounds (index 3)"), [1; 2; 3], Error IndexError);
     (Some("out of bounds (empty list)"), [], Error IndexError);
  ])

let append_tests =
  ("append", (fun (l1,l2)->append l1 l2), (=), (=),
   Some((fun x -> str_pair str_int_list  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), ([1;2],[3;4]), Ok [1;2;3;4]);
     (Some("append empty list"), ([], [1;2;3]), Ok [1;2;3]);
     (Some("append to empty list"), ([1;2;3], []), Ok [1;2;3]);
     (Some("longer lists"), ([1;2;3;4], [5;6;7;8]), Ok [1;2;3;4;5;6;7;8]);
     (Some("list with duplicate elements"), ([1;1;2], [2;3;3]), Ok [1;1;2;2;3;3]);
     (Some("append two empty lists"), ([], []), Ok []);
  ])

let reverse_tests =
  ("reverse", reverse, (=), (=), Some(str_int_list,str_int_list),
   [
     (Some("simple list"), [1;2;3;4;5], Ok[5;4;3;2;1]);
     (Some("reverse empty list"), [], Ok []);
     (Some("single element list"), [42], Ok [42]);
     (Some("list with repeating elements"), [1;1;2;2;3], Ok [3;2;2;1;1]);
     (Some("long list"), [10;20;30;40;50;60], Ok [60;50;40;30;20;10]);
     (Some("reverse list of strings"), ["a";"b";"c"], Ok ["c";"b";"a"]);

  ])

let length_tests =
  ("length", length, (=), (=), Some(str_int_list, string_of_int),
   [
     (Some("simple list"), [1; 2; 3; 4; 5], Ok 5);
     (Some("empty list"), [], Ok 0);
     (Some("single element"), [42], Ok 1);
     (Some("multiple ones"), [1; 1; 1; 1; 1; 1; 1; 1; 1; 1], Ok 10);
   ])

let list_prefix_tests =
  ("list_prefix", (fun (iend,l) -> list_prefix iend l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [1;2]);
     (Some("empty prefix"), (0, [1; 2; 3; 4; 5]), Ok []);
     (Some("whole list"), (5, [1; 2; 3; 4; 5]), Ok [1; 2; 3; 4; 5]);
     (Some("out of bounds"), (6, [1; 2; 3]), Error IndexError);
     (Some("empty list"), (2, []), Error IndexError);
     (Some("prefix from index 1"), (1, [10; 20; 30; 40]), Ok [10]);
  ])

let list_suffix_tests =
  ("list_suffix", (fun (istart,l) -> list_suffix istart l), (=), (=),
   Some((fun x -> str_pair string_of_int  str_int_list x),
        str_int_list),
   [
     (Some("simple list"), (2,[1;2;3;4;5]), Ok [3;4;5]);
     (Some("error less than boundary"), (-1,[1;2;3;4;5]), Error IndexError);
     (Some("suffix from index 2"), [1; 2; 3; 4; 5], Ok [3; 4; 5]);
     (Some("suffix from index 0"), [10; 20; 30], Ok [10; 20; 30]);
     (Some("empty list suffix"), [], Ok []);
     (Some("suffix from index 1"), [5; 10; 15], Ok [10; 15]);
     (Some("out of bounds (index 4)"), [1; 2; 3], Error IndexError);
  ])

let merge_tests =
  ("merge", (fun (cmp,l1,l2) -> merge cmp l1 l2), (=), (=),
   Some((fun (cmp,l1,l2) -> str_pair str_int_list str_int_list (l1, l2)),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3],[2;4;5]), Ok [1;2;3;4;5]);
     (Some("merge with duplicates"), ((<),[1;1;3],[2;3;4]), Ok [1;1;2;3;3;4]);
     (Some("merge empty list"), ((<),[],[1;2;3]), Ok [1;2;3]);
     (Some("merge equal elements"), ((<=),[1;2;3],[1;2;3]), Ok [1;1;2;2;3;3]);
     (Some("merge with all elements greater"), ((<),[6;7;8],[1;2;3]), Ok [1;2;3;6;7;8]);
     (Some("merge when both lists are already sorted"), ((<),[1;2;3],[4;5;6]), Ok [1;2;3;4;5;6]);
     (Some("simple list reverse"), ((>),[3;1],[5;4;2]), Ok [5;4;3;2;1]);
  ])


let mergesort_tests =
  ("mergesort", (fun (cmp,l) -> mergesort cmp l), (=), (=),
   Some((fun (cmp,l) -> str_int_list l),
        str_int_list),
   [
     (Some("simple list"), ((<),[1;3;4;2;5]), Ok [1;2;3;4;5]);
     (Some("Tiny list"), ((<),[5;3]), Ok [3;5]);
     (Some("Empty list"), ((<),[]), Ok []);
     (Some("Big list"), ((<),[21;22;23;24;25;20;19;18;17;16;15;14;13;12;11;10;9;8;7;6;5;4;3;2;1]), Ok [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25]);
     (Some("Already sorted list"), ((<),[1;2;3;4;5]), Ok [1;2;3;4;5]);
     (Some("Single number list"), ((<),[1]), Ok [1]);
     (Some("Extreme number difference list"), ((<),[421;1;59;15019;32;3;17;1598]), Ok [1;3;17;32;59;421;1598;15019]);
     (Some("Same number list"), ((<),[1;1;1;1;1;1;1;1;1]), Ok [1;1;1;1;1;1;1;1;1]);
     (Some("Many repeats list"), ((<),[1;5;7;7;5;1;5;7;1]), Ok [1;1;1;5;5;5;7;7;7]);
   ])
