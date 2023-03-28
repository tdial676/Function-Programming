(* name: Thierno Diallo *)
(* email: tdiallo@caltech.edu *)
(************************************************A************************************************)
(*A1*)
(*
  The worst-case asymptotic time complexity of f with respect to it's argument n is O(n).
  This is because f uses a tail recursive function to iterate from n to zero and iter steps
  down from n to zero one step at a time (n-1 with each recursive call) hence making it linearly 
  recursive. As n is the number of iterations and we assume that all built-in arithmetic functions 
  are O(1) therefore (n + 1000 * n * r) takes a constant maximum time to execute irrespective of the size
  of the arguments and (n + 1000 * n * r) will have n many times hence the worst time asymptotic 
  time complexity is linear and O(n).
*)

(*A2*)
(*
  The worst-case asymptotic time complexity of bounce with respect to its arguments m and n
  is O(|m-n|) because bounce is a recursive function whose base case is when m = n or otherwise 
  it decreses/increases m by a factor of 1 until it reaches n all the while adding the numbers in 
  between to m or n and this addition has a complexity of O(1) by our assumption 
  no matter how big m and n are. If m is less than n, then the function adds m to the result and 
  recursively calls itself with m incremented by 1. If m is greater than n, then the function adds
  n to the result and recursively calls itself with m decremented by 1. As it is stepping by 1 each 
  time hence it is linear as all operations take a maximum O(1) time hence the worst-case asymptotic 
  time complexity of function bounce is Θ(|m - n|) as the number of recursive calls is proportional to 
  the absolute difference between m and n
*)
(*A3*)
(*
  The worst-case asymptotic time complexity of trib with respect to its argument n is O(3^n) because 
  with each iteration trib calls three recursive calls of itself with n-1, n-2, n-3 steps unitl the recursive
  calls reach one of it's three base cases n=0, n=1, n=2 at which point it will return and summarize them 
  hence each producing n recursive calls for each hence n*n*n = 3^n hence O(3^n).
*)
(*A4*)
(*
  The worst-case asymptotic time complexity of weird with respect to its argument n is O(log n) because 
  We know that in the worst case if n is not divisible by 2 or 3 then we know that n is odd as it is not 
  divisible by 2 hence weird recurses with n-1 and we know this new argument is even because and odd number
  minus one yields an even number therefore n mod 2 = 0 will true hence it will recurse again with n/2. 
  If this yields an even number not divisible by 2 or 3 then this will be the worst case hence repeating the
  above pattern of subtracting one and dividing by 2 therefore yielding O(2 * log2 n) which goes 
  to O(log n).
*)
(*A5*)
(*
  The aspects of this function that makes it imposible to analyze the fact that for any odd number 
  not equal to 1 f recursively calls itself with an increased input for n rather than continuely 
  decreaing n with each recusrive call. Hence, as n fluctuates between increasing and decreasing 
  with each recursively call we cannot be certain whether f termiantes for all inputs (as stated in the problem)
  or and if f does terminate, we do not know how long it would take to terminate due to the oscillation
  of n which is different depending on the input of n hence it cannot be genralized. Lastly, as the Collatz 
  Conjecture states that the function terminates for all positive integers, but this has not been proven. 
  Hence, as f is not proven to terminate for all inputs then it does not have a time complexity.
*)
(************************************************B************************************************)
(*B1.a*)
let split3 list =
  let rec iter idx l0 l1 l2 l3 =
    match l0 with
    | [] -> (List.rev l1, List.rev l2, List.rev l3)
    | h::t -> match (idx mod 3) with
      | 0 -> (iter (idx + 1) t (h::l1) l2 l3)
      | 1 -> (iter (idx + 1) t l1 (h::l2) l3)
      | _ -> (iter (idx + 1) t l1 l2 (h::l3))
  in 
  iter 0 list [] [] []

(*B1.b*)
let merge2 ls1 ls2 =
  let rec iter l1 l2 l3 =
    match (l1, l2) with
    | ([], h2::t2) ->  iter l1 t2 (h2::l3)
    | (h1::t1, []) -> iter t1 l2 (h1::l3)
    | ([], [])-> List.rev l3
    | (h1::t1, h2::t2) -> if h1 <= h2 then iter t1 l2 (h1::l3) else iter l1 t2 (h2::l3)
  in
  iter ls1 ls2 []
let merge3 l0 l1 l2 =
  merge2 l0 (merge2 l1 l2)

(*B1.c*)
let rec merge_sort3 list =
  match list with
  | [] -> []
  | [_] -> list
  | _ -> match (split3 list) with 
    | (l0, l1, l3) -> 
      merge3 (merge_sort3 l0) (merge_sort3 l1) (merge_sort3 l3)
(*B2.a*)
let smallest_index list = 
  match list with
  | [] -> invalid_arg"You passed in an empty list. Try again with a filled list"
  | _ -> let rec iter min idx curr lst =
    match lst with
    | [] -> min
    | h::t when idx = 0 -> iter min (idx + 1) h t
    | h::t -> if curr <= h then iter min (idx + 1) curr t else iter idx (idx + 1) h t
  in
  iter 0 0 0 list

(*B2.b*)  
let flip_n n list =
  let rec iter curr l0 l1 =
    match l0 with
    | [] -> if curr < n then 
      invalid_arg"n is larger than the list length hence can't flip n elements" else
        l1
    | h::t -> if curr = n then l1@l0 else iter (curr + 1) t (h::l1)
  in
  iter 0 list []

(*B2.c*)
let block_sort1 list =
  match list with
  | [] -> []
  | _ -> let n = smallest_index list in flip_n (n+1) list

(*B2.d*)
let rec block_sort_r list = 
  match list with
  | [] -> []
  | _ -> let f = block_sort1 list in (List.hd f)::block_sort_r (List.tl f)

let block_sort_i list =
  let rec iter l0 l1 =
    match l0 with
    | [] -> List.rev l1
    | h::t -> iter (block_sort1 t) (h::l1)
  in
  iter (block_sort1 list) []

(*B3.a*)
let linrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      let s = split x in 
      combine (fst s) (f (snd s))
  in f  (* return the resulting recursive function *)

(*B3.b*)
let insert_r item =
  (* two base cases: the empty list
   * and when the item < the first element of the list *)
  let is_base lst = lst = [] || item <= List.hd lst in

  (* for both base cases, put the item on the front of the list *)
  let on_base lst = item :: lst in

  (* split the list.  Hint: structural recursion. *)
  let split lst = (List.hd(lst), List.tl(lst)) in

  (* combine to get the final result *)
  let combine first rest_after_rec = first::rest_after_rec in
    linrec is_base on_base split combine

(*B3.c*)
let insertion_sort =
  (* base case: the list is empty *)
  let is_base lst = lst = [] in

  (* if it's a base case, return the empty list *)
  let on_base _ = [] in

  (* otherwise, split (hint: structural recursion again) *)
  let split lst = (List.hd lst, List.tl lst) in

  (* then combine *)
  let combine first rest_after_rec = insert_r first rest_after_rec in

    linrec is_base on_base split combine

(*B4.a*)
let binrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      let (s0, s1, s2) = split x in 
      combine s0 (f s1) (f s2)
  in f  (* return the resulting recursive function *)

(*B4.b*)
let quicksort =
  let is_base lst = lst = [] in
  let on_base _ = [] in
  let split lst =
    match lst with
      | [] -> invalid_arg "quicksort: can't split"
      | h :: t -> 
        let lt = List.filter (fun x -> x < h) t in
        let ge = List.filter (fun x -> x >= h) t in 
        (h, lt, ge)
  in
  let combine pivot lt ge = lt@(pivot::ge) in
    binrec is_base on_base split combine

(*B5.a*)
let tailrec is_base on_base next =
  let rec f inputs =
    if is_base inputs then 
      on_base inputs
    else
      f (next inputs) 
  in f  (* return the tail-recursive function *)

(*B5.b*)
let insert_i item lst =
  let is_base (_, rest) = rest = [] || item <= List.hd rest in
  let on_base (prev, rest) =  List.rev(prev)@(item::rest) in
  let next (prev, rest) = ((List.hd rest)::prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)

(*B5.c*)
let insertion_sort_i lst =
  let is_base (_, rest) = rest = [] in
  let on_base (prev, _) = prev  in
  let next (prev, rest) = (insert_i (List.hd(rest)) prev, List.tl(rest)) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)

(************************************************C************************************************)
(* Assume we are storing only integers in the tree. *)
(* type tree =
  | Leaf
  | Node of int * tree * tree   data value, left subtree, right subtree *)

type tree =
| Leaf
| Node of int * int * tree * tree   (* level, value, left/right subtrees *)
(*C1*)
let rec member item = function
| Leaf -> false
| Node (_, value, left, right) -> if value = item then true else
  if  (member item left) then true else 
    if (member item right) then true else
      false

(*C2*)
(* Level of an AA tree. *)
let level = function
  | Leaf -> 0
| Node (lvl, _, _, _) -> lvl
  
let split = function
  | Leaf -> Leaf
  | Node (lvl, v, l, Node (llvl, lv, ll, lr)) when (lvl = llvl) && (lvl = level lr) -> 
      Node (lvl + 1, lv, Node(lvl, v, l, ll), lr)
  | tree -> tree
let skew tree = 
  match tree with
  | Node (lvl, v, Node (llvl, lv, ll, lr), r) 
       when lvl = llvl ->
    Node (llvl, lv, ll, Node (lvl, v, lr, r))
  | _ -> tree

(*C3*)
let rec insert item tree =
  match tree with
  | Leaf -> Node(1, item, Leaf, Leaf)
  | Node (_, v, _, _) when item = v -> tree
  | Node (lvl, v, l, r) -> if item < v then 
    split (skew (Node (lvl, v, insert item l, r))) else
    split (skew (Node (lvl, v, l, insert item r)))
