(**************************************A**************************************)
(*A.1*)
let last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [x] -> [x]
  |h::t ->  let rec iter = function
    | [] -> []
    | [x] -> [x]
    | h::t -> iter t
  in 
  iter t

(*A.2*)
let reverse lst = 
  let rec iter tail rev = 
    match tail with
    | [] -> rev
    | h::t -> iter t (h::rev) 
in 
iter lst []
(*A.3*)
let rec square_list = function
  | [] -> []
  | h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items

(*A.4*)
(*
   The first attempt produces a squared list in reverse order because
   each time he adds to answer using :: he is adding to the front of the list 
   hence creating the list in reverse order. His second attempt does not work 
   either because he is trying to add a list to an element that is not a list
   hence he will get the error: This expression has type 'a list but an expression 
   was expected of type 'a The type variable 'a occurs inside 'a list. The small
   fix he could make on his second attempt is to change :: to @ and put [] around 
   (h * h).
*)
(*A.5*)
let count_negative_numbers lst = 
  let rec iter count tail = 
    match tail with
    | [] -> count
    | h::t -> if h < 0 then iter (count + 1) t else iter count t
  in
  iter 0 lst
(*A.6*)
let power_of_two_list n =
  let rec pow x y = 
    match y with
    | 0 -> 1
    | _ -> x * pow x (y-1) in
  let rec iter curr nums =
    match curr with
    | e when e = n -> nums
    | _ -> iter (curr + 1) (nums @ [(pow 2 curr)]) in
  iter 0 []
(*A.7*)
let prefix_sum lst =
  let rec iter curr = function
  | [] -> []
  | hd :: tl -> (hd + curr) :: (iter (hd + curr) tl)
in
iter 0 lst
(*A.8*)
let deep_reverse lst = 
  let rec iter tail rev = 
    match tail with
    | [] -> rev
    | h::t -> iter t ((reverse(h)::rev))
in 
iter lst []
(*A.9*)
type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list
let deep_reverse_nested lst =
  let rec iter tail rev = 
    match tail with
    | List [] -> List rev
    | Value _ -> tail
    | List (h::t) -> iter (List t) (((iter h [])::rev))
in 
iter lst []
(**************************************B**************************************)
(*B.1*)
let rec filter predicate sequence =
  match sequence with
    | [] -> []
    | h :: t when predicate h -> h :: filter predicate t
    | _ :: t -> filter predicate t

let rec quicksort cmp list = 
  match list with
  | [] -> []
  | pivot :: rest ->
  let smaller = filter (fun x -> cmp x pivot) rest in
  let larger = filter (fun x -> not (cmp x pivot)) rest in
  quicksort cmp smaller @ [pivot] @ quicksort cmp larger

(*B.2*)
(*
  This is an instance of generative recussion because we are recurssing on the 
  split list components that are partly sorted versions of our orginal input list
  hence we are recurssing over the computed data that has been transformed
  rather than a subsets of our original list.
*)
(*B.3*)
(*
  When we run this code, we get a stackoverflow error due to an infinite recussion
  which is occuring because we never reach our base [] -> [] as even_half 
  and odd_half will continue to divide the list trying to reach the base case of size, 
  but it will never reach it hence it will continue to loop on forever until it reaches x/infinity
  which will give it zero however, our stack does not have infinite memory hence the stack
  overflow error due to stack spilling itno our heap because it is out of space. Hence Ben would need
  a Turing Machine for his code to work as it has an infinite tape or he can simply add the base case
  where our list size is 1.
*)
(*B.4*)
let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)

(**************************************C**************************************)
(*C.1*)
let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h::x) rest)
(*
  The subset function takes a list and maps to 2 cases using function 
  with the base case of whether that list is empty and if so, it returns 
  an empty set -> [[]]. However, if the list is not empty, then we grab the head
  and tail of our list and call subset on the tail to continuely split the list 
  elements until we have an empty list and as we are taking away the head each time. 
  The function stores each split list in our rest variable. After breaking down to 
  our empty list, the method combines the previous split list rest with all the 
  elements of the current rest.
*)
(*C.2*)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> p x::r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> r + 1) 0 sequence

(*C.3*)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)

(*C.4*)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1::t1, h2::t2) -> (f h1 h2) :: map2 f t1 t2
let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)
let matrix_times_vector m v = map (dot_product v) m

let transpose mat = accumulate_n (fun x y ->[x]@y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (matrix_times_vector cols) m