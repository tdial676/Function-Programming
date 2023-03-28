(*Received Extension*)
(*********************************A*********************************)
(*A1*)
(*
let factorial n =
  let rec iter m r =
    if m = 0
      then r
      else iter (m - 1) (r * m)
  in iter n 1
in
  factorial 3

FRAME 0 (initial environment)
parent: none
bindings:
- : [primitive function -]
* : [primitive function *]

FUNCTION 0 (fun n -> let rec iter m r = ...)
env: FRAME 0
params: n
body:
  let rec iter m r =
  if m = 0
    then r
    else iter (m - 1) (r * m)
  in iter 3 1

FRAME 1 (let factorial n = ...)
parent: FRAME 0
bindings:
  factorial : FUNCTION 0

FRAME 2 (FUNCTION 0 applied to 3)
parent: FRAME 0
bindings:
  m : 3

FUNCTION 1 (fun m r -> if m = 0 ...)
env: FRAME 3
params: m r
body:
  if m = 0
    then r
  else iter (m - 1) (r * m)

FRAME 3 (FUNCTION 1 in iter m 1)
	parent: FRAME 2
	bindings:
		iter : FUNCTION 1

FRAME 4 (FUNCTION 1 on 3, 1)
parent: FRAME 3
bindings:
m : 1
r : 3

FRAME 5 (FUNCTION 1 on 2, 3)
parent: FRAME 3
bindings:
m : 2
r : 3

FRAME 6 (FUNCTION 1 on 1, 6)
parent: FRAME 3
bindings:
m : 1
r : 6

FRAME 7 (FUNCTION 1 on 0, 6)
parent: FRAME 3
bindings:
m : 0
r : 6

Therefore the result is 6
*)
(*A2*)
let factorial =
  let f = ref (fun _ -> 0) in 
    f := (fun n -> if n = 0 then 1 else
      n * !f(n - 1)
      );
  !f

(*********************************B*********************************)
(*B1*)
exception Stat_error of string
let make_stat_1 () =
  let sum = ref 0.0
  and sumq = ref 0.0
  and n = ref 0 in
  object
    method append x =  
      sum := x +. !sum;
      sumq := (x *. x) +. !sumq;
      n := 1 + !n;
    method mean = 
      match !n with 
      | 0 -> raise (Stat_error "need at least one value for mean")
      | _ -> !sum /. (float_of_int !n)
    method variance = 
      match !n with
      | 0 -> raise (Stat_error "need at least one value for variance")
      | _ -> (!sumq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n)
    method stdev = 
      match !n with
      | 0 -> raise (Stat_error "need at least one value for stdev")
      | _ -> sqrt ((!sumq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n))

    method clear = 
      sum := 0.0 *. !sum;
      sumq := 0.0 *. !sumq;
      n := 0 * !n;
  end
(*B2*)
let make_stat_2 () =
  let sum = ref 0.0
  and sumq = ref 0.0
  and n = ref 0 in
  object(self)
    method append x =  
      sum := x +. !sum;
      sumq := (x *. x) +. !sumq;
      n := 1 + !n;
    method mean = 
      match !n with 
      | 0 -> raise (Stat_error  "need at least one value for mean")
      | _ -> !sum /. (float_of_int !n)
    method private _variance = 
      (!sumq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n)
    method variance = 
      match !n with
      | 0 -> raise (Stat_error "need at least one value for variance")
      | _ -> self#_variance
    method stdev = 
      match !n with
      | 0 -> raise (Stat_error "need at least one value for stdev")
      | _ -> sqrt self#_variance

    method clear = 
      sum := 0.0 *. !sum;
      sumq := 0.0 *. !sumq;
      n := 0 * !n;
  end 
(*********************************C*********************************)
(*C1*)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf

    let is_empty h = h = Leaf

    let find_min = function
    | Leaf -> raise Empty
    | Node(_, item, _, _) -> item

    (*HELPERS*)
    let rank = function
    | Leaf -> 0
    | Node(r, _, _, _) -> r

    let create_node item h1 h2 = 
      let a = rank h1 and b = rank h2 in
      if (a >= b) then Node(b + 1, item, h1, h2) else Node(a + 1, item, h2, h1)
    
    let rec merge h1 h2 = 
      match (h1, h2) with
      | (Leaf, t2) -> t2
      | (t1, Leaf) -> t1
      | (Node(_, e, l, r), h) when e < find_min h -> 
          create_node e l  (merge r h)
      | (h, Node(_, e2, l2, r2)) -> create_node e2 l2  (merge r2 h)
    (*END*)

    let insert h item = merge h (Node(1, item, Leaf, Leaf))
    let delete_min = function
    | Leaf -> raise Empty
    | Node(_, _, l, r) -> merge l r
    
    let rec from_list = function
    | [] -> empty
    | h::t -> insert (from_list t) h
  end

let heap_sort lst =
  let rec iter pq l1 =
    if (PriorityQueue.empty = pq) then l1 else
      iter (PriorityQueue.delete_min pq) ((PriorityQueue.find_min pq) :: l1)
  in
    List.rev (iter (PriorityQueue.from_list lst) [])

(*c2*)
(* Signature for ordered objects. *)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module MakePriorityQueue (Elt : ORDERED_TYPE)
  : (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
  exception Empty

  type elem = Elt.t

  (*
   * Data type: either
   * -- a Leaf, or
   * -- a Node of (rank, item, left heap, right heap).
   *)
  type t = Leaf | Node of int * elem * t * t

  let empty = Leaf

  let is_empty h = h = Leaf

  let find_min = function
  | Leaf -> raise Empty
  | Node(_, item, _, _) -> item

  (*HELPERS*)
  let rank = function
  | Leaf -> 0
  | Node(r, _, _, _) -> r

  let create_node item h1 h2 = 
    let a = rank h1 and b = rank h2 in
    if (a >= b) then Node(b + 1, item, h1, h2) else Node(a + 1, item, h2, h1)
  
  let rec merge h1 h2 = 
    match (h1, h2) with
    | (Leaf, t2) -> t2
    | (t1, Leaf) -> t1
    | (Node(_, e, l, r), h) when e < find_min h -> 
        create_node e l  (merge r h)
    | (h, Node(_, e2, l2, r2)) -> create_node e2 l2  (merge r2 h)
  (*END*)

  let insert h item = merge h (Node(1, item, Leaf, Leaf))
  let delete_min = function
  | Leaf -> raise Empty
  | Node(_, _, l, r) -> merge l r
  
  let rec from_list = function
  | [] -> empty
  | h::t -> insert (from_list t) h
  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst =
  let rec iter pq l1 =
    if (StringPQ.empty = pq) then l1 else
      iter (StringPQ.delete_min pq) ((StringPQ.find_min pq) :: l1)
  in
    List.rev (iter (StringPQ.from_list lst) [])

(*********************************D*********************************)
(*D1*)
type 'a contents = Evaluated of 'a | Unevaluated of (unit -> 'a)

type 'a lazy_t = 'a contents ref
let make_lazy e = ref (Unevaluated e)
let force lz = 
  match !lz with
  | Evaluated eval -> eval
  | Unevaluated f ->
    lz := Evaluated (f ());
    f ()

(*D2.a*)
let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))
(*
let rec sum = function
  | [] -> 0
  | h::t -> h + sum t
let almost_sum f = 
    fun l -> 
      match l with 
      | [] -> 0
      | h::t -> h + f t
      therefore
*)
let sum = 
  let almost_sum f =
    fun l -> 
      match l with
      | [] -> 0
      | h::t -> h + (f t)
  in
    y almost_sum

(*D2.b*)
(*
 let factorial n =
  let rec iter n r =
    if n = 0
      then r
      else iter (n - 1) (n * r)
  in
    iter n 1  
*)
let factorial2 =
  let almost_factorial f =
    fun (n, r) -> 
    if n = 0
      then r
      else f ((n - 1), (n * r))
  in
    fun n -> (y almost_factorial) (n, 1)