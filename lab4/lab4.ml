(*RECEIVED EXTENSION*)

(************************************A************************************)
(*A1*)
type point = {x : float; y : float}
type segment = {startp : point; endp : point}

let midpoint_segment (seg: segment) =
  {x = (seg.startp.x +. seg.endp.x) /. 2.0; y = (seg.startp.y +. seg.endp.y) /. 2.0}

let segment_length (seg: segment) =
  let square x = x *. x in
  sqrt((square (seg.endp.y -. seg.startp.y)) +. (square (seg.endp.x -. seg.startp.x)))

let print_point (p: point) = Printf.printf "(%g, %g)\n" p.x p.y

let make_point x' y' = {x = x'; y = y'}

let  get_coords (p: point) = (p.x, p.y)

let make_segment (p1: point) (p2: point) = {startp = p1; endp = p2}

let get_points (seg: segment) = (seg.startp, seg.endp)

(*A2*)
let get_x (p: point) = fst (get_coords p)
let get_y (p: point) = snd (get_coords p)

type rectangle = {ll: point; ur: point}
let make_rectangle ll ur = {ll = ll; ur = ur}
let rectangle_lower_segment (r: rectangle) = make_segment (r.ll) (make_point (get_x r.ur) (get_y r.ll))
let rectangle_upper_segment (r: rectangle) = make_segment (make_point (get_x r.ll) (get_y r.ur)) (r.ur)
let rectangle_left_segment (r: rectangle) = make_segment (r.ll) (make_point (get_x r.ll) (get_y r.ur))
let rectangle_right_segment (r: rectangle) = make_segment (make_point (get_x r.ur) (get_y r.ll)) (r.ur)
let rectangle_perimeter (r: rectangle) =  
  2.0 *. abs_float(segment_length (rectangle_lower_segment r) +. segment_length (rectangle_left_segment r))
let rectangle_area (r: rectangle) = segment_length (rectangle_lower_segment r) *. segment_length (rectangle_left_segment r)

type rectangle2 = {lx: float; ly: float; ux: float; uy: float}
let make_rectangle2 lx ly ux uy = {lx = lx; ly = ly; ux = ux; uy = uy}
let rectangle_lower_segment2 (r2: rectangle2) = make_segment (make_point r2.lx r2.ly) (make_point r2.ux r2.ly)
let rectangle_upper_segment2 (r2: rectangle2) = make_segment (make_point r2.lx r2.uy) (make_point r2.ux r2.uy)
let rectangle_left_segment2 (r2: rectangle2) = make_segment (make_point r2.lx r2.ly) (make_point r2.lx r2.uy)
let rectangle_right_segment2 (r2: rectangle2) = make_segment (make_point r2.ux r2.ly) (make_point r2.ux r2.uy)
let rectangle_perimeter2 (r2: rectangle2) = 2.0 *. (abs_float(r2.ux -. r2.lx) +. abs_float(r2.uy -. r2.ly))
let rectangle_area2 (r2: rectangle2) = (r2.ux -. r2.lx) *. (r2.uy -. r2.ly)

(*A3*)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)
let first z = z (fun x _ -> x)
let second z = z (fun _ y -> y)
(*
  Desugar: let make_pair x y = fun m -> m x y:
  --> let make_pair x y = fun x y -> fun m -> m x y 
  --> Bind make_pair to fun x y -> fun m -> m x y 

  Now Desugar: let first z = z (fun x y --> x)
  --> fun z -> z (fun x y -> x)
  Bind first to the value fun z -> z (fun x y -> x)

  Now Desugar: let second z = z (fun x y -> y)
  --> fun z -> z (fun x y -> y)
  Bind second to the value fun z -> z (fun x y -> y)

  verification (in a comment) that first (make_pair x y)
  yields x for any objects x and y:
  Evaluate first (make_pair x y):
    - evaluate (make_pair x y):
      - evaluate x --> x
      - evaluate y --> y
      - evaluate make pair -> fun x y -> fun m -> m x y
        - apply fun x y -> fun m -> m x y
        - substitute x for x and y for y
          --> fun m -> m x y
    - evaluate first (fun m -> m x y) and using above desugaring we get:
    --> fun z -> z (fun x y -> x) and substitute in (fun m -> m x y) for z:
    --> fun (fun m -> m x y) -> (fun m -> m x y) (fun x y -> x)
    - evaluate (fun m -> m x y) (fun x y -> x):
      - evaluate (fun m -> m x y) --> fun m -> m x y
      - evaluate (fun x y -> x) --> fun x y -> x
    - substitute fun x y -> x for m in fun m -> m x y:
      --> fun (fun x y -> x) -> (fun x y -> x) x y
      - evaluate (fun x y -> x) x y:
        - evaluate x --> x
        - evaluate y --> y
        - evaluate (fun x y -> x) --> (fun x y -> x)
        - apply (fun x y -> x) to x and y by substituting x=x and y=y:
          --> x
  Hece prove that the expressions evaluates to x as that is our result.
  - The full substitution model evaluation of second (make_pair 1 2):
    - Evaluate second (make_pair 1 2):
      - evaluate (make_pair 1 2):
        - evaluate 1 --> 1
        - evaluate 2 --> 2
        - evaluate make_pair --> fun x y -> fun m -> m x y (as done above)
        - apply fun x y -> fun m -> m x y to 1 and 2:
          - substitute x = 1
          - substitute y = 2
        --> fun 1 2 -> fun m -> m 1 2
      - Desugar second as done above:
      --> fun z -> z (fun x y -> y)
      apply fun z -> z (fun x y -> y) to fun m -> m 1 2 substituing fun m -> m 1 2 for z:
      --> (fun m -> m 1 2) (fun x y -> y)
      - Now, evaluate (fun m -> m 1 2) (fun x y -> y)
        - evaluate (fun x y -> y) --> (fun x y -> y)
        - evaluate (fun m -> m 1 2) --> (fun m -> m 1 2)
        - apply (fun x y -> y) to (fun m -> m 1 2)
          - substitute (fun x y -> y) for m:
          --> (fun x y -> y) 1 2:
          - evaluate (fun x y -> y) 1 2:
            - evaluate 1 --> 1
            - evaluate 2 --> 2
            - evaluate (fun x y -> y) --> (fun x y -> y)
          - Apply (fun x y -> y) to 1 and 2:
            - substitute x = 1
            - substitute y = 2
            --> fun 1 2 -> 2 --> 2
    Hence proven as our result is 2.
*)

(*A4*)
let rec pow x y = 
  match y with
  | 0 -> 1
  | _ -> x * pow x (y-1)

let int_log x y = 
  let rec iter x y i = 
    match y with
    | y' when (y' mod x) != 0 -> i
    | 0 -> i
    | _ -> iter x (y / x) (i + 1)
  in
  iter x y 0

let make_pairi a b = (pow 2 a) * (pow 3 b)
let firsti pair = int_log 2 pair
let secondi pair = int_log 3 pair
(*A5*)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
| [] -> invalid_arg "Argument is (unary) zero. Please enter another argument"
| _ :: t -> t 

let integer_to_unary num = 
  let rec iter num list= 
    if num = 0 then list else iter (num - 1) (succ list)
in
iter num zero

let unary_to_integer list = 
  let rec iter l num =
    if is_zero l then num else iter (prev l) (num + 1)
  in
  iter list 0
let unary_add num1 num2 = 
  let rec iter l1 l2 =
    if is_zero l1 then l2 else iter (prev l1) (succ l2) 
  in
  iter num1 num2

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' = function
| Zero -> invalid_arg "Argument is zero. Please enter another argument"
| Succ tail -> tail

(*No other changes need to be made asides from name changes due to 
   to our abstraction! See name changes below: *)
let integer_to_unary' num = 
  let rec iter num list= 
    if num = 0 then list else iter (num - 1) (succ' list)
  in
  iter num zero'

let unary_to_integer' list = 
  let rec iter l num =
    if l = zero' then num else iter (prev' l) (num + 1)
  in
  iter list 0
let unary_add' num1 num2 = 
  let rec iter l1 l2 =
    if l1 = zero' then l2 else iter (prev' l1) (succ' l2) 
  in
  iter num1 num2

(*A.6*)
(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)

let zerof = fun _ -> fun z -> z
(*Chnaged s to _ to get rid of warnings*)
  (* or equivalently: let zerof = fun s z -> z *)
  (* or equivalently: let zerof s z = z *)
 
let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)
let one s z = s z
let two s z = s (s z)
let three s z = s (s (s z))
let four s z = s (s (s (s z)))
let five s z = s (s (s (s (s z))))
let six s z = s (s (s (s (s (s z)))))
let seven s z = s (s (s (s (s (s (s z))))))
let eight s z = s (s (s (s (s (s (s (s z)))))))
let nine s z = s (s (s (s (s (s (s (s (s z))))))))
let ten s z = s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)
let church_to_integer church = church (fun x -> x + 1) 0

(*A7*)
(*
   Looking at the type definition of church_to_integer we see that the type signature is
   ((int -> int) -> int -> 'a) -> 'a meaning that it takes as input ((int -> int) -> int -> 'a) as
   it's church value. Now, we know the type signature of zerof is 'a -> 'b -> 'b and matching this 
   with the input type for church_to_integer as it does with unkown types then we can infer that
   'a in zerof is assigned type (int -> int), 'b is assigned to type int and therefore as 'b -> 'b then for the
   church_to_integer signature we will have that 'a will be the same as 'b therefore 'a will
   be assigned to type int aswell. Therefore, this gives us a type signature of  ((int -> int) -> int -> int) -> int
   for our church_to_integer hence why it would return an integer for zerof. Similarly for one, 
   the type signature is ('a -> 'b) -> 'a -> 'b and matching this to the input type signature 
   ((int -> int) -> int -> 'a) of church_to_integer then we see that we can infer ('a -> 'b) in one as type
   (int -> int) therefore assigning both 'a and 'b of type int and pluggin this into the type signature
   of one we get (int -> int) -> int -> int and matching this with the input type signature of church_to_integer
   ((int -> int) -> int -> 'a) we see that 'a is assigned to type int resulting in an inetger
   hence why church_to_integer one returns an integer.
*)

(************************************B************************************)
(*B1.a*)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (l, _)) = l
let right_branch (Mobile (_, r)) = r
let branch_length = function
| Weight (l, _) -> l
| Structure (l, _) -> l
let branch_structure = function
| Structure (_, m) -> `Structure m
| Weight (_, w) -> `Weight w
(*B1.b*)
let rec branch_weight1 = function
| Weight (_, w) -> w
| Structure (_, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) =
   (branch_weight1 l) + (branch_weight1 r)

let rec branch_weight2 b = 
  match branch_structure b with
  | `Weight w -> w
  | `Structure m -> total_weight2 m
and total_weight2 m =
  (branch_weight2 (left_branch m)) + (branch_weight2 (right_branch m))
(*B1.c*)
let rec is_balanced m =
  let left = (branch_length (left_branch m)) * (branch_weight2 (left_branch m)) in
  let right = (branch_length (right_branch m)) * (branch_weight1 (right_branch m)) in
  let balanced = left = right in 
  match (branch_structure (left_branch m), branch_structure (right_branch m)) with
  | (`Structure b1, `Structure b2) -> balanced && (is_balanced b1) && (is_balanced b2)
  | (`Structure b1, _) -> balanced && (is_balanced b1)
  | (_ , `Structure b2) -> balanced && (is_balanced b2)
  | (_, _) -> balanced
(*B1.d*)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = {left = l; right = r}
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m)

let left_branch' {left; _} = left
let right_branch' {right; _} = right
let branch_length' (Branch' (l, _)) = l
let branch_structure' (Branch' (_, s)) =
  match s with
  | Structure' m -> `Structure m
  | Weight' w -> `Weight w

let rec branch_weight' b = 
  match branch_structure' b with
  | `Weight w -> w
  | `Structure m -> total_weight' m
and total_weight' m =
  (branch_weight' (left_branch' m)) + (branch_weight' (right_branch' m))

let rec is_balanced' m =
  let left' = (branch_length' (left_branch' m)) * (branch_weight' (left_branch' m)) in
  let right' = (branch_length' (right_branch' m)) * (branch_weight' (right_branch' m)) in
  let balanced' = left' = right' in 
  match (branch_structure' (left_branch' m), branch_structure' (right_branch' m)) with
  | (`Structure b1, `Structure b2) -> balanced' && (is_balanced' b1) && (is_balanced' b2)
  | (`Structure b1, _) -> balanced' && (is_balanced' b1)
  | (_ , `Structure b2) -> balanced' && (is_balanced' b2)
  | (_, _) -> balanced'

(*B2*)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (t: tree) =
  let rec iter = function
    | [] -> []
    | (Num h) :: t -> Num (h * h) :: (iter t)
    | (Sub h) :: t -> Sub (square_tree h) :: (iter t)
  in match t with
  | Tree t -> Tree (iter t)

let rec square_tree' (Tree lst) =
  let square_t = function
  | Num h -> Num (h * h)
  | Sub h -> Sub (square_tree' h)
in 
Tree (List.map square_t lst)

(*B3*)
let rec tree_map f (Tree lst) =
  let map_func = function
  | Num h -> Num (f h)
  | Sub h -> Sub (tree_map f h)
in 
Tree (List.map map_func lst)
let square_tree'' tree = tree_map (fun n -> n * n) tree
(************************************B************************************)
(*C1*)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

let rec simplify1 exp = 
  match exp with
  | Add (Int x, Int y) -> Int (x + y)
  | Add (exp1, Int 0) -> exp1
  | Add (Int 0, exp1) -> exp1
  | Add (exp1, exp2) -> Add (simplify1 exp1, simplify1 exp2)
  | Mul (Int x, Int y) -> Int(x * y)
  | Mul (Int 0, _) -> Int 0
  | Mul (_, Int 0) -> Int 0
  | Mul (Int 1, exp1) -> exp1
  | Mul (exp1, Int 1) -> exp1
  | Mul (exp1, exp2) -> Mul (simplify1 exp1, simplify1 exp2)
  | Pow (_, 0) -> Int 1
  | Pow (Int x, y) -> Int (pow x y)
  | Pow (exp1, 1) -> exp1
  | Pow (exp1, y) -> Pow (simplify1 exp1, y)
  | Int _ -> exp
  | Var _ -> exp

let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e

(*C2*)
let rec deriv dx = function
| Int _ -> Int 0
| Var x -> if x = dx then Int 1 else Int 0
| Add (x, y) -> Add ((deriv dx x), (deriv dx y))
| Mul (x, y) -> Add ( Mul ((deriv dx x), y), Mul (x, (deriv dx y)))
| Pow (x, y) -> Mul ( Mul (Int y, Pow (x, y - 1)), deriv dx x)
let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d