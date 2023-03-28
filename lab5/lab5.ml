(*Received Extension*)
(***************************************A***************************************)
(*A.1*)
let fibonacci n =
  if (n < 0) then invalid_arg "n must be atleast 0" else
    if (n = 0) then 0 else
      let p1 = ref  0 in
      let p2 = ref 1 in
      let i = ref 1 in
      while !i < n do
        let curr = !p1 + !p2 in
        p1 := !p2;
        p2 := curr;
        i := !i + 1;
      done;
      !p2

let fibonacci2 n =
  if (n < 0) then invalid_arg "n must be atleast 0" else
    if (n = 0) then 0 else
      let p1 = ref 0 in 
      let p2 = ref 1 in
      for _ = 2 to n do
        let curr = !p1 + !p2 in
        p1 := !p2;
        p2 := curr;
      done;
      !p2

(*A2*)
let bubble_sort array =
  let len = Array.length(array) - 1 in
  try
  for i = 0 to len do 
    let swap = ref false in
    for j = 0 to len - i - 1 do 
      if (array.(j) > array.(j + 1)) then
        let curr = array.(j) in
        array.(j) <- array.(j + 1);
        array.(j + 1) <- curr;
        swap := true;
      done;
      if (!swap = false) then raise Exit
    done;
    with Exit -> Printf.printf "\n"

(***************************************B***************************************)
(*B1*)
let meters_per_foot = 0.3048

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i /. 12.0 *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)

(*B2*)
let grams_per_slug = 14593.903203
let get_grams mass =
  match mass with
  | `Gram g -> g
  | `Kilo k -> k *. 1000.0
  | `Slug s -> s *.  grams_per_slug

let mass_add m1 m2 = `Gram ((get_grams m1) +. (get_grams m2))

let get_seconds time = 
  match time with
  | `Second s -> s
  | `Minute m -> m *. 60.0
  | `Hour h -> h *. 60.0 *. 60.0
  | `Day d -> d *. 60.0 *. 60.0 *. 24.0

let time_add t1 t2 = `Second ((get_seconds t1) +. (get_seconds t2))
(*B3*)
let unit_add u1 u2 =
  match (u1, u2) with
  | (`Length l1, `Length l2) -> `Length (length_add l1 l2)
  | (`Mass m1, `Mass m2) -> `Mass (mass_add m1 m2)
  | (`Time t1, `Time t2) -> `Time (time_add t1 t2)
  | _ -> failwith "the units must match"
(*
  We do not get a combinatorial explosion when adding more unit classes, at least as far as unit 
  addition is concerned because we only have to add one more match per additional unit class due 
  to the way we've written our abractions to capture all units under their specified class tags
  create a tagged variable and return it as long as the types match. Therefore as long as we maintain
  this abstractions with each new class then we will avoid a combinatorial explosion.
*)

(***************************************C***************************************)
(*C1*)
let rec make_gram g =
  let grams_per_slug = 14593.903203 in
  let matching other =
    match other#unit_type with 
    | `Gram -> true
    | `Slug -> true
    | _ -> false
  in
    object
      method get_grams = g
      method get_slugs = g /.  grams_per_slug
      method unit_type = `Gram
      method compatible other = matching other
      method add other = 
        if (matching other) then make_gram (g +. other#get_grams) else
          failwith "Units must be Slugs or Grams"
    end

(*C2*)
(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
   let evaluate expr v n = expr#evaluate v n

   (* Return the string representation of an expression. *)
   let show expr = expr#show
   
   (* Return the derivative of an expression. *)
   let differentiate expr v = expr#derive v


(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
        make_number (expr1#value + expr2#value)
    | _ when expr1#is_number = false && expr2#is_number = false && expr1#show = expr2#show -> 
      make_product (make_number 2) expr1  (* var + var = 2 * var *)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end

and make_product expr1 expr2 = 
  match () with
  | _ when expr1#is_zero || expr2#is_zero -> make_number 0  (* 0 * expr = 0 *) (* expr * 0 = expr *)
  | _ when expr2#is_number && expr2#value = 1 -> expr1  (* expr * 1 = expr *)
  | _ when expr1#is_number && expr1#value = 1 -> expr2(* 1 * expr = expr *)
  | _ when expr1#is_number && expr2#is_number ->
    make_number (expr1#value * expr2#value)
  | _ ->  (* create a new object representing the sum *)
    object
      method value = failwith "product expression has no numerical value"
      method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
      method is_zero = false
      method is_number = false
      method evaluate v n =
        make_product (expr1#evaluate v n) (expr2#evaluate v n)
      method derive v =
        make_sum (make_product (expr1#derive v) expr2) (make_product expr1 (expr2#derive v))
    end

(*C2*)
(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)
let f =
  make_sum
   (make_product
    (make_variable "x")
    (make_product
     (make_variable "x")
     (make_product
      (make_variable "x")
      (make_variable "y"))))
   (make_sum
    (make_product
     (make_number 3)
     (make_product
      (make_variable "x")
      (make_product
       (make_variable "x")
       (make_product
        (make_variable "y")
        (make_variable "y")))))
    (make_sum
     (make_product
      (make_variable "y")
      (make_variable "y"))
     (make_number 2))) ;;
(*
   val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)
let dfdx = differentiate f "x" ;;
(*
   val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a; is_number : 
    bool; is_zero : bool; show : string; value : int >
  as 'a = <obj>
*)
show dfdx ;;
(*
   - : string =
"(((x * (x * y)) + (x * (2 * (x * y)))) + (3 * (2 * (x * (y * y)))))"
*)
show (evaluate f "x" 3) ;;
(*
"((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"
*)
show (evaluate (evaluate f "x" 3) "y" 4) ;;
(*- : string = "558"*)
show (evaluate (evaluate dfdx "x" 3) "y" 4) ;;
(*- : string = "396"*)
let n0  = make_number 0
let n1  = make_number 1
let n2  = make_number 2
let x   = make_variable "x"
let s1  = make_sum n1 n2
let s2  = make_sum n1 n0
let s3  = make_sum x n1
let s4  = make_sum (make_variable "y") (make_number 4)
let p1  = make_product n2 n2
let p2  = make_product x n0
let p3  = make_product x n2
let p4  = make_product x s1
let sl1 = make_sum p3 (make_sum p4 s3)
let p5  = make_product n2 p4
let p6  = make_product x s3
let ap1 = make_sum p3 p4
let pa1 = make_product s3 s4
let pl1 = make_product s2 (make_product s4 sl1)