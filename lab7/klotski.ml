(*Received Extension*)
(* klotski.ml: core functionality of the Klotski game. *)
(* Student name: Thierno Diallo *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"
let is_solved b = 
  let solved = LocSet.of_list [(3, 1); (3, 2); (4, 1); (4, 2)] in
   let equal key locs =  LocSet.equal locs solved in
    CharMap.exists (equal) (b.pieces)

let compare b1 b2 = 
  let unoccupied = LocSet.compare b1.unoccupied b2.unoccupied in
    if unoccupied != 0 then unoccupied else
      let rec pieces = function
        | [] -> LocSetSet.empty
        | (_, loc)::t -> LocSetSet.add (loc) (pieces t)
    in
      let b1_pieces = pieces (CharMap.bindings b1.pieces) 
      and b2_pieces = pieces (CharMap.bindings b2.pieces) in
        LocSetSet.compare (b1_pieces) (b2_pieces)


let remove c ({ pieces = p; unoccupied = u } as b) = 
  try
    let spot = LocSet.union u (CharMap.find c p) in
    {pieces = CharMap.remove c p; unoccupied = spot}
  with
    Not_found -> b

let add (c, p) { pieces = ps; unoccupied = u } = 
  if ((CharMap.mem c ps) = true) || ((LocSet.subset p u) != true) then None else
    Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}

let make_move (c, d, i) b =
  if ((CharMap.mem c b.pieces) = false) || (i < 1) then None else
    (*Helpers. Pretty self-descriptive*)
    let interpret_move i (r, c) =
      match d with (*Remebering that top left is 0, 0*)
      | Up -> (r - i, c) 
      | Down -> (r + i, c)
      | Left -> (r, c - i)
      | Right -> (r, c + i) 
    in
      let curr_loc = CharMap.find c b.pieces in
      let next_loc = LocSet.map (interpret_move i) (curr_loc)
      in 
        (*Checking if we are still on the board after our move*)
        if (LocSet.exists (fun (r, c) -> c < 0 || r < 0 || r > 4 || c > 3) next_loc)
          then None else
            (*Checking for overlap*)
            let rec overlap j =
              if (j < 1) then true else
                (* let next = (LocSet.map (interpret_move j) (curr_loc)) in
                let removed =  (LocSet.union curr_loc b.unoccupied) in
                let subset = LocSet.subset next removed in
                subset && overlap (j - 1) *)
                (LocSet.subset (LocSet.map (interpret_move j) (curr_loc))
                            (LocSet.union curr_loc b.unoccupied)) && overlap (j - 1)
              in 
                if overlap i then add (c, next_loc) (remove c b) else None

let next b =
  (*Helpers to see how far in each a direction a piece can move in*)
  let total_next_moves c =
    let rec directional_next_moves c d i =
      match (make_move (c, d, i) b) with
      | None -> []
      | Some b' -> b' :: directional_next_moves c d (i + 1)
    in 
      directional_next_moves c Up 1 @ directional_next_moves c Down 1
       @ directional_next_moves c Left 1 @ directional_next_moves c Right 1
  in
    (*Concatenating all possible moves*)
    let rec iter l p =
      match p with
      | [] -> l
      | (c, _)::t -> iter (l @ total_next_moves c) t
    in
      iter [] (CharMap.bindings b.pieces)

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end