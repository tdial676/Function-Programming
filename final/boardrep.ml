type loc = int * int
type move = Up | Down | Left | Right

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end
(*Helper i made to find index of items in a list to avoid redundancy in code*)
let rec idx i item = function
| [] -> failwith "Not Found."
| h::t -> if h = item then i else idx (i + 1) item t 
(*Helper to get hole position to avoid redundancy in code*)
let get_new_hole_position hole m =
  let r_hole = fst (hole)
  and c_hole = snd (hole) in
  match m with
  | Up -> (r_hole - 1, c_hole)
  | Down -> (r_hole + 1, c_hole)
  | Left -> (r_hole, c_hole - 1)
  | Right ->(r_hole, c_hole + 1) 
(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)

module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        acontents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size = 
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let conts = Array.init (size * size) (fun x -> x + 1) 
        and hole = (size - 1, size - 1) in 
        Array.set conts (size * size - 1) 0;
        {acontents = conts; size = size; hole = hole}
       
    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else if size * size <> List.length lst then
        failwith "invalid list length"
      else
        let sorted = List.sort compare lst in 
        if sorted <> (List.init (size * size) (fun i -> i)) then  
          failwith "invalid list contents" else
            let hole_idx = idx 0 0 lst in 
            let hole = (hole_idx / size, hole_idx mod size)
            and conts = Array.of_list lst in
            {acontents = conts; size = size; hole = hole}


    let get_size b = b.size

    let get_hole b = b.hole

    let get { acontents; size = s; _ } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          acontents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location

    let make_move b m =
      let size = get_size b
      and hole = get_hole b in
      let new_hole = get_new_hole_position hole m in
      match new_hole with
      | (r, c) -> if (r < 0 || c < 0 || r >= size || c >= size ) then
        raise Invalid_move else
          let new_hole_idx = ((fst new_hole) * size) + (snd new_hole)
          and old_hole_idx = ((fst hole) * size) + (snd hole) in 
          let value = b.acontents.(new_hole_idx) in 
          let conts = Array.copy b.acontents in
          Array.set conts old_hole_idx value;
          Array.set conts new_hole_idx 0;
          {acontents = conts; size = size; hole = new_hole}

    let show = make_show get get_size
  end

module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        mcontents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size =
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        let hole = (size - 1, size - 1) in
        let conts = 
          List.init (size * size - 1) (fun x -> x + 1) @ [0]
          |> List.map (fun n -> 
            if n = 0 then (n, hole) else
              (n, ((n - 1) / size, (n-1) mod size)))
          |> List.fold_left (fun acc (n, l) -> LocMap.add l n acc) LocMap.empty
        in
        {mcontents = conts; size = size; hole = hole}
  
    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else if size * size <> List.length lst then
        failwith "invalid list length"
      else
        let sorted = List.sort compare lst in 
        if sorted <> (List.init (size * size) (fun i -> i)) then  
          failwith "invalid list contents" else
            let hole_idx = idx 0 0 lst in 
            let hole = (hole_idx / size, hole_idx mod size) in
            let conts = List.mapi (fun i n -> 
                (n, (i / size, i mod size))) lst
              |> List.fold_left (fun acc (n, l) -> 
                LocMap.add l n acc) LocMap.empty
          in
          {mcontents = conts; size = size; hole = hole}


    let get_size b = b.size

    let get_hole b = b.hole

    let get { mcontents; _ } l = 
      try
        LocMap.find l mcontents
      with Not_found ->
        raise Invalid_location

    let make_move b m =
      let size = get_size b
      and hole = get_hole b in
      let new_hole = get_new_hole_position hole m in
      match new_hole with
      | (r, c) -> if (r < 0 || c < 0 || r >= size || c >= size ) then
        raise Invalid_move else
          let value = get b new_hole in 
          let conts = LocMap.add hole value (LocMap.add new_hole 0 b.mcontents)
      in
      {mcontents = conts; size = size; hole = new_hole}
        
    let show = make_show get get_size
  end

