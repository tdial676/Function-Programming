(*Received Extension*)
(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    let search init = 
      let storage = S.create () 
    in 
      let rec iter past =
        if (S.is_empty storage) then raise Not_found else
          let popped_history = S.pop storage in
          let most_recent_board = List.hd popped_history
        in
          if (DS.mem most_recent_board past) then iter past else
            if (D.is_solved most_recent_board) then popped_history else
              let update_past b =
                  S.push  (b :: popped_history)  storage
              in
                List.iter update_past (D.next most_recent_board);
                iter (DS.add most_recent_board past);
      in
        S.push [init] storage;
        iter DS.empty

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

