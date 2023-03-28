open Boardrep

module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      if (B.get_size b1) <> (B.get_size b2) then
        failwith "boards have different sizes" else
          let size = B.get_size b1 in
          let locs = List.init (size * size) (fun i -> (i / size, i mod size))
        in
          let count = List.fold_left (fun acc loc ->
            if (B.get b1 loc <> B.get b2 loc) && (B.get b2 loc <> 0) then
              acc + 1
            else
              acc
          ) 0 locs in
          count
  end
  module Manhattan(B : BoardRep) : BoardMetric with type t = B.t = 
  struct
    type t = B.t
    let distance b1 b2 =
      if (B.get_size b1) <> (B.get_size b2) then 
        failwith "boards have different sizes" else
          let size = B.get_size b1 in
          let locs = Array.init (size * size) (fun i -> (i / size, i mod size)) in
          let b1_locs = Array.init (size * size) (fun i -> locs.(i)) in
          let () =Array.iter (fun loc -> 
            let idx = B.get b1 loc in 
            b1_locs.(idx) <- loc) locs in
          let count = Array.fold_left (fun acc loc ->
            if (B.get b1 loc <> B.get b2 loc) && (B.get b2 loc <> 0) then
                let r_dist = abs (fst b1_locs.(B.get b2 loc) - fst loc) 
                and l_dist = abs (snd b1_locs.(B.get b2 loc) - snd loc) in
              acc + r_dist + l_dist
            else
              acc
          ) 0 locs in
          count
  end

