(* Purely functional, functorized kd-tree algorithm from Overmars (2008). 

   Note the use of a special comparison function to achieve the effect of having 
   all of the points in general position.

   TODO: range query, versions in other languages (Scala, Clojure, Haskell)
*)
module type S =
  sig
    type real
    type point
    type range
    type elt
    (* Make t private and not abstract for debugging and fiddling at the top level *)
    type t = private Leaf of elt | Branch of int * point * range * t * t 
    val split: int -> elt list -> (elt * range * elt list * elt list)
    val make: elt list -> t

    (* kd tree functionality *)

    val nearest_neighbors: t -> point -> elt list * float
    val range_search: t -> range -> elt list

  end;;

(* A static KD tree *)

module Make(M: Multidim.S) :
  (S with type elt   = M.elt 
     with type real  = M.real 
     with type point = M.point 
     with type range = M.range) =
  struct
    type elt   = M.elt
    type real  = M.real 
    type range = M.range
    type point = M.point
    type t = Leaf of elt | Branch of int * point * range * t * t

    let iota n = 
      let rec loop curr accum = 
	if curr <= 0 then accum else loop (pred curr) (curr::accum) in 
      loop n []

    let list_split_at l n =
      if n < 0 then invalid_arg "Kd_tree.list_split_at" else
      let rec loop l n accum =
	match l with
	| [] -> failwith "Kd_tree.list_split_at"
	| x::xs -> if n = 0 then (List.rev accum, l) else loop xs (n-1) (x::accum)
      in loop l n []

    let split (depth : int) (elts : elt list) : (elt * range * elt list * elt list) = 
      let axis = depth mod M.dim in
      match elts with
	[]    -> invalid_arg "split: empty list"
      | [elt] -> invalid_arg "split: singleton list"
      | _ ->
	let cmpf e0 e1 = M.axial_compare axis (M.to_point e0) (M.to_point e1) in
	let len = List.length elts in
	let range = List.fold_left M.range_maker M.null_range elts in
	let sorted   = List.sort cmpf elts in
	let (lt,gte) = list_split_at sorted (len/2) in 
	(List.hd gte, range, lt, gte)
(*
	let median = List.nth sorted (len/2) in 
	let (lt,gte) = List.partition (fun elt-> cmpf elt median < 0) sorted in
	(median, range, lt, gte)
*)	  
    let make elts =
      let rec mk depth es =
	match es with
	  [] -> 
	    let err_msg = Printf.sprintf "Kd_tree.make: num_elts=%d, depth=%d, null list " (List.length elts) depth in 
	    invalid_arg err_msg
	| [elt] -> Leaf elt
	| _ ->
	  let depth' = depth + 1 in
	  let (median, range, lt,gte) = split depth es in
	  Branch(depth mod M.dim, M.to_point median, range, mk depth' lt, mk depth' gte)
      in
      mk 0 elts
	
    let nearest_neighbors root q =
      let update elt ((curr_closest, curr_distance) as curr) =
	let p = M.to_point elt in
	let d_2 = M.squared_distance q p in
	if d_2 < curr_distance then
	  ([elt],d_2)
	else if d_2 > curr_distance then
	  curr
	else (* d_2 = curr_distance, so merge *)
	  (elt::curr_closest,d_2)
      in
      let rec descend node path ((curr_elts, curr_distance) as curr) =
	match node,path with
	| Leaf elt,_ -> ascend path (update elt curr)
	| Branch(axis, median, _, lt, gte),_ ->
	  let n = if M.axial_compare axis q median >= 0 then gte else lt in
	  descend n (node::path) curr
      and ascend path curr =
	match path with
	  [] -> curr
	| node::nodes ->  ascend nodes (check_other_side node [] curr)
      and check_other_side node path ((_, curr_distance) as curr) =
	match node with
	| Branch(axis, median, _, lt, gte)
            when M.squared_axial_distance axis q median <= curr_distance ->
	  if M.axial_compare axis q median >= 0 then
            descend lt path curr
	  else
            descend gte path curr
	| _ -> curr
      in
      descend root [] ([], Pervasives.max_float)

    let range_search t r  =
      let rec search curr accum =
	match curr with
	  Leaf elt -> 
	    if M.point_in_range r (M.to_point elt) then elt::accum else accum
	| Branch(axis, median, range, lt, gte) ->
	  let range_intersection = M.intersect_ranges r range in
	  if range_intersection <> M.null_range then
            let accum' = search lt accum in
            search gte accum'
	  else
            accum
      in
      search t []

  end ;;
(*
let elts0 = [(2.,3.); (5.,4.); (9.,6.); (4.,7.); (8.,1.); (7.,2.)];;
let kd0 = M.make elts0;;

M.nearest_neighbors kd0 (0.5,0.5) ;;
(* should be ([(2.,3.], 8.5) *)
M.nearest_neighbors kd0 (7.,4.) ;;
(* should be ([(5., 4.); (7., 2.)], 4.) *)

let elts1 =
  [(35., 90.);
   (70., 80.);
   (10., 75.);
   (80., 40.);
   (50., 90.);
   (70., 30.);
   (90., 60.);
   (50., 25.);
   (25., 10.);
   (20., 50.);
   (60., 10.)];;

let kd1 = M.make elts1;;
*)
