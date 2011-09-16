module type S =
  sig
    (* Dimension of the space we're interested in *)
    val dim : int 
    (* The "real" or floating point number system for location and distance *)
    type real 
    (* The n dimensional real values point *)
    type point
    (*  *)
    type range

    (* The actual element (eg, a city) with a point *)
    type elt

    val null_range:range
    val range_maker: range -> elt -> range
    val intersect_ranges: range -> range -> range
    val point_in_range: range -> point -> bool
    val to_point : elt -> point
    val axial_compare: int -> point -> point -> int
    val squared_distance : point -> point -> float
    val squared_axial_distance : int -> point -> point -> float
  end;;
