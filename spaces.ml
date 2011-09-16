
module TwoD =
  struct
    type real     = float
    type point    = float * float
    type elt      = point
    type interval = float * float
    type range = (float * float) array

    let noninterval (x0, x1) = x1 > x0

    let add_loc ((x0, x1) as interval) x =
      if x0 < x1 then
	if x < x0 then
          (x,x1)
	else if x > x1 then
	  (x0,x)
	else
	  interval
	else if x0 = x1 then
	  if x > x0 then (x0,x) else if x < x0 then (x,x0) else interval
	  else (* x0 > x1, invalid range *)
	    (x,x)

    let dim = 2
    let null_interval = (Pervasives.max_float, Pervasives.min_float)
    let null_range = Array.create dim null_interval

    let range_maker r (f0,f1) =
      let mapf i interval = if i = 0 then add_loc interval f0 else add_loc interval f1 in
      Array.mapi mapf r

    let intersect_intervals (x00,x01) (x10,x11) =
      if x01 < x10 || x11 < x00 then
	null_interval
      else
	(max x00 x10, min x01 x11)

    let is_valid_interval (x0,x1) = x0 <= x1

    let intersect_ranges r0 r1 =
      let ((x00,x01) as x0_interval) = Array.get r0 0 in
      let ((y00,y01) as y0_interval) = Array.get r0 1 in
      let ((x10,x11) as x1_interval) = Array.get r1 0 in
      let ((y10,y11) as y1_interval) = Array.get r1 1 in
      let ivi = is_valid_interval in
      if ivi x0_interval && ivi x1_interval && ivi y0_interval && ivi y1_interval then
	let x_interval = intersect_intervals x0_interval x1_interval in
	let y_interval = intersect_intervals y0_interval y1_interval in
	if ivi x_interval && ivi y_interval then
	  [|x_interval; y_interval|]
	else
	  null_range
      else
	null_range

    let point_in_range r (x,y) =
      let (x0,x1) = Array.get r 0 in
      let (y0,y1) = Array.get r 1 in
      x >= x0 && x <= x1 && y >= y0 && y <= y1

    let to_point e = e
    let square x = x *. x

    let axial_compare n (x0,y0) (x1,y1) =
      if n = 0 then
	Pervasives.compare x0 x1
      else if n = 1 then
	Pervasives.compare y0 y1
      else
	invalid_arg ("TwoD.select: " ^ (Pervasives.string_of_int n))

    let squared_distance (x0,y0) (x1,y1) =
      let dx = x1 -. x0 in
      let dy = y1 -. y0 in
      dx *. dx +. dy *. dy

    let squared_axial_distance n (x0,y0) (x1,y1) =
      if n = 0 then
	square (x0 -. x1)
      else if n = 1 then
	square (y0 -. y1)
      else
	invalid_arg ("TwoD.select: " ^ (Pervasives.string_of_int n))
  end;;
