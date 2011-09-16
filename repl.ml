open Printf

let input_file  = ref None

let set_input_file file = begin input_file := Some (file) end

module Two_D_Point = 
  struct 
    type point = float * float
    type elt   = point
    let to_point p = p 
  end

module City = 
  struct 
    type point = float * float
    type elt   = 
      { name : string;
	population : int; 
	state : string; 
	latlong : point;
      }
    let to_point e = e.latlong 
    let to_string e = 
      let (lat,long) = e.latlong in 
      Printf.sprintf "{name=%s; population=%d; state=%s; latlong=(%g,%g)}"
	e.name e.population e.state lat long
  end

let line_opt ic = 
  try 
    Some(Pervasives.input_line ic)
  with End_of_file -> None

let comma_re = Str.regexp ",[ \t]*"

let make_city_list ic = 
  let make_city city_name pop state lat long = 
    { City.name = city_name;
      City.population = int_of_string pop;
      City.state = state; 
      City.latlong = (float_of_string lat, float_of_string long);
    } 
  in
  let rec loop line_number accum = 
    match line_opt ic with 
      None -> accum 
    | Some line -> 
      let l = Str.split comma_re line in 
	match l with 
	[city_name; pop; state; lat; long] -> 
	  let city = 
	    try 
	      let city = make_city city_name pop state lat long in  
	      begin
		Printf.printf "loop: %s\n" line;
		city
	      end
	    with e -> 
	      let err_msg = Printf.sprintf "Repl.make_city_list: bad line %d, city_name=\"%s\", city_population=%s" line_number city_name pop in
	      failwith err_msg
	  in 
	  loop (succ line_number) (city::accum)
      | _ -> 
	let err_msg = Printf.sprintf "Repl.make_city_list: bad line %d = %s" line_number line in
	failwith err_msg
  in
  loop 1 []
      
module M = Kd_tree.Make(Two_d.Make(City));;


let process_line tree line =
  if line = "q" || line = "Q" then
    raise Exit
  else
    let toks = Str.split comma_re line in 
    match toks with 
      [lat_str; long_str] -> 
	let (lat,long) = (float_of_string lat_str, float_of_string long_str) in 
	let (res, dist) = M.nearest_neighbors tree (lat,long) in 
	String.concat ", " (List.map City.to_string res) 
    | _ -> "Bad input!"


let main () =
  let options =
    [("-input_file", Arg.String set_input_file, "input csv file for city data "); ] in
  let files : string list ref = ref [] in
  let add_file s = files := s :: !files in
  let usage = sprintf "Usage: %s <options> <files>" Sys.argv.(0) in
  begin
    Arg.parse options add_file usage;
    match !input_file with 
      Some file_name -> 
	  let ic = Pervasives.open_in file_name in 
	  let cities = make_city_list ic in 
	  let kd_tree = M.make cities in 
	  begin
	    (try
	       while true do
		 print_string "enter (lat,long) >>> ";
		 Printf.printf "%s\n" (process_line kd_tree (read_line()))
	       done;
	     with Exit ->
	       Printf.printf "Exiting\n");
	    Pervasives.close_in ic
	  end
    | None -> Printf.printf "No input?\n"
  end

let () = main ()
