module Runn = SmallestRUNN.Runn
module Command = struct

  let oc = Core.Out_channel.stdout;;    
  let print_n_flush s =
    let open Core in 
    Out_channel.output_string oc s;
    Out_channel.flush oc;;

  let execute ~verbose ~max ~density ?forceincludezero ?forceexcludezero () =
    try
      let r = Runn.generateRUNN ~max ~density ?forceincludezero ?forceexcludezero () in
      let maxopt_minopt = Runn.getMinMax ~runn:r in
      let max_s = match (snd maxopt_minopt ) with Some i -> string_of_int i | None -> "None" in
      let min_s = match (fst maxopt_minopt ) with Some i -> string_of_int i | None -> "None" in                                                                                  
      let () = print_n_flush (Core.String.concat ["Created a RUNN whose max is ";max_s;" and min is ";min_s;"\n"]) in 
      let () = if verbose then 
                 let s = Runn.toString ~runn:r in
                 let () = print_n_flush s in
                 print_n_flush "\n"
               else () in
      let numberline = Runn.makeNumberLine ~max in
      let () = print_n_flush (Core.String.concat ["Starting search for solution at ";(Core.Time.to_string_utc (Core.Time.now ()));"\n"]) in 
      let answer = Runn.sift ~max ~numberline ~runn:r in
      let () = print_n_flush (Core.String.concat ["Finished search for solution at ";(Core.Time.to_string_utc (Core.Time.now ()));"\n"]) in 
      print_n_flush (Core.String.concat ["Answer is: ";(string_of_int answer);"\n"])
    with
    | Failure s -> print_n_flush s

  let main_command =
    let usage_msg = "Generate a random unsorted non-negative number (RUNN) set of ints and  \
		     find the smallest int not present using a linear time algorithm." in 
    let max = ref 0 in
    (*let min = ref 0 in*)
    let density = ref 0 in
    let forcezeroin = ref false in
    let forcezeroout = ref false in
    (*made a mistake; min is always zero*)
    let verbose = ref false in 
    let options = [(*("-min",Arg.Set_int min,"Minimum");*)
        ("-verbose",Arg.Set verbose,"Be more verbose, print the RUNN.");
	("-max",Arg.Set_int max,"Maximum");
        ("-density",Arg.Set_int density,"Percentage expressed as an int > 0 and <= 100, it is the (approximated) % of unused numbers in the range from zero the largest number in the RUNN set.");
	("-includezero",Arg.Set forcezeroin,"Force include zero; a min of zero is required");
	("-excludezero",Arg.Set forcezeroout,"Force exclude zero");
      ] in 
    let () = Arg.parse options (fun _x -> ()) usage_msg in
    (*===SANITY CHECKS===*)
    (*if !min > 0 && !forcezeroin then
      let () = print_n_flush "A min of zero is required if you want to force the inclusion of zero..." in ()
    if !max <= !min then
      let () = print_n_flush "Max must be > min..." in ()*)
    if !density <= 0 || !density > 100 then
      let () = print_n_flush "Density must be > 0 and <= 100..." in ()
    else if !forcezeroin && !forcezeroout then
      let () = print_n_flush "We can include zero or exclude zero from the random set, but not both..." in ()
    else 
      execute ~verbose:!verbose ~max:!max ~density:!density ~forceincludezero:!forcezeroin ~forceexcludezero:!forcezeroout ();;
end
