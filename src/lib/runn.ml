module Runn = struct

  type t = int option Core.Array.t

  (*density : "the percentage of unused numbers in the range from zero to the 
    largest number in the set." But we're just approximating it, not assured. *)
  let generateRUNN ~max ~density ?forceincludezero ?forceexcludezero () =
    let () = Random.self_init () in
    (*For max 10 and min 0 -> 
      index = value (if min = zero) and array should be :  
      [0 1 2 3 4 5 6 7 8 9 10 11]*)
    (*density here is being approximated; we're not guarunteeing that we will 
      achieve the level of density specified because the rng could and will 
      repeat digits, so the set of numbers is not a set in the sense that some 
      members repeat themselves sometimes.*)
    let numberofelements = ((max + 2) * density) / 100 in
    (*NOT REQUIRED: Support RUNNs that start with any N such as : [10 11 12 13 14 15 ... ]*)
    let intarray = Array.init numberofelements
                     (fun _ith ->
                       Some (Random.int max)) in
    (*command line ensures these mutually exclusive options are never both present*)
    let () =
      (match forceincludezero with
       | Some true -> intarray.(0) <- (Some 0)
       | Some false -> ()
       | None -> ()) in
    let () =
      (match forceexcludezero with
       | Some true -> intarray.(0) <- None
       | Some false -> ()
       | None -> ()) in
    intarray;;

  let makeNumberLine ~max =
    (*+1 for zero + 1 for max+1 which might be the smallest int answer*)
    let numberofelements = (max + 2) in
    (*For max 10 and min 0 -> 
      index = value (if min = zero) and array should be :  
      [0 1 2 3 4 5 6 7 8 9 10 11]*)
    (*Support RUNNs that start with any N such as : [10 11 12 13 14 15 ... ]*)
    Array.init numberofelements
      (fun ith -> Some ith);;

  (*===this function is not strictly needed but if we were provided a set of 
    RUNNs not created by us, we would have no idea of the min and max.*)
  let getMinMax ~runn =
    (* (min, max) tuple *)
    Core.Array.fold runn ~init:(None, None)
      ~f:(fun accum x ->
        match x with
        | Some v -> 
           (match accum with
            | None, None -> (Some v, Some v)
            | Some y, Some z ->
               if v < y then (Some v, Some z)
               else if v > z then (Some y, Some v)
               else (Some y, Some z)
            | Some _, None 
              | None, Some _ -> accum              
           )
        | None -> accum);;
  
  let toString ~runn =
    let s = Core.Array.fold runn ~init:[]
              ~f:(fun accum x ->
                match x with
                | Some v -> (Core.Printf.sprintf "%d" v)::accum
                | None -> (Core.Printf.sprintf "None")::accum
              ) in
    Core.String.concat ~sep:" " s;;

  (*given a complete numberline from the min to the max of the RUNN, start 
    eliminating elements by indexing into the array, and then traverse the 
    array to extract the first (lowest index) element still present in the 
    numberline.  Min is not required...problem instructions presume zero.*)
  let sift ~max ~(numberline:t) ~(runn:t) =
    let () = Core.Array.iter runn ~f:(fun x -> match Core.Option.is_some x with
                                               | true ->
                                                  let i = Core.Option.value_exn x in 
                                                  numberline.(i) <- None
                                               | false -> ())
    in
    Core.Array.fold_until numberline ~init:None ~f:(fun accum x -> match x with
                                                               | None -> (Continue accum)
                                                               | Some i -> Stop i)
      ~finish:(fun _accum ->
        let opt = numberline.(max+1) in
        match Core.Option.is_some opt with
        | true -> Core.Option.value_exn opt
        | false -> raise Not_found
      )
end
