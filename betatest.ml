open Tools
open Gfile
open Fordfulkerson
open Graph 


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

(*
    let rec affichage l = match l with
        |a :: rest -> (string_of_int a) ^ (affichage rest)
        |[] -> ""     
    in
   (*NOS FONCTIONS*)
*)

(*
  	let graph = gmap (build_network (convert_tolabel (gmap graph int_of_string ))) string_of_int in
 	Printf.printf "%s %!" (affichage (find_path (gmap graph int_of_string) 0 4)) ;

*)

(*
	let graphh = gmap graph int_of_string in
	(*let graph = build_network (convert_tolabel (gmap graph int_of_string )) in
	let chemin = (find_path graph  _source _sink) in
	let flow = (min_f graph 900 chemin) in *)
	let graphhh = gmap (aug_f graphh 10 [0;3;2;4]) string_of_int in
	(*Printf.printf "Chemin %s %!\n" (affichage chemin) ;
	Printf.printf "Flow %s %!\n" (string_of_int flow) ;
*)

*)

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graphhh in

  () 

