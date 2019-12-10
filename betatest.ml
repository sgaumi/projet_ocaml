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

    let rec affichage l = match l with
        |a :: rest -> (string_of_int a) ^ (affichage rest)
        |[] -> ""     
    in
    
   let string_of_label (l:label) = (string_of_int l.flow)^"/"^(string_of_int l.cap) in
   (*NOS FONCTIONS*)


(*     Test des fonctions intermediaires         *)

	let graph1 = gmap graph int_of_string in
	let graph2 = build_network (convert_tolabel graph1) in
	let chemin = (find_path graph2  _source _sink) in
	let flow = (min_f graph2 900 chemin) in
	let graph3 = aug_f (convert_tolabel graph1) flow chemin in
	let graph4 = gmap graph3 string_of_label in
	Printf.printf "Chemin %s %!\n" (affichage chemin) ;
	Printf.printf "Flow %d %!\n"  flow ;



  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph4 in

  () 

