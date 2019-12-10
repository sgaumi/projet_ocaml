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
  
  (* These command-line arguments *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  let string_of_label (l:label) = (string_of_int l.flow)^"/"^(string_of_int l.cap) in

  (* Open file *)
  let graph = from_file infile in
  let graph2 = convert_tolabel (gmap graph int_of_string) in
  let (flow, graph3) = (ford_fulkerson graph2 _source _sink 0) in
  Printf.printf "Flow %s %!\n" (string_of_int flow) ;

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile (gmap graph3 string_of_label) in

  () 

