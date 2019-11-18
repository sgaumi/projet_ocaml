open Graph
open Tools

(*  Type label *)
type label =  {flow : int ; cap : int}


(*  converts an int graph into a label graph *)
val convert_tolabel : int graph -> label graph


(*  returns the reversed graph *)
val build_network : label graph -> int graph


(* returns a list of possible paths linking s -> t*)
val find_path : int graph -> id -> id -> int out_arcs list


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
val ford_fulkerson : label graph -> id -> id -> int
