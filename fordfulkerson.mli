open Graph
open Tools

(*  Type label *)
type label =  {flow : int ; cap : int}

type out_arc = (id * int) 

type arcs = {sr : id ; o_arcs : int out_arcs}


(*  converts an int graph into a label graph *)
val convert_tolabel : int graph -> label graph


(*  returns the reversed graph *)
val build_network : label graph -> int graph


(* returns a list of possible paths linking s -> t*)
val find_path : int graph -> id -> id -> out_arc list


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
val ford_fulkerson : label graph -> id -> id -> int
