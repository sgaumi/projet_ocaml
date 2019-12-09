open Graph
open Tools

(*  Type label *)
type label =  {flow : int ; cap : int}

type out_arc = (id * int) 

type arcs = {sr : id ; o_arcs : int out_arcs}

type path = id list

val get_node_list : int graph -> id list

(*  converts an int graph into a label graph *)
val convert_tolabel : int graph -> label graph


(*  returns the reversed graph *)
val build_network : label graph -> int graph


(* returns a list of possible paths linking s -> t*)
val find_path : int graph -> id -> id -> path


val min_f : int graph -> int -> path -> int 


val aug_f : int graph -> int -> path -> int graph


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
val ford_fulkerson : label graph -> id -> id -> int
