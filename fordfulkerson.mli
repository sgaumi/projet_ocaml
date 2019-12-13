open Graph
open Tools

(*  Type label & path *)
type label =  {flow : int ; cap : int}

type path = id list

(*  converts an int graph into a label graph *)
val convert_tolabel : int graph -> label graph


(*  returns the reversed graph *)
val build_network : label graph -> int graph


(* returns a path linking s -> t*)
val find_path : int graph -> id -> id -> path


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
val ford_fulkerson : label graph -> id -> id -> int -> (int*label graph)
