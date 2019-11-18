open Graph
open Tools

(*  Type ratio *)
type label =  {flow : int ; cap : int}

let convert_tolabel gr = gmap gr (function i -> {flow = 0; cap = i}) 

let build_network gr = 
	let graph = gmap gr (function (r:label) -> (r.cap - r.flow))
	in
	let add_arc_inv grr id1 id2 (i:label) = add_arc grr id2 id1 i.flow
	in 
	e_fold gr add_arc_inv graph
	

let find_path graph id1 id2 =
(*    let get_node_list = 
        n_fold graph (function l id -> id::l  ) []
     in
     let rec find_arcs list_accu =
        |id2 :: _ -> list_accu (* stop/trouvé*)
        | ->
*)

    assert false 

(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
let ford_fulkerson graph id1 id2 = assert false
