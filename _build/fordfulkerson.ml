open Graph
open Tools

(*  Type ratio *)
type label =  {flow : int ; cap : int}

let convert_tolabel gr = gmap gr (function i -> {flow = 0; cap = i}) 

let build_network gr = 
	let graph = gmap gr (function (r:label) -> (r.cap - r.flow))
	in
	(*let recup_flow id1 id2 gra = match find_arc id1 id2 gra with
									|Some x -> x.flow
									|None -> 0 
	in*)
	let add_arc_inv grr id1 id2 (i:label) = new_arc grr id2 id1 i.flow
	in 
	e_fold gr add_arc_inv graph
	
	
let find_path graph id1 id2 = assert false


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
let ford_fulkerson graph id1 id2 = assert false
