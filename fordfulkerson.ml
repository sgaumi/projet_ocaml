open Graph
open Tools

(*  Type ratio *)
type label =  {flow : int ; cap : int}

type out_arc = (id * int) 

type arcs = {sr : id ; o_arcs : int out_arcs}

let convert_tolabel gr = gmap gr (function i -> {flow = 0; cap = i}) 

let build_network gr = 
	let graph = gmap gr (function (r:label) -> (r.cap - r.flow))
	in
	let add_arc_inv grr id1 id2 (i:label) = add_arc grr id2 id1 i.flow
	in 
	e_fold gr add_arc_inv graph
	
(* *
let find_path graph id1 id2 =
    let get_node_list = 
        n_fold graph (function l id -> id::l  ) []
     in
     let rec find_arcs list_accu =
        |id2 :: _ -> list_accu (* stop/trouvé*)
        |e1 :: e2 :: rest -> match find_arc graph e1 e2 with
                                |None -> find_arcs list_accu (e1 :: rest)  
                                |Some x -> find_arcs ({src = e1 ; dest = e2 ; value = x } :: list_accu) (e2 :: rest)
                                
        |e::[] -> find_arcs 

**)
  (*  assert false  *) 
  
let find_path graph id1 id2 =
    let get_node_list gr = 
        n_fold gr (fun l id -> id::l  ) []
     in
     let list_arcs = List.map (fun id -> {sr = id ; o_arcs = out_arcs graph id } ) (get_node_list graph) 
     in
     let rec find_in id l = match l with
        |(e,x) :: rest -> if(e == id && x != 0) then (e,x) else (find_in id rest)
        |[]  -> (-1,-1)
    in
    let rec aux accu l = match l with 
        |e1 :: rest -> if (find_in id2 e1.o_arcs) != (-1,-1) 
                       then if(e1.sr != id1)
                                 then aux ((find_in id2 e1.o_arcs) :: accu) rest 
                                 else ((find_in id2 e1.o_arcs) :: accu)
                       else aux accu rest
        |[] -> []
    in
    aux [] list_arcs
    
     

    (*  assert false  *) 

(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
let ford_fulkerson graph id1 id2 = assert false
