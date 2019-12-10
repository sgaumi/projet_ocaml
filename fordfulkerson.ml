open Graph
open Tools

(*  Type ratio *)
type label =  {flow : int ; cap : int}

type out_arc = (id * int) 

type arcs = {sr : id ; o_arcs : int out_arcs}

type path = id list

let convert_tolabel gr = gmap gr (function i -> {flow = 0; cap = i}) 

let build_network gr = 
	let graph = gmap gr (function (r:label) -> (r.cap - r.flow))
	in
	let add_arc_inv grr id1 id2 (i:label) = add_arc grr id2 id1 i.flow
	in 
	e_fold gr add_arc_inv graph
	
	
let get_node_list graph = 
    n_fold graph (fun l id -> id::l ) []  


let find_path graph id1 id2 =
    let rec find_nodes (accu:path) (id:id) (l:id list) = match l with
		|[] -> []
		|e :: suite -> match (find_arc graph id e) with
						|None -> find_nodes accu id suite
						|Some x -> if (x == 0) then (find_nodes accu id suite) 
						 else (if(e == id2) then (id2::accu) else (find_nodes (e::accu) e (get_node_list graph)))
	in
	List.rev (find_nodes [id1] id1 (get_node_list graph) )



let rec min_f graph aux path = match path with
    |[] -> aux
	|[eee] -> aux
	|e::ee::rest -> match find_arc graph e ee with
                        |None -> raise Not_found
						|Some 0 -> raise Not_found
                        |Some x -> if(x < aux) then (min_f graph x (ee::rest)) else (min_f graph aux (ee::rest))


let add_flow gr id1 id2 i = 
    match find_arc gr id1 id2 with
    |None -> raise Not_found
    |Some x -> new_arc gr id1 id2 {flow = x.flow + i ; cap = x.cap} 

let aug_f gr1 f1 ch1 =  
	let rec add gr id1 id2 f ch= match ch with
		|[] -> Printf.printf "end of augmentation%!\n";(add_flow gr id1 id2 0);
		|e::[]-> Printf.printf "end of augmentation%!\n"; (add_flow gr id1 id2 0);
		|e1::e2::rest -> if e1 == id1 && e2 == id2 then (Printf.printf "augmenting arc %d %d of %d %!\n" e1 e2 f; add_flow gr e1 e2 f) else (add gr id1 id2 f (e2::rest));
	in
	let add_arc_f gr id1 id2 f = add gr id1 id2 f1 ch1 in
    e_fold gr1 (add_arc_f) gr1
(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)

(*      Il faut trouver un moyen d'augmenter le flow d'un type label graph *)
let rec ford_fulkerson graph id1 id2 flow = 
      let gr_inv = build_network graph in match find_path gr_inv id1 id2 with
            |[]-> Printf.printf "end of ford%!\n";(flow,graph)
            |l -> Printf.printf "appel ford%!\n";(ford_fulkerson (aug_f graph (min_f gr_inv 800 l) l) id1 id2 (flow + (min_f gr_inv 800 l)))

      
      
      
      
      
