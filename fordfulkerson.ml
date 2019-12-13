open Graph
open Tools
open Gfile

(*  Type label & path *)
type label =  {flow : int ; cap : int}

type path = id list

(*  converts an int graph into a label graph *)
let convert_tolabel gr = gmap gr (function i -> {flow = 0; cap = i}) 

(*  returns the reversed graph *)
let build_network gr = 
	let graph = gmap gr (function (r:label) -> (r.cap - r.flow))
	in
	let add_arc_inv grr id1 id2 (i:label) = add_arc grr id2 id1 i.flow
	in 
	e_fold gr add_arc_inv graph
	
(*  prints a path in string *)
let rec affichage l = match l with
        |a :: rest -> (string_of_int a) ^"->"^ (affichage rest)
        |[] -> ""     

(* returns a path linking s -> t*)
let find_path graph id1 id2 =
	(* find the different nodes contained in the path from the current node id *)    
	let rec find_nodes (accu:path) (id:id) (l: int out_arcs) =
	    if List.mem id accu then [] 
	    else (
	        match l with
			    |[] -> []
			    |(e,v) :: suite -> if v = 0 then (find_nodes accu id suite) 
							       else ( 
							        if e = id2 then (id2::id::accu) else (
							                                    let result = (find_nodes (id::accu) e (out_arcs graph e)) in
					                                            if result = [] 
					                                            then find_nodes accu id suite
					                                            else result
					                                            )                         
				                )
		)
	in
	List.rev (find_nodes [] id1 (out_arcs graph id1) )


(*	finds the minimum *)
let rec min_f graph aux path = match path with
    |[] -> aux
	|[eee] -> aux
	|e::ee::rest -> match find_arc graph e ee with
                        |None -> raise Not_found
						|Some 0 -> raise Not_found
                        |Some x -> if(x < aux) then (min_f graph x (ee::rest)) else (min_f graph aux (ee::rest))


(* adds arc function applied to label graphs *)
let add_flow gr id1 id2 i = 
    match find_arc gr id1 id2 with
    |None -> raise Not_found
    |Some x -> new_arc gr id1 id2 {flow = x.flow + i ; cap = x.cap}
    
     
(* updates the current flow of every arc in the path *)
let aug_f gr1 f1 ch1 =  
	let rec add gr id1 id2 f ch= match ch with
		|[] -> gr
		|e::[]-> gr
		|e1::e2::rest -> if e1 == id1 && e2 == id2 then ( add_flow gr e1 e2 f) else (add gr id1 id2 f (e2::rest))
	in
	(* increases flow on forward arcs *)
	let add_arc_f gr id1 id2 f = add gr id1 id2 f1 ch1 in

    let grrr = e_fold gr1 (add_arc_f) gr1 in
    
    let rec sub gr id1 id2 f ch= match ch with
		|[] -> gr
		|e::[]-> gr
		|e1::e2::rest -> if e1 == id2 && e2 == id1 then ( add_flow gr e2 e1 (-f)) else (sub gr id1 id2 f (e2::rest));
	in
	(* decreases flow on reversed arcs*)
	let sub_arc_f gr id1 id2 f = sub gr id1 id2 f1 ch1 in
    e_fold grrr (sub_arc_f) grrr


(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)
let rec ford_fulkerson graph id1 id2 flow = 
      let gr_inv = build_network graph in match find_path gr_inv id1 id2 with
            |[]-> (flow,graph)
            |l -> (ford_fulkerson (aug_f graph (min_f gr_inv 800 l) l) id1 id2 (flow + (min_f gr_inv 800 l)))

      
