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


(*  Trouve un chemin vers id2 mais pas a partir de id1*)
let find_path graph id1 id2 =
     let rec find_nodes (accu:path) (id:id) (l:id list) = match l with
		|[] -> accu
		|e :: suite -> match (find_arc graph e id) with
						|None -> find_nodes accu id suite
						|Some x -> if (x == 0) then (find_nodes accu id suite) 
						    else ( 
						    if(e == id1) then (id1::accu) else (find_nodes (e::accu) e (get_node_list graph)))
	in
	find_nodes [id2] id2 (get_node_list graph) 


  (*
let find_path graph id1 id2 =
(*    let get_node_list gr = 
        n_fold gr (fun l id -> id::l  ) []
    in
        (*liste de chaque noeuds avec ses arcs sortants (et noeuds voisins)*)
    let list_arcs = List.map (fun id -> {sr = id ; o_arcs = out_arcs graph id } ) (get_node_list graph) 
    in
        (*cherche si un id est dans une liste d'out_arc et renvoie le couple (id*flot) *)
    let rec find_in id l = match l with
        |(e,x) :: rest -> if(e == id && x != 0) then (e,x) else (find_in id rest)
        |[]  -> (-1,-1)
    in
    let rec aux accu tarte l idd = match l with 
        |e1 :: rest -> if (find_in idd e1.o_arcs) != (-1,-1) 
                       then (if(e1.sr != id1)
                                 then aux ((find_in idd e1.o_arcs) :: accu) (e1::tarte) tarte e1.sr
                                 else ((find_in idd e1.o_arcs) :: accu))
                       else aux accu (e1::tarte) rest idd
        |[] -> []
    in
    aux [] [] list_arcs id2
    
     
*)
*)

    (*  assert false  *) 

(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)

(*      Il faut trouver un moyen d'augmenter le flow d'un type label graph *)
let ford_fulkerson graph id1 id2 = 
        let chem = find_path (build_network graph) id1 id2
        in
        let rec min_f (aux:int) (ch:path) = match ch with
            |[] -> aux
        (*    |[a,b] -> match find_arc graph a b with
                      |None -> aux
                      |Some y -> if(y < aux) then (min_f y rest) else (min_f aux rest) *)
            |e::rest -> match find_arc graph e (List.hd rest) with
                        |None -> aux
                        |Some x -> if(x.flow < aux) then (min_f x.flow rest) else (min_f aux rest)
        in
        let rec aug_f gr1 f1 ch1 = match ch1 with 
            |[] -> empty_graph
			|e1::suite -> aug_f (add_arc gr1 e1 (List.hd suite) f1) f1 suite
        in
        let rec ford_ful gr f = match find_path (build_network (gr)) id1 id2 with
            |[] -> f
            |chemin -> ford_ful (aug_f gr (min_f 0 chemin) chemin) (f+(min_f 0 chemin)) 
        in
        ford_ful graph 0

