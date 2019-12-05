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
	

let find_path graph id1 id2 =
    let get_node_list = 
        n_fold graph (function l id -> id::l ) []
     in
     let rec find_nodes accu id l = match l with
		|[] -> accu
		|e :: suite -> match find_arc graph e id with
						|None -> find_node accu id suite
						|Some x -> if (x == 0) then (find_node accu id suite) else (find_node (accu::e) e get_node_list)
	in
	find_nodes [id2] id1 get_node_list

  (*  assert false  *) 
  
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

    (*  assert false  *) 

(* runs the ford fulkerson algorithm on a graph and returns its maximum flow*)

let ford_fulkerson graph id1 id2 = assert false (*
        let chem = find_path (build_network graph) id1 id2
        in
        let min_f aux ch = match ch with
            |[] -> aux
            |(e,f)::rest -> if(f < aux) then (min_f e rest) else (min_f aux rest)
        in
        let rec aug_f gr1 f1 ch1 = match ch1 with 
            |[] -> []
			|e::e2::suite -> aug_f (add_arc gr1 e e2 f1) f1 suite
        in
        let rec ford_ful gr f = match find_path (build_network (gr)) id1 id2 with
            |[] -> f
            |chemin -> ford_ful (aug_f gr (min_f 0 chemin) chemin) (f+(min_f 0 chemin)) 
        in
        ford_ful graph 0

*)


