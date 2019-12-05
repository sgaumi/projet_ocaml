open Graph
open Gfile



(*     returns a new graph having the same nodes than gr, but no arc     *)

let clone_nodes gr = n_fold gr new_node empty_graph 


(*     maps all arcs of gr by function f   
        gmap: 'a graph -> ('a -> 'b) -> 'b graph  *)

let gmap gr f = 
    let new_arc_2 graph id_1 id_2 a = new_arc graph id_1 id_2 (f a) in  
    e_fold gr new_arc_2 (clone_nodes gr) 
 

(*     adds n to the value of the arc between id1 and id2. If the arc does not exist, it is created.     *)

let add_arc gr id1 id2 i = 
    match find_arc gr id1 id2 with
    |None -> new_arc gr id1 id2 i
    |Some x -> new_arc gr id1 id2 (x + i) 
