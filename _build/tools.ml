open Graph
open Gfile



(*     returns a new graph having the same nodes than gr, but no arc     *)
let clone_nodes gr = n_fold gr new_node empty_graph 
(*     maps all arcs of gr by function f   
        gmap: 'a graph -> ('a -> 'b) -> 'b graph

let rec gmap gr f = let new_graph = clone_nodes gr in
    let new_arc_2 graph id_1 id_2 a = new_arc graph id_1 id_2 (f a) in  
    e_fold gr new_arc_2 new_graph 
*)  



let  gmap gr f =
assert false 


let  add_arc gr f g d =
assert false 
