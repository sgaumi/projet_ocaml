open Graph
open Gfile



(*     returns a new graph having the same nodes than gr, but no arc     *)
let rec clone_nodes gr = match gr with
    |[]->[]
    |(i,l)::rest -> (i,[])::(clone_nodes rest)

(*     maps all arcs of gr by function f     
let rec gmap gr f = match gr with
     |[] -> []
     |(i,l) :: rest -> (i, List.map (function ( -> )*)


let  gmap gr f =
assert false 


let  add_arc gr f g d =
assert false ;;
