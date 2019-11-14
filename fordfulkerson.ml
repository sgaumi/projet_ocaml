open Graph
open Tools

(*  Type ratio *)
type label =  {flow : int ; cap : int}

let convert_tolabel gr = gmap gr (function i -> lab = {flow = 0; cap = i}) 

let build_network gr =
    gmap gr (function r:label -> (r.cap - r.flow))

