type vertex = unit ref
type edge = vertex * vertex
type graph =
  {
    vertices : vertex list;
    edges : edge list;
  }

let x = ref ()
let y = ref ()
let f = (x,x)
let g = (x,y)
let h = (x,x)

type lvertex = vertex ref
type ledge = lvertex * edge * lvertex
type lgraph =
  {
    lvertices : lvertex list;
    ledges : ledge list;
  }
let x0 = ref x
let x1 = ref x
let x2 = ref x
let y3 = ref y
let f4 = (x0,f,x1)
let f5 = (x1,f,x2)
let g6 = (x2,g,y3)
let h = { lvertices = [x0;x1;x2;y3]; ledges = [f4;f5;g6] }

type morphism =
  | Generator of generator
  | Composition of morphism * int * morphism
  | Identity of morphism
