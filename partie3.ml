type 'a arbre = Node of int * 'a * 'a foret
and 'a foret = 'a arbre list ;;


let union a1 a2 =
  match a1, a2 with
  | Node (k, a, b), Node (k1, c, d) when k=k1-> 
      if a < c then Node (k + 1, a, a2 :: b)
      else Node (k + 1, c, a1 :: d)
  | _,_ -> assert false ;;

let rec est_binomial k (Node(ordre,a,b))=
  let rec aux l p=
    match l with
    |[] -> p = -1
    |t::q -> est_binomial p t  && aux q (p-1)
  in
  if ordre <> k then false
  else aux b (k-1);;
      

let arbre = Node(2, "A", [
    Node(1, "B", [Node(0, "C", [])]);
    Node(0, "D", [])
  ]);;

est_binomial 2 arbre