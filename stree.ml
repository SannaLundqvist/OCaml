(* fil: stree.mli *)

(*
 * Här är gränssnittet för en implementation av Binärt sökträd
 * 
 * Vi abstraherar ut jämförelseoperationen compare till
 * en egen modul som vi skickar in i denna modul.
 *
 *)

module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type T =
sig
  type 'a t
  exception ErrorTree of string
  val empty: 'a t
  val is_empty: 'a t -> bool
  val add: 'a -> 'a t -> 'a t
  val remove: 'a -> 'a t -> 'a t
  val cardinal: 'a t -> int
  val exists: ('a -> bool) -> 'a t -> bool
  val filter: ('a -> bool) -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map: ('a -> 'b) -> 'a t -> 'b t
end

module Make(Ord: OrderedType): T =  struct
  type 'a t =
    | Empty
    | Node of ('a t * 'a * 'a t )

  (* let is_empty tree = (Empty = tree) *)
  exception ErrorTree of string
  let empty = Empty

  let is_empty =
     function
    | Empty -> true
    | Node _ -> false

  (* funktion för att addera alla emlement i ett träd obs! kan inte skriva match-sträck *)
  let rec functionen (t : int t) : int =
    match t with
    | Empty -> 0
    | Node (lt, data, rt) -> -(functionen lt) + data + functionen rt

  (* abstahera ut saker för att inte duplicera kod *)
  let rec fold f t basfall =
    match t with
    | Empty -> basfall
   (* | Node (lt, data, rt) -> (f data (fold f lt ( fold f rt basfall )))*)
    | Node (lt, data, rt) -> fold f rt basfall |> fold f lt |> f data

  let rec add el tree =
    match tree with
    | Empty -> Node(Empty, el, Empty)
    | Node (vt, d, ht) -> if el > d then
        Node (vt, d, (add el ht))
      else
        Node ((add el vt), d, ht)

  let rec min tree=
    match tree with
    | Empty -> raise (ErrorTree "Not valid tree")
    | Node (vt, d, ht) ->
      if vt = Empty then d
      else min vt

  let rec merge lt gt =
    match lt with
    | Empty -> gt
    | Node(l, d, g) -> Node (l, d, merge g gt)

  let rec remove el tree =
    match tree with
    | Empty -> Empty
    | Node (vt, d, ht) -> if d = el then
        remove el (merge vt ht)
      else if  d > el then
        remove el vt
      else
        remove el ht

  let rec exists f tree =
    match tree with
    | Empty -> false
    | Node (vt, d, ht) -> exists f vt || f d || exists f ht

  let cardinal tree =
    match tree with
    | Empty -> 0
    | Node (vt, _, ht) ->
      if vt = Empty && ht = Empty then 0
        else if vt = Empty || ht = Empty then 1
          else 2

  let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node (vt, d, ht) -> Node((map f vt), f d, (map f ht))

  let rec iter f tree =
    match tree with
    | Empty -> ()
    | Node (vt, d, ht) ->
      ignore (f d; iter f vt; iter f ht)

  let rec filter f tree =
    match tree with
    | Empty -> Empty
    | Node (vt, d, ht) -> if (f d) then
       filter f (Node(vt, d, ht))
      else
       filter f (merge vt ht)

end
