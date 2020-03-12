open Js_of_ocaml

type 'a collection
val empty : 'a collection
val singleton : 'a -> 'a collection
val join : 'a collection -> 'a collection -> 'a collection
val list : 'a collection list -> 'a collection
val lsingleton : 'a Lwd.t -> 'a collection Lwd.t
val ljoin : 'a collection Lwd.t -> 'a collection Lwd.t -> 'a collection Lwd.t

type property
type properties = property collection
val attribute : string -> string -> properties
val event : (#Dom_html.event Js.t as 'a) Dom.Event.typ ->
  (Dom_html.element Js.t -> 'a -> bool) -> properties

type node = Dom.node Js.t
val element : ?properties:properties Lwd.t -> ?children:node collection Lwd.t -> string -> node Lwd.t
val text : string Lwd.t -> node Lwd.t
