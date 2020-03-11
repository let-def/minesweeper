open Js_of_ocaml

type attribute
val attribute : string -> string -> attribute

type event
val event : (#Dom_html.event Js.t as 'a) Dom.Event.typ ->
  (Dom_html.element Js.t -> 'a -> bool) -> event
val get_attribute : Dom_html.element Js.t -> string -> string

type 'a collection
val empty : 'a collection
val singleton : 'a -> 'a collection
val join : 'a collection -> 'a collection -> 'a collection

val lsingleton : 'a Lwd.t -> 'a collection Lwd.t
val ljoin : 'a collection Lwd.t -> 'a collection Lwd.t -> 'a collection Lwd.t

type node = Dom.node Js.t
val element : string -> (attribute collection * node collection * event collection) Lwd.t -> node Lwd.t
val text : string Lwd.t -> node Lwd.t
