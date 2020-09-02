open Js_of_ocaml
open Tyxml_lwd

module Xml = Xml
module Svg : Svg_sigs.Make(Xml).T
module Html : Html_sigs.Make(Xml)(Svg).T

val elt : 'a -> 'a Xml.Elt.t
val children : 'a Xml.Elt.t list -> 'a Xml.Child.list
val child_flatten : 'a Xml.Child.list list -> 'a Xml.Child.list
val attr : 'a -> 'a Xml.Attr.t
val lwd_attr : 'a Lwd.t -> 'a Xml.Attr.t
val dom_node : 'a Html.data Singleton.t -> Dom.node Js.t
