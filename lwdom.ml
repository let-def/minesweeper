module Xml = Tyxml_lwd.Xml
module Svg = Svg_f.Make(Tyxml_lwd.Xml)
module Html = Html_f.Make(Tyxml_lwd.Xml)(Svg)

let elt x = Lwd.pure (Tyxml_lwd.Singleton.inj x)
let attr x : _ Xml.Attr.t = Lwd.pure (Some x)
let lwd_attr x : _ Xml.Attr.t = Lwd.map (fun x -> Some x) x

let dom_node = Tyxml_lwd.Singleton.prj

let child_flatten = function
  | [] -> Xml.Child.nil ()
  | [x] -> x
  | [x; y] -> Lwd.map2 Lwd_seq.concat x y
  | xs -> Lwd_utils.pure_pack Lwd_seq.lwd_monoid xs

let children ls =
  child_flatten (ls : _ Xml.Child.t list :> _ Xml.Child.list list)
