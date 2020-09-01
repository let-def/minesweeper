module Xml = Tyxml_lwd.Xml
module Svg = Svg_f.Make(Tyxml_lwd.Xml)
module Html = Html_f.Make(Tyxml_lwd.Xml)(Svg)
