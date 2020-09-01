module Xml = Tyxml_lwd.Xml
module Svg : Svg_sigs.Make(Xml).T
module Html : Html_sigs.Make(Xml)(Svg).T
