(* Js_of_ocaml examples
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2008 Benjamin Canou
 *
 *           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 *  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 *
 *)
open Js_of_ocaml
module Html = Dom_html

let js = Js.string

let document = Html.window##.document

let of_list ls =
  List.fold_left Lwdom.join Lwdom.empty ls

let lof_list ls =
  List.fold_left Lwdom.ljoin (Lwd.pure Lwdom.empty) ls

let int_input name value ~set_value =
  let value = Lwd.map string_of_int value in
  let name = Lwdom.text (Lwd.pure name) in
  let input = Lwdom.element "input" ~properties:(lof_list [
      Lwd.pure (Lwdom.attribute "type" "text");
      Lwd.map (Lwdom.attribute "value") value;
      Lwd.pure @@ Lwdom.event Dom_events.Typ.change
        (fun element _event ->
           let element : Html.inputElement Js.t = Obj.magic element in
           let value = Js.to_string element##.value in
           prerr_endline @@ "change " ^ value;
           begin match int_of_string_opt value with
             | None -> ()
             | Some v -> set_value v
           end;
           true
        )
    ]
    )
  in
  Lwdom.ljoin (Lwdom.lsingleton name) (Lwdom.lsingleton input)

let button name callback =
  Lwdom.element "input" ~properties:(Lwd.pure @@ of_list [
      Lwdom.attribute "type" "submit";
      Lwdom.attribute "value" name;
      Lwdom.event Dom_events.Typ.click callback
    ])

let onload _ =
  let main = Js.Opt.get (document##getElementById (js "main")) (fun () -> assert false) in
  let nbr, nbc, nbm = Lwd.var 10, Lwd.var 12, Lwd.var 15 in
  let boards = Lwd_table.make () in
  let elements = lof_list [
      int_input "Number of columns" ~set_value:(fun v -> Lwd.set nbr v; prerr_endline @@ "columns = " ^ string_of_int v) (Lwd.get nbr);
      Lwd.map Lwdom.singleton (Lwdom.element "br");
      int_input "Number of rows" ~set_value:(Lwd.set nbc) (Lwd.get nbc);
      Lwd.map Lwdom.singleton (Lwdom.element "br");
      int_input "Number of mines" ~set_value:(Lwd.set nbm) (Lwd.get nbm);
      Lwd.map Lwdom.singleton (Lwdom.element "br");
      Lwd.map Lwdom.singleton @@ button "nouvelle partie" (fun _ _ ->
          (*let div = Html.createDiv document in
          Dom.appendChild main div;*)
          Lwd_table.append' boards
            (Minesweeper.run (Lwd.peek nbc) (Lwd.peek nbr) (Lwd.peek nbm));
          false
        )
    ] in
  let root = Lwdom.element "span"
      ~children:
        (Lwd.map2 Lwdom.join
           elements
           (Lwd.join
              (Lwd_table.reduce
                 (Lwd_utils.lift_monoid (Lwdom.empty, Lwdom.join)) boards)))
  in
  let root = Lwd.observe root in
  Lwd.set_on_invalidate root (fun _ ->
      ignore (Html.window##requestAnimationFrame
                (Js.wrap_callback (fun _ -> ignore (Lwd.sample root)))
             ));
  Dom.appendChild main (Lwd.sample root);
  Js._false

let _ = Html.window##.onload := Html.handler onload
