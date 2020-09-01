(* Js_of_ocaml examples
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2008 Benjamin Canou
 *
 *           DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
 *  TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
 *
 *)
open Js_of_ocaml
open Lwdom

let js = Js.string

let document = Dom_html.window##.document

(*let of_list ls = Lwd_utils.pack Lwd_seq.monoid ls
let lof_list ls = Lwd_utils.pack Lwd_seq.lwd_monoid ls*)

let children ls =
  let ls = (ls : _ Html.elt list :> _ Html.children list) in
  Lwd_utils.pack Lwd_seq.monoid ls

let singleton x = (x : _ Html.elt :> _ Html.children)

let int_input name value ~set_value =
  let value = Lwd.map string_of_int value in
  Lwd.map2 Lwd_seq.concat
    (Html.Xml.Child.singleton (Html.txt (Lwd.pure (Tyxml_lwd.Singleton.inj name))))
    (Html.Xml.Child.singleton (Html.input ~a:[
        Html.a_input_type (Lwd.pure (Some `Text));
        Html.a_value (Lwd.map (fun x -> Some x) value);
        Html.a_onchange (Lwd.pure (Some (fun event ->
            let element = Dom_html.eventTarget event in
            let element : Dom_html.inputElement Js.t = Obj.magic element in
            let value = Js.to_string element##.value in
            prerr_endline @@ "change " ^ value;
            begin match int_of_string_opt value with
              | None -> ()
              | Some v -> set_value v
            end;
            true
          )));
      ] ()))

let button name callback =
  Html.input ~a:[
    Html.a_input_type (Lwd.pure (Some `Submit));
    Html.a_value (Lwd.pure (Some name));
    Html.a_onclick (Lwd.pure (Some callback));
  ] ()

let onload _ =
  let main = Js.Opt.get (document##getElementById (js "main")) (fun () -> assert false) in
  let nbr, nbc, nbm = Lwd.var 10, Lwd.var 12, Lwd.var 15 in
  let boards = Lwd_table.make () in
  let elements = Lwd_utils.pack Lwd_seq.monoid [
      int_input "Number of columns"
        ~set_value:(fun v -> Lwd.set nbr v; prerr_endline @@ "columns = " ^ string_of_int v)
        (Lwd.get nbr);
      singleton (Html.br ());
      int_input "Number of rows" ~set_value:(Lwd.set nbc) (Lwd.get nbc);
      singleton (Html.br ());
      int_input "Number of mines" ~set_value:(Lwd.set nbm) (Lwd.get nbm);
      singleton (Html.br ());
      singleton (button "nouvelle partie" (fun _ ->
          (*let div = Html.createDiv document in
            Dom.appendChild main div;*)
          Lwd_table.append' boards
            (Minesweeper.run (Lwd.peek nbc) (Lwd.peek nbr) (Lwd.peek nbm));
          false
        ))
    ] in
  let root =
    Html.span
      (Lwd.map2 Lwd_seq.concat
         elements
         (Lwd.join (Lwd_table.reduce Lwd_seq.lwd_monoid boards)))
  in
  let root = Lwd.observe root in
  Lwd.set_on_invalidate root (fun _ ->
      ignore (Dom_html.window##requestAnimationFrame
                (Js.wrap_callback (fun _ -> ignore (Lwd.quick_sample root)))
             ));
  Dom.appendChild main
    (Obj.magic (Tyxml_lwd.Singleton.prj (Lwd.quick_sample root)));
  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload
