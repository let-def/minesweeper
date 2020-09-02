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

let children' ls = Lwd_utils.pack Lwd_seq.monoid ls

let event_input event =
  let target = Js.Opt.bind event##.target Dom_html.CoerceTo.input in
  match Js.Opt.to_option target with
  | None -> None
  | Some target -> Some (Js.to_string target##.value)

let int_input name value ~set_value =
  let value = Lwd.map string_of_int value in
  children [
    Html.txt (elt name);
    Html.input ~a:[
        Html.a_input_type (attr `Text);
        Html.a_value (lwd_attr value);
        Html.a_onchange (attr (fun event ->
            begin match Option.bind (event_input event) int_of_string_opt with
              | None -> ()
              | Some v -> set_value v
            end;
            true
          ));
      ] ()
  ]

let button name callback =
  Html.input ~a:[
    Html.a_input_type (attr `Submit);
    Html.a_value (attr name);
    Html.a_onclick (attr callback);
  ] ()

let onload _ =
  let main =
    Js.Opt.get (Dom_html.window##.document##getElementById (js "main"))
      (fun () -> assert false)
  in
  let nbr, nbc, nbm = Lwd.var 10, Lwd.var 12, Lwd.var 15 in
  let boards = Lwd_table.make () in
  let root = Html.span @@ children' [
      int_input "Number of columns"
        ~set_value:(fun v -> Lwd.set nbr v; prerr_endline @@ "columns = " ^ string_of_int v)
        (Lwd.get nbr);
      children [Html.br ()];
      int_input "Number of rows" ~set_value:(Lwd.set nbc) (Lwd.get nbc);
      children [Html.br ()];
      int_input "Number of mines" ~set_value:(Lwd.set nbm) (Lwd.get nbm);
      children [
        Html.br ();
        button "nouvelle partie" (fun _ ->
            (*let div = Html.createDiv document in
                Dom.appendChild main div;*)
            Lwd_table.append' boards
              (Minesweeper.run (Lwd.peek nbc) (Lwd.peek nbr) (Lwd.peek nbm));
            false
          );
      ];
      Lwd.join (Lwd_table.reduce Lwd_seq.lwd_monoid boards);
    ]
  in
  let root = Lwd.observe root in
  Lwd.set_on_invalidate root (fun _ ->
      ignore (Dom_html.window##requestAnimationFrame
                (Js.wrap_callback (fun _ -> ignore (Lwd.quick_sample root)))
             ));
  Dom.appendChild main (dom_node (Lwd.quick_sample root));
  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload
