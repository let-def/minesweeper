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

let empty_element = Lwd.pure (Lwdom.empty, Lwdom.empty, Lwdom.empty)

let of_list ls =
  List.fold_left Lwdom.join Lwdom.empty ls

let lof_list ls =
  List.fold_left Lwdom.ljoin (Lwd.pure Lwdom.empty) ls

let int_input name value ~set_value =
  let value = Lwd.map string_of_int value in
  let name = Lwdom.text (Lwd.pure name) in
  let input =
    let attributes =
      Lwdom.ljoin
        (Lwdom.lsingleton (Lwd.pure (Lwdom.attribute "type" "text") ))
        (Lwdom.lsingleton (Lwd.map (Lwdom.attribute "value") value ))
    in
    let events =
      Lwdom.event Dom_events.Typ.change
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
      |> Lwdom.singleton
    in
    Lwdom.element "input"
      (Lwd.map' attributes (fun attr -> attr, Lwdom.empty, events))
  in
  Lwdom.ljoin (Lwdom.lsingleton name) (Lwdom.lsingleton input)

let button name callback =
  let attributes = of_list [
      Lwdom.singleton @@ Lwdom.attribute "type" "submit";
      Lwdom.singleton @@ Lwdom.attribute "value" name;
    ]
  and events = of_list [
    Lwdom.singleton @@ Lwdom.event Dom_events.Typ.click callback
  ]
  in
  Lwdom.element "input" (Lwd.pure (attributes, Lwdom.empty, events))

let onload _ =
  let main = Js.Opt.get (document##getElementById (js "main")) (fun () -> assert false) in
  let nbr, nbc, nbm = Lwd.var 10, Lwd.var 12, Lwd.var 15 in
  let boards = Lwd_table.make () in
  let elements = lof_list [
      int_input "Number of columns" ~set_value:(fun v -> Lwd.set nbr v; prerr_endline @@ "columns = " ^ string_of_int v) (Lwd.get nbr);
      Lwd.map Lwdom.singleton (Lwdom.element "br" empty_element);
      int_input "Number of rows" ~set_value:(Lwd.set nbc) (Lwd.get nbc);
      Lwd.map Lwdom.singleton (Lwdom.element "br" empty_element);
      int_input "Number of mines" ~set_value:(Lwd.set nbm) (Lwd.get nbm);
      Lwd.map Lwdom.singleton (Lwdom.element "br" empty_element);
      Lwd.map Lwdom.singleton @@ button "new game" (fun _ _ ->
          (*let div = Html.createDiv document in
          Dom.appendChild main div;*)
          Lwd_table.append' boards
            (Minesweeper.run (Lwd.peek nbc) (Lwd.peek nbr) (Lwd.peek nbm));
          false
        );
    ] in
  let root =
    let (>>=) = Lwd.bind in
    let div_board =
      Lwd.join (Lwd_table.reduce
        (Lwd_utils.lift_monoid (Lwdom.empty, Lwdom.join)) boards)
      >>= fun boards ->
      Lwdom.element "div" @@ Lwd.return (Lwdom.empty, boards, Lwdom.empty)
    in
    Lwd.map2' elements div_board
      (fun elts div_board ->
         Lwdom.element "div" @@
           Lwd.return
           (Lwdom.empty, Lwdom.join elts (Lwdom.singleton div_board), Lwdom.empty)
      )
      |> Lwd.join
  in
  let root = Lwd.observe root in
  Lwd.set_on_invalidate root (fun _ ->
      ignore (Html.window##requestAnimationFrame
                (Js.wrap_callback (fun _ -> ignore (Lwd.sample root)))
             ));
  Dom.appendChild main (Lwd.sample root);
  Js._false

let _ = Html.window##.onload := Html.handler onload
