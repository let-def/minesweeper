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

type config =
  { nbcols : int
  ; nbrows : int
  ; nbmines : int
  }

let default_config = { nbcols = 10; nbrows = 10; nbmines = 15 }

type cell =
  { mined : bool
  ; seen : bool
  ; flag : bool
  ; nbm : int
  }

type board = cell array array

let iter_on_cell cf f =
  for i = 0 to cf.nbcols - 1 do
    for j = 0 to cf.nbrows - 1 do
      f (i, j)
    done
  done

let random_list_mines lc m =
  let cell_list = ref [] in
  while List.length !cell_list < m do
    let n = Random.int lc in
    if not (List.mem n !cell_list) then cell_list := n :: !cell_list
  done;
  !cell_list

let generate_seed () =
  let t = Sys.time () in
  let n = int_of_float (t *. 1000.0) in
  Random.init (n mod 100000)

let valid cf (i, j) = i >= 0 && i < cf.nbcols && j >= 0 && j < cf.nbrows

let neighbours cf (x, y) =
  let ngb =
    [ x - 1, y - 1
    ; x - 1, y
    ; x - 1, y + 1
    ; x, y - 1
    ; x, y + 1
    ; x + 1, y - 1
    ; x + 1, y
    ; x + 1, y + 1
    ]
  in
  List.filter (valid cf) ngb

let update v f = Lwd.set v (f (Lwd.peek v))

let initialize_board cf =
  let initial = { mined = false; seen = false; flag = false; nbm = 0 } in
  let copy_cell_init b (i, j) = b.(i).(j) <- Lwd.var initial in
  let set_mined b n =
    update b.(n / cf.nbrows).(n mod cf.nbrows)
      (fun c -> {c with mined = true})
  in
  let count_mined_adj b (i, j) =
    let x = ref 0 in
    let inc_if_mined (i, j) = if (Lwd.peek b.(i).(j)).mined then incr x in
    List.iter inc_if_mined (neighbours cf (i, j));
    !x
  in
  let set_count b (i, j) =
    let cell = b.(i).(j) in
    if not (Lwd.peek cell).mined then
      update cell (fun c -> {c with nbm = count_mined_adj b (i, j)})
  in
  let list_mined = random_list_mines (cf.nbcols * cf.nbrows) cf.nbmines in
  let board =
    Array.init cf.nbcols @@ fun _ ->
    Array.init cf.nbrows @@ fun _ ->
    Lwd.var initial
  in
  iter_on_cell cf (copy_cell_init board);
  List.iter (set_mined board) list_mined;
  iter_on_cell cf (set_count board);
  board

let cells_to_see bd cf (i, j) =
  let visited = Array.make_matrix cf.nbcols cf.nbrows false in
  let rec relevant = function
    | [] -> [], []
    | ((x, y) as c) :: l ->
        let cell = Lwd.peek bd.(x).(y) in
        if cell.mined || cell.flag || cell.seen || visited.(x).(y)
        then relevant l
        else
          let l1, l2 = relevant l in
          visited.(x).(y) <- true;
          if cell.nbm = 0 then l1, c :: l2 else c :: l1, l2
  in
  let rec cells_to_see_rec = function
    | [] -> []
    | ((x, y) as c) :: l ->
        if (Lwd.peek bd.(x).(y)).nbm <> 0
        then c :: cells_to_see_rec l
        else
          let l1, l2 = relevant (neighbours cf c) in
          (c :: l1) @ cells_to_see_rec (l2 @ l)
  in
  visited.(i).(j) <- true;
  cells_to_see_rec [ i, j ]

let b0 = 3

let l1 = 15

let l2 = l1

let l4 = 20 + (2 * b0)

let l3 = (l4 * default_config.nbcols) + (2 * b0)

let l5 = 40 + (2 * b0)

let h1 = l1

let h2 = 30

let h3 = l5 + 20 + (2 * b0)

let h4 = h2

let h5 = 20 + (2 * b0)

let h6 = l5 + (2 * b0)

type demin_cf =
  { bd : cell Lwd.var array array
  ; cf : config
  ; mutable nb_marked_cells : int
  ; mutable nb_hidden_cells : int
  ; mutable flag_switch_on : bool
  }

let cell_image_src cell =
  if cell.flag
  then "sprites/flag.png"
  else if cell.mined
  then "sprites/bomb.png"
  else if cell.seen
  then
    if cell.nbm = 0
    then "sprites/empty.png"
    else "sprites/" ^ string_of_int cell.nbm ^ ".png"
  else "sprites/normal.png"

let cell_image cell ~on_click =
  Lwdom.element "img" @@ Lwd.map' cell @@ fun cell ->
  let attributes =
    Lwdom.singleton (Lwdom.attribute "src" (cell_image_src cell))
  and events =
    Lwdom.singleton (Lwdom.event Dom_events.Typ.click (fun _ _ -> on_click ()))
  and elements =
    Lwdom.empty
  in
  (attributes, elements, events)

let mark_cell d cell =
  let cell' = Lwd.peek cell in
  if cell'.flag
  then (
    d.nb_marked_cells <- d.nb_marked_cells - 1;
    Lwd.set cell {cell' with flag = false})
  else (
    d.nb_marked_cells <- d.nb_marked_cells + 1;
    Lwd.set cell {cell' with flag = true}
  )

let reveal d i j =
  let reveal_cell (i, j) =
    Lwd.set d.bd.(i).(j) {(Lwd.peek d.bd.(i).(j)) with seen = true};
    d.nb_hidden_cells <- d.nb_hidden_cells - 1
  in
  List.iter reveal_cell (cells_to_see d.bd d.cf (i, j));
  if d.nb_hidden_cells = 0
  then (Html.window##alert (js "YOU WIN"))

let create_demin nb_c nb_r nb_m =
  let nbc = max default_config.nbcols nb_c and nbr = max default_config.nbrows nb_r in
  let nbm = min (nbc * nbr) (max 1 nb_m) in
  let cf = { nbcols = nbc; nbrows = nbr; nbmines = nbm } in
  generate_seed ();
  { cf
  ; bd = initialize_board cf
  ; nb_marked_cells = 0
  ; nb_hidden_cells = (cf.nbrows * cf.nbcols) - cf.nbmines
  ; flag_switch_on = false
  }

type mode =
  | Normal
  | Flag

let init_table d =
  let mode = ref Normal in
  (*let buf = document##createDocumentFragment in
    Dom.appendChild buf (document##createTextNode (js "Mode : "));
    let img = Html.createImg document in
    Dom.appendChild buf img;
    img##.src := js "sprites/bomb.png";
    img##.onclick :=
      Html.handler (fun _ ->
          (match !mode with
          | Normal ->
              mode := Flag;
              img##.src := js "sprites/flag.png"
          | Flag ->
              mode := Normal;
              img##.src := js "sprites/bomb.png");
          Js._false);
    Dom.appendChild buf (Html.createBr document);*)
  let table = Lwd_table.make () in
  Array.iteri (fun x col ->
      let tcol = Lwd_table.make () in
      Lwd_table.append' table tcol;
      Array.iteri (fun y cell ->
          Lwd_table.append' tcol @@
          cell_image (Lwd.get cell) ~on_click:(fun () ->
              begin match !mode with
                | Normal ->
                  let cell' = Lwd.peek cell in
                  if cell'.seen
                  then ()
                  else if d.flag_switch_on
                  then mark_cell d cell
                  else if cell'.flag
                  then ()
                  else if cell'.mined
                  then (
                    (*draw_board d; disable_events d;*)
                    Html.window##alert (js "YOU LOSE"))
                  else reveal d x y
                | Flag ->
                  update cell (fun c -> {c with flag = not c.flag})
              end;
              true
            )
        ) col
    ) d.bd;
  let empty_element = Lwd.pure (Lwdom.empty, Lwdom.empty, Lwdom.empty) in
  let collection_monoid = (Lwdom.empty, Lwdom.join) in
  let nodes =
    Lwd_table.map_reduce (fun _ table ->
        let nodes =
          Lwd_table.map_reduce
            (fun _ cell -> Lwd.map Lwdom.singleton cell)
            (Lwd_utils.lift_monoid collection_monoid)
            (table : Lwdom.node Lwd.t Lwd_table.t)
        in
        Lwd.map2'
          (Lwd.join nodes)
          (Lwdom.lsingleton (Lwdom.element "br" empty_element))
          Lwdom.join
      )
      (Lwd_utils.lift_monoid collection_monoid)
      table
    |> Lwd.join
  in
  nodes

let run nbc nbr nbm =
  let d = create_demin nbc nbr nbm in
  init_table d