open Js_of_ocaml

type attribute = {
  attr_key: Js.js_string Js.t;
  attr_val: Js.js_string Js.t;
}

let attribute k v = {
  attr_key = Js.string k;
  attr_val = Js.string v;
}

type event =
    Event :  {
      typ : (#Js_of_ocaml__.Dom_html.event as 'a) Js.t Dom.Event.typ;
      callback : Dom_html.element Js.t -> 'a Js.t -> bool;
      mutable listener : Dom_events.listener option;
    } -> event

let event typ callback =
  Event { typ; callback; listener = None }

let get_attribute node attr =
  match Js.Opt.to_option (node##getAttribute (Js.string attr)) with
  | None -> ""
  | Some str -> Js.to_string str

type 'a collection = 'a Dset.t

let empty = Dset.empty
let singleton x = Dset.element x
let join c1 c2 = Dset.union c1 c2

let cache_result cache result = (cache := result; result)

let lsingleton lx =
  let cache = ref empty in
  Lwd.map' lx @@ fun x ->
  match Dset.view !cache with
  | Dset.Element x' when x == x' -> !cache
  | _ -> cache_result cache (singleton x)

let ljoin ll lr =
  let cache = ref empty in
  Lwd.map2' ll lr @@ fun l r ->
  match Dset.view !cache with
  | Dset.Union (l', r') when l == l' && r == r' -> !cache
  | _ -> cache_result cache (join l r)

type node = Dom.node Js.t

type state = {
  element: Dom_html.element Js.t;
  mutable attributes: attribute collection;
  mutable events: event collection;
  mutable children: node collection;
}

let remove_left_nodes (parent : node) (marking : node Dset.marking) t =
  let rec aux t =
    match Dset.get_mark marking t with
    | Dset.Right -> assert false
    | Dset.Both -> ()
    | Dset.Left ->
      match Dset.view t with
      | Dset.Empty -> ()
      | Dset.Union (l, r) -> aux r; aux l
      | Dset.Element x ->
        ignore (parent##removeChild x)
  in
  aux t

let insert_right_nodes (parent : node) (marking : node Dset.marking) t =
  let rec left_most acc t =
    match Dset.view t with
    | Dset.Empty -> acc
    | Dset.Union (l, _) -> left_most acc l
    | Dset.Element x -> Js.some x
  in
  let rec aux t acc =
    match Dset.get_mark marking t with
    | Dset.Left -> assert false
    | Dset.Both ->
      left_most acc t
    | Dset.Right ->
      match Dset.view t with
      | Dset.Empty ->
        acc
      | Dset.Union (l, r) ->
        aux l (aux r acc)
      | Dset.Element x ->
        ignore (parent##insertBefore x acc);
        Js.some x
  in
  aux t Js.null

let mk_state tag = {
  element = Dom_html.document##createElement (Js.string tag);
  attributes = empty;
  events = empty;
  children = empty;
}

let update_attributes state ~left ~right =
  state.attributes <- right;
  let diff = Dset.diff ~left ~right in
  List.iter (fun a -> state.element##removeAttribute a.attr_key)
    diff.left_only;
  List.iter (fun a -> state.element##setAttribute a.attr_key a.attr_val)
    diff.right_only

let update_events state ~left ~right =
  state.events <- right;
  let diff = Dset.diff ~left ~right in
  List.iter (fun (Event e) ->
      match e.listener with
      | None -> ()
      | Some listener ->
        e.listener <- None;
        Dom_events.stop_listen listener
    ) diff.left_only;
  List.iter (fun (Event e) ->
      let l = Dom_events.listen state.element e.typ e.callback in
      e.listener <- Some l
    ) diff.right_only

let update_children state ~left ~right =
  state.children <- right;
  let marking = Dset.mark ~left ~right in
  begin try
      let node : Dom_html.element Js.t :> Dom.node Js.t = state.element in
      ignore (insert_right_nodes node marking right : _ Js.opt);
      remove_left_nodes node marking left;
    with exn ->
      Dset.unmark marking;
      raise exn
  end;
  Dset.unmark marking

let element tag collections =
  let state = mk_state tag in
  Lwd.map' collections @@ fun (attributes, children, events) ->
  update_attributes state ~left:state.attributes ~right:attributes;
  update_events state ~left:state.events ~right:events;
  update_children state ~left:state.children ~right:children;
  (state.element : Dom_html.element Js.t :> Dom.node Js.t)

let text txt =
  Lwd.map' txt (fun txt ->
      let node = Dom_html.document##createTextNode (Js.string txt) in
      (node : Dom.text Js.t :> node)
    )
