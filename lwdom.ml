open Js_of_ocaml

type 'a collection = 'a Dset.t

let empty = Dset.empty
let singleton x = Dset.element x
let join c1 c2 = Dset.union c1 c2
let list l =
  List.fold_left join empty l

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

type property =
  | Attribute of {
      key: Js.js_string Js.t;
      value: Js.js_string Js.t;
    }
  | Event :  {
      typ : (#Js_of_ocaml__.Dom_html.event as 'a) Js.t Dom.Event.typ;
      callback : Dom_html.element Js.t -> 'a Js.t -> bool;
      mutable listener : Dom_events.listener option;
    } -> property

type properties = property collection

let attribute k v =
  singleton (Attribute { key = Js.string k; value = Js.string v })

let event typ callback =
  singleton (Event { typ; callback; listener = None })

type node = Dom.node Js.t

type state = {
  element: Dom_html.element Js.t;
  mutable properties: property collection;
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

let update_properties state right =
  let left = state.properties in
  state.properties <- right;
  let diff = Dset.diff ~left ~right in
  List.iter (function
      | Attribute a ->
        Js.Unsafe.delete state.element a.key
      | Event { listener = None; _ } -> ()
      | Event ({ listener = Some listener; _ } as e) ->
        e.listener <- None;
        Dom_events.stop_listen listener
    ) diff.left_only;
  List.iter (function
      | Attribute a -> Js.Unsafe.set state.element a.key a.value
      | Event e ->
        e.listener <- Some (Dom_events.listen state.element e.typ e.callback)
    ) diff.right_only

let update_children state right =
  let left = state.children in
  state.children <- right;
  let marking = Dset.mark ~left ~right in
  begin try
      let node : Dom_html.element Js.t :> Dom.node Js.t = state.element in
      remove_left_nodes node marking left;
      ignore (insert_right_nodes node marking right : _ Js.opt);
    with exn ->
      Dset.unmark marking;
      raise exn
  end;
  Dset.unmark marking

let mk_state tag = {
  element = Dom_html.document##createElement (Js.string tag);
  properties = empty;
  children = empty;
}

let element_as_node x = (x : Dom_html.element Js.t :> Dom.node Js.t)

let element ?properties ?children tag =
  let state = mk_state tag in
  match properties, children with
  | None, None -> Lwd.pure (element_as_node state.element)
  | Some props, None ->
    Lwd.map' props (fun props ->
        update_properties state props;
        element_as_node state.element
      )
  | None, Some children ->
    Lwd.map' children (fun children ->
        update_children state children;
        element_as_node state.element
      )
  | Some props, Some children ->
    Lwd.map2' props children (fun props children ->
        update_properties state props;
        update_children state children;
        element_as_node state.element
      )

let text txt =
  Lwd.map' txt (fun txt ->
      (Dom_html.document##createTextNode (Js.string txt) :> node)
    )
