open Extensions

module type Hashable = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
end

module type T = sig
  type key
  type 'a t

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> unit
  val find_opt : key -> 'a t -> 'a option
  val remove : key -> 'a t -> bool
  val print : 'a t -> unit
  val depth : 'a t -> int
  val size : 'a t -> int
end

module Make (H : Hashable) = struct
  module Types = struct
    type key = H.t

    type 'a t = { root : 'a iNode }
    and gen = < >

    (* The gen field in INodes is only a Kcas.ref so that it can be compared atomically,
       but it is actually treated as immutable. *)
    and 'a iNode = { main : 'a mainNode Kcas.ref; gen : gen Kcas.ref }

    and 'a mainNode =
      | CNode of 'a cNode
      | TNode of 'a leaf
      | LNode of 'a leaf list

    and 'a cNode = { bmp : Int32.t; array : 'a branch array }
    and 'a branch = INode of 'a iNode | Leaf of 'a leaf
    and 'a leaf = { key : key; value : 'a }

    exception Recur
  end

  include Types

  module Operations = struct
    (** Generational Double Compare Single Swap *)
    let gen_dcss inode old_m new_m gen =
      let cas = Kcas.mk_cas inode.main old_m new_m in
      let atomic_read = Kcas.mk_cas inode.gen gen gen in
      Kcas.kCAS [ cas; atomic_read ]
  end

  include Operations

  let value_opt key leaf =
    if H.compare key leaf.key = 0 then Some leaf.value else None

  let create () =
    {
      root =
        {
          main = Kcas.ref @@ CNode { bmp = 0l; array = [||] };
          gen = Kcas.ref (object end);
        };
    }

  (** Only correct in sequential contexts. *)
  let is_empty t =
    match Kcas.get t.root.main with CNode cnode -> cnode.bmp = 0l | _ -> false

  (** NOT ATOMIC. FOR DEBUGGING PURPOSES ONLY *)
  let print t =
    let print_cnode fmt cnode =
      Format.fprintf fmt "{CNode: %s, array length %d}\n%!"
        (Int32.to_string cnode.bmp)
        (Array.length cnode.array)
    in
    let print_other fmt = Format.fprintf fmt "non cnode here??\n%!" in
    let printer fmt t =
      match Kcas.get t.root.main with
      | CNode x -> print_cnode fmt x
      | _ -> print_other fmt
    in
    Format.fprintf Format.std_formatter "%a" printer t

  (** The depth of a tree is the number of INodes.
      It is only correct in sequential contexts. *)
  let depth t =
    let rec aux i =
      match Kcas.get i.main with
      | CNode cnode ->
          Array.fold_left
            (fun acc b ->
              match b with Leaf _ -> acc | INode i -> max acc (1 + aux i))
            1 cnode.array
      | _ -> 1
    in
    aux t.root

  let size t =
    let rec aux i =
      match Kcas.get i.main with
      | CNode cnode ->
          Array.fold_left
            (fun acc b ->
              match b with Leaf _ -> acc + 1 | INode i -> acc + aux i)
            0 cnode.array
      | TNode _ -> 1
      | LNode lst -> List.length lst
    in
    aux t.root

  (** The maximum value for the `lvl` variable.
      This makes the maximum real depth to be 52 (unreachable in practice). *)
  let max_lvl = 256

  (* We only use 5 bits of the hashcode, depending on the level in the tree.
   * Note that `lvl` is always a multiple of 5. (5 = log2 32) *)
  let to_flag key lvl =
    let open Digestif.SHA256 in
    let hash_str =
      key |> H.to_string |> digest_string |> to_hex |> hex_to_binary
    in
    let relevant = String.sub hash_str (String.length hash_str - lvl - 5) 5 in
    let to_shift = int_of_string ("0b" ^ relevant) in
    Int32.shift_left 1l to_shift

  (** `bit` is a single bit flag (never 0)
   *  `pos` is an index in the array, hence it satisfies 0 <= pos <= popcount bitmap *)
  let flagpos key lvl bitmap =
    let flag = to_flag key lvl in
    let pos =
      Ocaml_intrinsics.Int32.count_set_bits
      @@ Int32.logand (Int32.pred flag) bitmap
    in
    (flag, pos)

  let resurrect tombed =
    match Kcas.get tombed.main with
    | TNode leaf -> Leaf leaf
    | _ -> INode tombed

  let contract cnode lvl =
    if lvl > 0 && Array.length cnode.array = 1 then
      match cnode.array.(0) with Leaf leaf -> TNode leaf | _ -> CNode cnode
    else CNode cnode

  let compress cnode lvl =
    let array =
      Array.map
        (function Leaf l -> Leaf l | INode i -> resurrect i)
        cnode.array
    in
    contract { bmp = cnode.bmp; array } lvl

  let clean t lvl startgen =
    match t with
    | None -> ()
    | Some t -> (
        match Kcas.get t.main with
        | CNode cnode as cn ->
            let _ignored =
              (* TODO: it is ignored in the paper, but investigate if that is really wise *)
              gen_dcss t cn (compress cnode lvl) startgen
            in
            ()
        | _ -> ())

  let inserted cnode flag pos l =
    let new_bitmap = Int32.logor cnode.bmp flag in
    let new_array = Array.insert cnode.array pos (Leaf l) in
    { bmp = new_bitmap; array = new_array }

  let updated cnode pos inode =
    let array = Array.copy cnode.array in
    array.(pos) <- inode;
    { cnode with array }

  (** Update the generation of the immediate child cnode.array.(pos) of parent to new_gen.
      We volontarily do not update the generations of deeper INodes, as this is a lazy algorithm.
      TODO: investigate if this is really wise. *)
  let regenerate parent cnode pos mainnode new_gen =
    let new_cnode =
      updated cnode pos
        (INode { main = Kcas.ref mainnode; gen = Kcas.ref new_gen })
    in
    gen_dcss parent (CNode cnode) (CNode new_cnode) new_gen

  let rec find_opt key t =
    let rec aux i key lvl parent startgen =
      match Kcas.get i.main with
      | CNode cnode -> (
          let flag, pos = flagpos key lvl cnode.bmp in
          if Int32.logand cnode.bmp flag = 0l then None
          else
            match cnode.array.(pos) with
            | INode inner ->
                if Kcas.get inner.gen = startgen then
                  aux inner key (lvl + 5) (Some i) startgen
                else if regenerate i cnode pos (Kcas.get inner.main) startgen
                then aux i key lvl parent startgen
                else raise Recur
            | Leaf leaf -> value_opt key leaf)
      | LNode lst -> List.find_map (value_opt key) lst
      | TNode _ ->
          clean parent (lvl - 5) startgen;
          raise Recur
    in
    try aux t.root key 0 None (Kcas.get t.root.gen)
    with Recur -> find_opt key t

  let mem key t = Option.is_some (find_opt key t)

  let rec branch_of_pair l1 l2 lvl gen =
    let flag1 = to_flag l1.key lvl in
    let flag2 = to_flag l2.key lvl in
    let bmp = Int32.logor flag1 flag2 in
    match Int32.unsigned_compare flag1 flag2 with
    | 0 ->
        if lvl > max_lvl then
          (* Maximum depth reached, it's a full hash collision. We just dump everything into a list. *)
          INode { main = Kcas.ref @@ LNode [ l1; l2 ]; gen = Kcas.ref gen }
        else
          (* Collision on this level, we need to go deeper *)
          INode
            {
              main =
                Kcas.ref
                @@ CNode
                     { bmp; array = [| branch_of_pair l1 l2 (lvl + 5) gen |] };
              gen = Kcas.ref gen;
            }
    | 1 ->
        INode
          {
            main = Kcas.ref @@ CNode { bmp; array = [| Leaf l2; Leaf l1 |] };
            gen = Kcas.ref gen;
          }
    | _ ->
        INode
          {
            main = Kcas.ref @@ CNode { bmp; array = [| Leaf l1; Leaf l2 |] };
            gen = Kcas.ref gen;
          }

  let rec add key value t =
    let rec aux i key value lvl parent startgen =
      match Kcas.get i.main with
      | CNode cnode as cn -> (
          let flag, pos = flagpos key lvl cnode.bmp in
          if Int32.logand cnode.bmp flag = 0l then (
            (* no flag collision means it's a free insertion *)
            let new_cnode = inserted cnode flag pos { key; value } in
            if not @@ gen_dcss i cn (CNode new_cnode) startgen then raise Recur)
          else
            (* collision, we need to go a level deeper in the tree *)
            match cnode.array.(pos) with
            | INode inner ->
                if Kcas.get inner.gen = startgen then
                  aux inner key value (lvl + 5) (Some i) startgen
                else if regenerate i cnode pos (Kcas.get inner.main) startgen
                then aux i key value lvl parent startgen
                else raise Recur
            | Leaf l ->
                if H.compare l.key key = 0 then (
                  (* No need to go deeper, just to update the new value *)
                  let new_cnode = updated cnode pos (Leaf { key; value }) in
                  if not @@ gen_dcss i cn (CNode new_cnode) startgen then
                    raise Recur)
                else
                  let new_pair =
                    branch_of_pair l { key; value } (lvl + 5) startgen
                  in
                  let new_cnode = updated cnode pos new_pair in
                  if not @@ gen_dcss i cn (CNode new_cnode) startgen then
                    raise Recur)
      | TNode _ ->
          clean parent (lvl - 5) startgen;
          raise Recur
      | LNode lst as ln ->
          let new_list = LNode ({ key; value } :: lst) in
          if not @@ gen_dcss i ln new_list startgen then raise Recur
    in

    try aux t.root key value 0 None (Kcas.get t.root.gen)
    with Recur -> add key value t

  let rec clean_parent parent t key lvl startgen =
    if Kcas.get t.gen <> startgen then ()
    else
      let main = Kcas.get t.main in
      let p_main = Kcas.get parent.main in
      match p_main with
      | CNode cnode as cn -> (
          let flag, pos = flagpos key lvl cnode.bmp in
          if Int32.logand flag cnode.bmp <> 0l && cnode.array.(pos) = INode t
          then
            match main with
            | TNode _ ->
                let new_cnode = updated cnode pos (resurrect t) in
                if not @@ gen_dcss parent cn (contract new_cnode lvl) startgen
                then clean_parent parent t key lvl startgen
            | _ -> ())
      | _ -> ()

  let removed cnode pos flag =
    let bmp = Int32.logand cnode.bmp (Int32.lognot flag) in
    let array = Array.remove cnode.array pos in
    { bmp; array }

  let rec remove key t =
    let rec aux i key lvl parent startgen =
      match Kcas.get i.main with
      | CNode cnode as cn ->
          let flag, pos = flagpos key lvl cnode.bmp in
          if Int32.logand cnode.bmp flag = 0l then false
          else
            let res =
              match cnode.array.(pos) with
              | INode inner ->
                  if Kcas.get inner.gen = startgen then
                    aux inner key (lvl + 5) (Some i) startgen
                  else if regenerate i cnode pos (Kcas.get inner.main) startgen
                  then aux i key lvl parent startgen
                  else raise Recur
              | Leaf l ->
                  if H.compare l.key key <> 0 then false
                  else
                    let new_cnode = removed cnode pos flag in
                    let contracted = contract new_cnode lvl in
                    gen_dcss i cn contracted startgen || raise Recur
            in
            (if res then
             match (Kcas.get i.main, parent) with
             | TNode _, Some parent ->
                 clean_parent parent i key (lvl - 5) startgen
             (* 'parent = None' means i is the root, and the root cannot have a TNode child. *)
             | _ -> ());
            res
      | TNode _ ->
          clean parent (lvl - 5) startgen;
          raise Recur
      | LNode lst as ln ->
          let new_list, changed =
            List.remove_map (fun leaf -> H.compare leaf.key key = 0) lst
          in
          changed && (gen_dcss i ln (LNode new_list) startgen || raise Recur)
    in
    try aux t.root key 0 None (Kcas.get t.root.gen) with Recur -> remove key t
end