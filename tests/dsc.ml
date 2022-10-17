module Domain = Dscheck.TracedAtomic
module Test = Dscheck.TracedAtomic

(* BUG: a copy should not update its parent!
   - Only happens with collisions? (the `add 1` and `2` are required to trigger)
   - `remove` on the copy also breaks the parent

   module P = Paradict.Make (struct
     type t = int
     let equal = Int.equal
     let hash _ = 0
   end)

   let test () =
     let t = P.create () in
     P.add 1 () t;
     P.add 2 () t;
     let t' = P.copy t in
     P.add 3 () t';
     assert (not (P.mem 3 t))

   let () = Test.trace test
*)

module P = Paradict.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

let test () =
  let t = P.create () in
  for i = 0 to 5 do
    P.add i () t
  done;
  Domain.spawn (fun () -> assert (P.mem 3 t));
  let t' = P.copy t in
  P.remove 3 t';
  Test.final (fun () ->
      Test.check (fun () -> P.mem 3 t);
      Test.check (fun () -> not (P.mem 3 t')))

let () = Test.trace test

(* Too many possibilities to test exhaustively:

   let test () =
     let t = P.create () in
     Domain.spawn (fun () -> P.add 5 () t);
     Domain.spawn (fun () -> P.add 5 () t);
     Test.final (fun () -> Test.check (fun () -> P.mem 5 t))
*)

(* Infinite run in kCAS when add/copy interleave?

   let test () =
     let t = P.create () in
     Domain.spawn (fun () -> P.add 3 () t);
     let _ = P.copy t in
     Test.final (fun () -> Test.check (fun () -> false)) (* should crash *)
*)
