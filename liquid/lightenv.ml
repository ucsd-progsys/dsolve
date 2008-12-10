(*
 * Copyright © 2008 The Regents of the University of California. All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
 * FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 * ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN
 * IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION
 * TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 *)

(* A small environment module; provided so we (hopefully) won't have to screw
   with OCaml's env. *)

module M = Map.Make(Common.ComparablePath)
module B = Buffer

type 'a t = 'a M.t

let maplist f env =
  M.fold (fun k v r -> (f k v)::r) env []

let flaplist f env =
  List.flatten (maplist f env)

let maplistfilter f env =
  M.fold (fun k v r -> let c = f k v in match c with Some c -> c::r | None -> r) env []

let primfilterlist g f env =
  M.fold (fun k v r -> if f k v then (g (k, v))::r else r) env []

let filterlist f env =
  primfilterlist snd f env

let filterkeylist f env =
  primfilterlist fst f env

let mapfilter f env =
  M.fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let add a m =
  M.add a m

let empty = M.empty

let addn items m =
  List.fold_left (fun e (k, v) -> M.add k v e) m items

let find k m =
  Bstats.time "env find" (M.find k) m

let iter f m =
  M.iter f m

let mem k m =
  M.mem k m

let map f m =
  M.map f m

let mapi f m =
  M.mapi f m

let fold f m b =
  M.fold f m b

let all m =
  maplist (fun v r -> v) m

let combine e1 e2 =
  fold (fun p f e -> add p f e) e1 e2

let domain env = maplist (fun k _ -> k) env

let badstring s = if String.length s >= 2 then (s.[0] = '_' && s.[1] = '\'') || (s.[0] = 'A' && s.[1] = 'A') else false

let setstring e = B.contents (fold (fun k _ s -> match Path.unique_ident_name k with Some n when String.length n <= 2 || not(badstring n) -> B.add_char s '*'; B.add_string s n; s | _ -> s) e (B.create 400))
