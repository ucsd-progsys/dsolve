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

module E = Map.Make(Common.ComparablePath)

include E

module B = Buffer

let maplist f env =
  fold (fun k v r -> (f k v)::r) env []

let maplistfilter f env =
  fold (fun k v r -> let c = f k v in match c with Some c -> c::r | None -> r) env []

let primfilterlist g f env =
  fold (fun k v r -> if f k v then (g (k, v))::r else r) env []

let filterlist f env =
  primfilterlist snd f env

let filterkeylist f env =
  primfilterlist fst f env

let mapfilter f env =
  fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let addn items env =
  List.fold_left (fun e (k, v) -> add k v e) env items

let cardinality e = fold (fun _ _ c -> c + 1) e 0

let setcompare e1 e2 = compare (fun x y -> 0) e1 e2
let compare e1 e2 = Pervasives.compare (cardinality e1) (cardinality e2)

let badstring s = s.[0] = '_' && s.[1] = '\''
let setstring e = B.contents (fold (fun k _ s -> match Path.unique_ident_name k with Some n when String.length n <= 2 || not(badstring n) -> B.add_char s '*'; B.add_string s n; s | _ -> s) e (B.create 400))

let domain env = maplist (fun k _ -> k) env
