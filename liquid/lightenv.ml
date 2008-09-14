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

type 'a t = Env of 'a M.t * 'a origin_env

and 'a origin_env =
  | Nil
  | Parent of 'a t
  | Alias of 'a t

let empty = Env (M.empty, Nil)

let unalias (Env (_, o) as env) = match o with
  | Nil | Parent _ -> env
  | Alias e        -> e

let add x y (Env (m, _) as parent) =
  Env (M.add x y m, Parent parent)

let aliasing_add x y (Env (m, _) as alias) =
  Env (M.add x y m, Alias alias)

let nil_add x y (Env (m, _)) =
  Env (M.add x y m, Nil)

let maplist f (Env (env, _)) =
  M.fold (fun k v r -> (f k v)::r) env []

let maplistfilter f (Env (env, _)) =
  M.fold (fun k v r -> let c = f k v in match c with Some c -> c::r | None -> r) env []

let primfilterlist g f (Env (env, _)) =
  M.fold (fun k v r -> if f k v then (g (k, v))::r else r) env []

let filterlist f env =
  primfilterlist snd f env

let filterkeylist f env =
  primfilterlist fst f env

let mapfilter f (Env (env, _)) =
  M.fold (fun k v r -> match f k v with Some x -> x::r | None -> r) env []

let addn items (Env (m, _) as parent) =
  Env (List.fold_left (fun e (k, v) -> M.add k v e) m items, Parent parent)

let find k (Env (m, _)) =
  M.find k m

let iter f (Env (m, _)) =
  M.iter f m

let mem k (Env (m, _)) =
  M.mem k m

let map f (Env (m, p)) =
  Env (M.map f m, p)

let mapi f (Env (m, p)) =
  Env (M.mapi f m, p)

let fold f (Env (m, _)) b =
  M.fold f m b

let domain env = maplist (fun k _ -> k) env

let badstring s = (s.[0] = '_' && s.[1] = '\'') || (s.[0] = 'A' && s.[1] = 'A')

let setstring e = B.contents (fold (fun k _ s -> match Path.unique_ident_name k with Some n when String.length n <= 2 || not(badstring n) -> B.add_char s '*'; B.add_string s n; s | _ -> s) e (B.create 400))
