(*
 * Copyright ? 1990-2007 The Regents of the University of California. All rights reserved. 
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

(* $Id: misc.ml,v 1.14 2006/09/26 01:47:01 jhala Exp $
 *
 * This file is part of the SIMPLE Project.
 *)

(**
 * This module provides some miscellaneous useful helper functions.
 *)

let do_memo memo f args key = 
  try Hashtbl.find memo key with Not_found ->
    let rv = f args in
    let _ = Hashtbl.replace memo key rv in
    rv


let flap f xs = List.flatten (List.map f xs)

(** hashtbl_keys tbl returns the list of keys with bindings in the table *)

let hashtbl_keys t = 
  Hashtbl.fold (fun x y l -> x::l) t []
 
let sort_and_compact ls =
  let rec _sorted_compact l = 
    match l with
	h1::h2::tl ->
	  let rest = _sorted_compact (h2::tl) in
	    if h1 = h2 then rest else h1::rest
      | tl -> tl
  in
    _sorted_compact (List.sort compare ls)   

(** repeats f: unit - > unit i times *)
let rec repeat_fn f i = 
  if i = 0 then ()
  else (f (); repeat_fn f (i-1))
(*
let is_substring s subs = 
  let reg = Str.regexp subs in
  try ignore(Str.search_forward reg s 0); true
  with Not_found -> false
*)
module IntMap = 
  Map.Make 
  (struct
    type t = int
    let compare i1 i2 = 
      compare i1 i2
  end)

let iteri f xs =
  let rec _m i l = 
    match l with [] -> ()
    | h::t -> ((f i h);(_m (i+1) t)) in
  _m 0 xs

exception FalseException
let rec intmap_for_all f m =
  try 
    IntMap.iter (fun i v -> if not (f i v) then raise FalseException) m;
    true
  with FalseException -> false

let hashtbl_to_list t = 
   Hashtbl.fold (fun x y l -> (x,y)::l) t []

let rec clone x n = 
  if n <= 0 then [] else (x::(clone x (n-1)))

let rec join l s =
  match l with
      [] ->
	""
    | h::[] ->
	h
    | h::t ->
	h ^ s ^ (join t s)

let rec search_list test = function
    h::t ->
      begin
	try
	  test h
	with Not_found ->
	  search_list test t
      end
  | [] ->
      raise Not_found

let rec mapfilter f = function
    h::t ->
      let rest = mapfilter f t in
	begin match f h with
	    Some r ->
	      r::rest
	  | None ->
	      rest
	end
  | [] ->
      []
	      
