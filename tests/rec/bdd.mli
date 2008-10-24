(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(** Binary Decision Diagrams (BDDs) *)

(** Number of variables *)

type variable = int (** 1..max_var *)

val get_max_var : unit -> int
val set_max_var : int -> unit

(** Size of the internal nodes table. 
   The table has a default size (100003) and is resized when necessary 
   (i.e. when too many collisions occur).
   But it can be given a different initial size with this function.
   IMPORTANT NOTE: nodes created before [init] should not be used anymore. *)

val init : int -> unit

(** The abstract type of BDD nodes *)

type t

(** View *)

type view = Zero | One | Node of variable * t (*low*) * t (*high*)
val view : t -> view

(** Accessors *)

val var : t -> variable
    (** convention: [Zero] and [One] have variable [max_var+1] *)

val low : t -> t
val high : t -> t
    (** [low] and [high] raise [Invalid_argument] on [Zero] and [One] *)

(** Constructors *)

val zero : t
val one : t

val mk : variable -> low:t -> high:t -> t

val mk_var : variable -> t

val mk_not : t -> t
val mk_and : t -> t -> t
val mk_or : t -> t -> t
val mk_imp : t -> t -> t

(** Generic binary operator constructor *)

val apply : (bool -> bool -> bool) -> t -> t -> t

(** formula -> bdd *)

type formula = 
  | Ffalse 
  | Ftrue 
  | Fvar of variable 
  | Fand of formula * formula
  | For  of formula * formula
  | Fimp of formula * formula
  | Fiff of formula * formula
  | Fnot of formula

val build : formula -> t

(** Satisfiability *)

val is_sat : t -> bool (* <> 0 *)
val tautology : t -> bool (* = 1 *)

val count_sat : t -> Int64.t
val any_sat : t -> (variable * bool) list
val all_sat : t -> (variable * bool) list list

(** Pretty printer *)

val print_to_dot : t -> file:string -> unit

val display : t -> unit
  (** displays the given bdd using a shell command "dot -Tps <file> | gv -" *)

(** Stats *)

val stats : unit -> int * int * int * int * int * int
  (** Return statistics on the internal nodes table.  
      The numbers are, in order:
      table length, number of entries, sum of bucket lengths,
      smallest bucket length, median bucket length, biggest bucket length. *)
