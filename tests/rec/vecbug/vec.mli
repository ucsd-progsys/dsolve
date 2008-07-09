(* Vec: implementation of extensible arrays. 

   Copyright Luca de Alfaro <lda@dealfaro.com>, 2007.

   Version 1.1

   Based on Xavier Leroy's code for Set and Map.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the following special exception:

   You may link, statically or dynamically, a "work that uses the
   Library" with a publicly distributed version of the Library to
   produce an executable file containing portions of the Library, and
   distribute that executable file under terms of your choice, without
   any of the additional requirements listed in clause 6 of the GNU
   Library General Public License.  By "a publicly distributed version
   of the Library", we mean either the unmodified Library as
   distributed by INRIA, or a modified version of the Library that is
   distributed under the conditions defined in clause 2 of the GNU
   Library General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be
   covered by the GNU Library General Public License.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   The GNU Library General Public License is available at
   http://www.gnu.org/copyleft/lgpl.html; to obtain it, you can also
   write to the Free Software Foundation, Inc., 59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Extensible arrays. 

    This module implements extensible arrays.  All operations are
    purely applicative (no side-effects).  The implementation, based
    on the implementation of Set, uses balanced binary trees, so that
    insertion, deletions, and updates take logarithmic time in the
    size of the array.  *)

type 'a t
  (** The type of extensible arrays (vec) of elements of type ['a] *)

exception Vec_index_out_of_bounds
  (** Raised when a vec is accessed out of bounds *)

val empty : 'a t
  (** The empty vec. *)

val singleton : 'a -> 'a t
  (** [singleton d] creates a vec consisting of exactly the element [d] *)

val create : 'a -> int -> 'a t
  (** [create d n] creates a vec of length [n], where each cell
      contains [d]. *)

val length : 'a t -> int
  (** Returns the length of the vec. *)

val is_empty : 'a t -> bool
  (** Tests whether a vec is empty or not. *)

val get : int -> 'a t -> 'a
  (** [get i v] gets element [i] of vec [v]; the first element of [v]
      is element 0. *)

val set : int -> 'a -> 'a t -> 'a t
  (** [set i d v] sets the element in position [i] of vec [v] to value [d].  
      The position must already exist. *)

val append : 'a -> 'a t -> 'a t
  (** [append d v] extends the vec [v], by adding the element [d] at
      the end. *)

val setappend : 'a -> 'a -> int -> 'a t -> 'a t
  (** [setappend d0 d i v] sets the element in position [i] of [v] to
      value [d], if [v] is long enough.  Otherwise, it extends [v]
      with as many elements [d0] as needed, then appends the value [d]
      in position [i]. *)

val concat : 'a t -> 'a t -> 'a t
  (** [concat v1 v2] concatenates the vecs [v1] and [v2]. *)

val pop : int -> 'a t -> ('a * 'a t)
  (** [pop i v] returns (e, v'), where [e] is the element in position [i] 
      of vec [v], and [v'] is obtained by removing [e] from [v]. *)

val remove : int -> 'a t -> 'a t
  (** [remove i v] removes the element in position [i] of vec [v]. *)

val insert : int -> 'a -> 'a t -> 'a t
  (** [insert i d v] inserts element [d] in position [i] of vec [v].
      All elements following [i] are shifted right by 1.  If [i] is 0,
      then [d] is inserted in the first position of the vec.  If [i] is
      [length v], then [d] is inserted at the end of [v], extending it
      by 1. *)

val sub : int -> int -> 'a t -> 'a t 
  (** [sub i j v] returns the sub-vec of [v] consisting of all elements
      with indices [k] with [i] <= [k] < [j]. *)

val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f v] applies [f] to all elements in [v].  The elements are
      passed to [f] in increasing index order. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
  (** Similar to iter, but the function is passed also the index of the 
      element to which it is applied. *)

val reviter : ('a -> unit) -> 'a t -> unit
  (** [reviter f v] applies [f] to all elements in [v].  The elements are
      passed to [f] in decreasing index order. *)

val rangeiter : ('a -> unit) -> int -> int -> 'a t -> unit
  (** [rangeiter f i j v] applies [f] to all elements in [v] that have
      index [k] with [i] <= [k] < [j].  The elements are passed to [f]
      in increasing index order. *)

val rangeiteri : (int -> 'a -> unit) -> int -> int -> 'a t -> unit
  (** [rangeiteri f i j v] is the same as [rangeiter f i j v], but [f] 
      is applied also to the index of each element. *)

val revrangeiter : ('a -> unit) -> int -> int -> 'a t -> unit
  (** [revrangeiter f v i j] applies [f] to all elements in [v] that have
      index [k] with [i] <= [k] < [j].  The elements are passed to [f]
      in decreasing index order. *)

val revrangeiteri : (int -> 'a -> unit) -> int -> int -> 'a t -> unit
  (** [revrangeiteri f i j v] is the same as [revrangeiter f i j v],
      but [f] is applied also to the index of each element. *)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f v] returns a vec, obtained by applying the function [f]
      to each element of [v].  The elements are passed to [f] in
      increasing order. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi f v] is the same as [map f v], but the function [f] is
      applied also to the index of each element. *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f v a] computes [(f dN ... (f d0 a)...)], where [d0
      ... dN] are data in the vec, in increasing order. *)

val foldi : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [foldi f v a] computes [(f N dN ... (f 1 d0 a)...)], where 
      [d0 ... dN] are data in the vec, in increasing order. *)

val rangefoldi : (int -> 'a -> 'b -> 'b) -> int -> int -> 'a t -> 'b -> 'b
  (** [rangefoldi f i j v] folds function [f] over the elements in the
      range i, i+1, ..., j-1 of the vector [v].  The function [f] is
      passed the index of the vec element, the element itself, and the
      accumulation.  The folding proceeds in increasing index
      order. *)

val revrangefoldi : (int -> 'a -> 'b -> 'b) -> int -> int -> 'a t -> 'b -> 'b
  (** [revrangefoldi f i j v] is equivalent to [rangefoldi f i j v],
      except that the folding proceeds in decreasing, rather than
      increasing, index order. *)

val of_list : 'a list -> 'a t 
  (** [of_list l] returns a vec consisting of the elements of [l], in
      the same order. *)

val to_list : 'a t -> 'a list
  (** [to_list v] returns a list containing the elements of vec [v], 
      in the order in which they appear in [v]. *)

val of_array : 'a array -> 'a t 
  (** [of_array a] returns a vec consisting of the elements of array [a], in
      the same order. *)

val to_array : 'a t -> 'a array
  (** [to_array v] returns either None, if the vector is empty, or Some of an 
      array containing the elements of vec [v], in the order in which they 
      appear in [v].  The option type is needed, as there is no way to make
      an array of zero length without specifying one element of the array. *)

val visit_post : 'b -> ('b -> 'a -> 'b -> 'b) -> ('a t) -> 'b
  (** [visit_post ve vn a] implements a post-order visit to the tree representation of [a]. 
      [ve] is the evaluation of an empty tree.  For an internal node with data [d] and 
      left and right children that evaluate to [l], [r], the function returns [vn l d r]. *)

val visit_in : 'b -> ('b -> 'a -> 'c) -> ('c -> 'b -> 'b) -> ('a t) -> 'b
  (** [visit_in ve vl vr a] implements an in-order visit to the tree representation of [a]. 
      [ve] is the evaluation of an empty tree.  For an internal node with data [d] and 
      left and right children [cl], [cr], the function returns [vr (vl cl' d) cr'], 
      where [cl'] is the evaluation of [cl], and [cr'] is the evaluation of [cr]. 
      Moreover, [cr'] is computed only after [vl cl' d] is computed. *)

val check_balanced : 'a t -> bool
  (** checks whether a vector is backed by a balanced tree; note that balanced means that
   *  subtree heights are off by at most 2 throughout the tree *)
