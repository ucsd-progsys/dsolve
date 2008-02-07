open Constraint

type error

exception Error of Location.t * error
exception Errors of (Location.t * error) list

val report_error: Format.formatter -> error -> unit
val report_errors: Format.formatter -> (Location.t * error) list -> unit 

val qualify_implementation:
  string -> Frame.t Lightenv.t -> Qualifier.t list -> Typedtree.structure -> unit
