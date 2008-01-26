open Constraint

type error

exception Error of Location.t * error
exception Errors of (Location.t * error) list

val report_error: Format.formatter -> error -> unit
val report_errors: Format.formatter -> (Location.t * error) list -> unit 

val write_frame_log: string -> unit

val qualify_implementation:
  string -> Frame.t Lightenv.t -> Qualifier.t list -> Typedtree.structure -> unit
val qualgen_nasty_hack: Frame.t Lightenv.t -> Typedtree.structure -> unit
