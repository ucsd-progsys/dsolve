open Qualifymod
open Format

val dump_qualified_structure: formatter -> Frame.t LocationMap.t -> Typedtree.structure -> unit
val pprint_expression: formatter -> Parsetree.expression -> unit
