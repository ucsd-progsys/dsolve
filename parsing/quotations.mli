(* a small routine to parse a string as a toplevel stmt *)

val quote : string -> Parsetree.structure_item_desc
val quote_expr : string -> Parsetree.expression
val map_expr : (Parsetree.expression_desc -> Parsetree.expression_desc)
   -> Parsetree.expression -> Parsetree.expression
val map_expr_desc : (Parsetree.expression_desc -> Parsetree.expression_desc)
   -> Parsetree.expression_desc -> Parsetree.expression_desc
