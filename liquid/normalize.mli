exception NormalizationFailure of Parsetree.expression * Location.t * string

val normalize_structure : Parsetree.structure -> Parsetree.structure

val desugar_forloops : Parsetree.structure -> Parsetree.structure 
