type instance
val new_instance:       (int -> QpDag.predicate -> (QpDag.predicate -> unit) -> unit) -> instance 
val push:               instance -> QpDag.predicate -> unit
val is_consistent:      instance -> bool
val is_valid:           instance -> QpDag.predicate -> bool
