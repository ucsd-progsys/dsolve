(* File generated from z3.idl *)

type config
and context
and ast
and type_ast
and const_decl_ast
and const_ast
and numeral_ast
and pattern_ast
and symbol
and value
and parameter
and model
and labels
and enum_1 =
  | L_FALSE
  | L_UNDEF
  | L_TRUE
and lbool = enum_1
and enum_2 =
  | INT_SYMBOL
  | STRING_SYMBOL
and symbol_kind = enum_2
and enum_3 =
  | UNINTERPRETED_TYPE
  | BOOL_TYPE
  | INT_TYPE
  | REAL_TYPE
  | BV_TYPE
  | ARRAY_TYPE
  | TUPLE_TYPE
  | UNKNOWN_TYPE
and type_kind = enum_3
and enum_4 =
  | NUMERAL_AST
  | CONST_DECL_AST
  | CONST_AST
  | TYPE_AST
  | VAR_AST
  | PATTERN_AST
  | QUANTIFIER_AST
  | UNKNOWN_AST
and ast_kind = enum_4
and enum_5 =
  | OP_TRUE
  | OP_FALSE
  | OP_EQ
  | OP_DISTINCT
  | OP_ITE
  | OP_AND
  | OP_OR
  | OP_IFF
  | OP_XOR
  | OP_NOT
  | OP_IMPLIES
  | OP_LE
  | OP_GE
  | OP_LT
  | OP_GT
  | OP_ADD
  | OP_SUB
  | OP_UMINUS
  | OP_MUL
  | OP_DIV
  | OP_IDIV
  | OP_REM
  | OP_MOD
  | OP_STORE
  | OP_SELECT
  | OP_CONST_ARRAY
  | OP_ARRAY_DEFAULT
  | OP_STORE_ITE
  | OP_SET_UNION
  | OP_SET_INTERSECT
  | OP_SET_DIFFERENCE
  | OP_SET_COMPLEMENT
  | OP_SET_SUBSET
  | OP_BIT1
  | OP_BIT0
  | OP_BNEG
  | OP_BADD
  | OP_BSUB
  | OP_BMUL
  | OP_BSDIV
  | OP_BUDIV
  | OP_BSREM
  | OP_BUREM
  | OP_BSMOD
  | OP_BSDIV0
  | OP_BUDIV0
  | OP_BSREM0
  | OP_BUREM0
  | OP_BSMOD0
  | OP_ULEQ
  | OP_SLEQ
  | OP_UGEQ
  | OP_SGEQ
  | OP_ULT
  | OP_SLT
  | OP_UGT
  | OP_SGT
  | OP_BAND
  | OP_BOR
  | OP_BNOT
  | OP_BXOR
  | OP_BNAND
  | OP_BNOR
  | OP_BXNOR
  | OP_CONCAT
  | OP_SIGN_EXT
  | OP_ZERO_EXT
  | OP_EXTRACT
  | OP_REPEAT
  | OP_BREDOR
  | OP_BREDAND
  | OP_BCOMP
  | OP_BSHL
  | OP_BLSHR
  | OP_BASHR
  | OP_ROTATE_LEFT
  | OP_ROTATE_RIGHT
  | OP_INT2BV
  | OP_BV2INT
  | OP_UNINTERPRETED
and decl_kind = enum_5
and enum_6 =
  | BOOL_VALUE
  | NUMERAL_VALUE
  | ARRAY_VALUE
  | TUPLE_VALUE
  | UNKNOWN_VALUE
and value_kind = enum_6

(**
   

*)
(**
   
   
   

   
   
   
   
   
   
   
   
   
   
   
*)
(**
   
*)
(**
   
*)
(**
   
*)
(**
   

   
   
*)
(**
   
*)
(**
   

   
   
   
   
   
   
   
   
*)
(**
   

*)
(**
   
*)
(** 
        {2 {L Create configuration}}
    *)
(**
       Summary: Create a configuration.

       Configurations are created in order to assign parameters prior to creating 
       contexts for Z3 interaction. For example, if the users whishes to use model
       generation, then call:

       [set_param_value cfg "MODEL" "true"]

        - {b Remarks}: Consider using {!Z3.mk_context_x} instead of using
       explicit configuration objects. The function {!Z3.mk_context_x}
       receives an array of string pairs. This array represents the
       configuration options. 

       - {b See also}: {!Z3.set_param_value}
       - {b See also}: {!Z3.del_config}
    *)
external mk_config : unit -> config
	= "camlidl_z3_Z3_mk_config"

(**
       Summary: Delete the given configuration object.

       - {b See also}: {!Z3.mk_config}
    *)
external del_config : config -> unit
	= "camlidl_z3_Z3_del_config"

(**
       Summary: Set a configuration parameter.

       The list of all configuration parameters can be obtained using the Z3 executable:

       {v 
       z3.exe -ini?
        v}

       - {b See also}: {!Z3.mk_config}
    *)
external set_param_value : config -> string -> string -> unit
	= "camlidl_z3_Z3_set_param_value"

(**
       {2 {L Create context}}
    *)
(**
       Summary: Create a logical context using the given configuration. 
    
       After a context is created, the configuration cannot be changed.
       All main interaction with Z3 happens in the context of a context.

        - {b Remarks}: Consider using {!Z3.mk_context_x} instead of using
       explicit configuration objects. The function {!Z3.mk_context_x}
       receives an array of string pairs. This array represents the
       configuration options. 

       - {b See also}: {!Z3.del_context}
    *)
external mk_context : config -> context
	= "camlidl_z3_Z3_mk_context"

(**
       Summary: Delete the given logical context.

       - {b See also}: {!Z3.mk_config}
    *)
external del_context : context -> unit
	= "camlidl_z3_Z3_del_context"

(**
       Summary: Enable trace messages to a file

       When trace messages are enabled, Z3 will record the operations performed on a context in the given file file.
       Return TRUE if the file was opened successfully, and FALSE otherwise.

       - {b See also}: {!Z3.trace_off}
    *)
external trace_to_file : context -> string -> bool
	= "camlidl_z3_Z3_trace_to_file"

(**
       Summary: Enable trace messages to a standard error.

       - {b See also}: {!Z3.trace_off}
    *)
external trace_to_stderr : context -> unit
	= "camlidl_z3_Z3_trace_to_stderr"

(**
       Summary: Enable trace messages to a standard output.

       - {b See also}: {!Z3.trace_off}
    *)
external trace_to_stdout : context -> unit
	= "camlidl_z3_Z3_trace_to_stdout"

(**
       Summary: Disable trace messages.

       - {b See also}: {!Z3.trace_to_file}
       - {b See also}: {!Z3.trace_to_stdout}
       - {b See also}: {!Z3.trace_to_stderr}
    *)
external trace_off : context -> unit
	= "camlidl_z3_Z3_trace_off"

(**
       {2 {L Theories}}
    *)
(**
       Summary: Enable arithmetic theory in the given logical context.
    *)
external enable_arithmetic : context -> unit
	= "camlidl_z3_Z3_enable_arithmetic"

(**
       Summary: Enable bit-vector theory in the given logical context.
    *)
external enable_bv : context -> unit
	= "camlidl_z3_Z3_enable_bv"

(**
       Summary: Enable array theory in the given logical context.
    *)
external enable_arrays : context -> unit
	= "camlidl_z3_Z3_enable_arrays"

(**
       Summary: Enable tuple theory in the given logical context.
    *)
external enable_tuples : context -> unit
	= "camlidl_z3_Z3_enable_tuples"

(**
       {2 {L Symbols}}
    *)
(**
       Summary: Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       - {b See also}: {!Z3.mk_string_symbol}
    *)
external mk_int_symbol : context -> int -> symbol
	= "camlidl_z3_Z3_mk_int_symbol"

(**
       Summary: Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       - {b See also}: {!Z3.mk_int_symbol}
    *)
external mk_string_symbol : context -> string -> symbol
	= "camlidl_z3_Z3_mk_string_symbol"

(**
       {2 {L Types}}
    *)
(**
       Summary: Create a free (uninterpreted) type using the given name (symbol).
       
       Two free types are considered the same iff the have the same name.
    *)
external mk_uninterpreted_type : context -> symbol -> type_ast
	= "camlidl_z3_Z3_mk_uninterpreted_type"

(**
       Summary: Create the Boolean type. 

       This type is used to create propositional variables and predicates.
    *)
external mk_bool_type : context -> type_ast
	= "camlidl_z3_Z3_mk_bool_type"

(**
       Summary: Create an integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       {!Z3.mk_bv_type} creates a bit-vector type.

       - {b See also}: {!Z3.mk_bv_type}
    *)
external mk_int_type : context -> type_ast
	= "camlidl_z3_Z3_mk_int_type"

(**
       Summary: Create a real type. 

       This type is not a floating point number.
       Z3 does not have support for floating point numbers yet.
    *)
external mk_real_type : context -> type_ast
	= "camlidl_z3_Z3_mk_real_type"

(**
       Summary: Create a bit-vector type of the given size.
    
       This type can also be seen as a machine integer.

       - {b Remarks}: The size of the bitvector type must be greater than zero.
    *)
external mk_bv_type : context -> int -> type_ast
	= "camlidl_z3_Z3_mk_bv_type"

(**
       Summary: Create an array type. 
       
       We usually represent the array type as: {e [domain -> range] }.
       Arrays are usually used to model the heap/memory in software verification.

       - {b See also}: {!Z3.mk_select}
       - {b See also}: {!Z3.mk_store}
    *)
external mk_array_type : context -> type_ast -> type_ast -> type_ast
	= "camlidl_z3_Z3_mk_array_type"

(**
       Summary: Create a tuple type.
       
        [mk_tuple_type c name field_names field_types] creates a tuple with a constructor named [name],
       a [n] fields, where [n] is the size of the arrays [field_names] and [field_types].
       

       
       

       
       
       
       
       
       
       
    *)
external mk_tuple_type : context -> symbol -> symbol array -> type_ast array -> type_ast * const_decl_ast * const_decl_ast array
	= "camlidl_z3_Z3_mk_tuple_type"

(**
       {2 {L Constants and Applications}}
     *)
(**
       Summary: Declare a constant or function.

        [mk_func_decl c n d r] creates a function with name [n], domain [d], and range [r].
       The arity of the function is the size of the array [d]. 

       
       
       
       
       

       After declaring a constant or function, the function
       {!Z3.mk_app} can be used to create a constant or function
       application.

       - {b See also}: {!Z3.mk_app}
    *)
external mk_func_decl : context -> symbol -> type_ast array -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_mk_func_decl"

(**
       Summary: Create a constant or function application.

       - {b See also}: {!Z3.mk_func_decl}
    *)
external mk_app : context -> const_decl_ast -> ast array -> ast
	= "camlidl_z3_Z3_mk_app"

(**
       Summary: Declare and create a constant.
       
       
       
       
       
       
       
        [mk_const c s t] is a shorthand for [mk_app c (mk_func_decl c s [||] t) [||]] 

       - {b See also}: {!Z3.mk_func_decl}
       - {b See also}: {!Z3.mk_app}
    *)
external mk_const : context -> symbol -> type_ast -> ast
	= "camlidl_z3_Z3_mk_const"


(**
       Summary: Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       
       
       

       - {b See also}: {!Z3.mk_func_decl}
    *)
external mk_fresh_func_decl : context -> string -> type_ast array -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_mk_fresh_func_decl"

(**
       Summary: Declare and create a fresh constant.
       
       
       
       
       
       

        [mk_fresh_const c p t] is a shorthand for [mk_app c (mk_fresh_func_decl c p [||] t) [||]]. 

       
       
       - {b See also}: {!Z3.mk_func_decl}
       - {b See also}: {!Z3.mk_app}
    *)
external mk_fresh_const : context -> string -> type_ast -> ast
	= "camlidl_z3_Z3_mk_fresh_const"

(** 
        Summary: Create an AST node representing true.
    *)
external mk_true : context -> ast
	= "camlidl_z3_Z3_mk_true"

(** 
        Summary: Create an AST node representing false.
    *)
external mk_false : context -> ast
	= "camlidl_z3_Z3_mk_false"

(** 
        Summary: \[ [ mk_eq c l r ] \]
        Create an AST node representing {e l = r }.
        
        The nodes l and r must have the same type. 
    *)
external mk_eq : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_eq"

(**
       
        Summary: \[ [mk_distinct c [| t_1; ...; t_n |]] \] Create an AST
       node represeting a distinct construct. It is used for declaring
       the arguments t_i pairwise distinct. 

       
       
       
       All arguments must have the same type.

       - {b Remarks}: The number of arguments of a distinct construct must be greater than one.
    *)
external mk_distinct : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_distinct"

(** 
        Summary: \[ [ mk_not c a ] \] 
        Create an AST node representing {e not(a) }.
        
        The node a must have Boolean type.
    *)
external mk_not : context -> ast -> ast
	= "camlidl_z3_Z3_mk_not"

(**
       Summary: \[ [ mk_ite c t1 t2 t2 ] \] 
       Create an AST node representing an if-then-else: {e ite(t1, t2,
       t3) }.

       The node t1 must have Boolean type, t2 and t3 must have the same type.
       The type of the new node is equal to the type of t2 and t3.
    *)
external mk_ite : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ite"

(**
       Summary: \[ [ mk_iff c t1 t2 ] \]
       Create an AST node representing {e t1 iff t2 }.

       The nodes t1 and t2 must have Boolean type.
    *)
external mk_iff : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_iff"

(**
       Summary: \[ [ mk_implies c t1 t2 ] \]
       Create an AST node representing {e t1 implies t2 }.

       The nodes t1 and t2 must have Boolean type.
    *)
external mk_implies : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_implies"

(**
       Summary: \[ [ mk_xor c t1 t2 ] \]
       Create an AST node representing {e t1 xor t2 }.

       The nodes t1 and t2 must have Boolean type.
    *)
external mk_xor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_xor"

(**
       
        Summary: \[ [mk_and c [| t_1; ...; t_n |]] \] Create the conjunction: {e t_1 and ... and t_n}. 

       
       All arguments must have Boolean type.
       
       - {b Remarks}: The number of arguments must be greater than zero.
    *)
external mk_and : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_and"

(**
       
        Summary: \[ [mk_or c [| t_1; ...; t_n |]] \] Create the disjunction: {e t_1 or ... or t_n}. 

       
       All arguments must have Boolean type.

       - {b Remarks}: The number of arguments must be greater than zero.
    *)
external mk_or : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_or"

(**
       
        Summary: \[ [mk_add c [| t_1; ...; t_n |]] \] Create the term: {e t_1 + ... + t_n}. 

       
       All arguments must have int or real type.

       - {b Remarks}: The number of arguments must be greater than zero.
    *)
external mk_add : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_add"

(**
       
        Summary: \[ [mk_mul c [| t_1; ...; t_n |]] \] Create the term: {e t_1 * ... * t_n}. 

       
       All arguments must have int or real type.
       
       - {b Remarks}: Z3 has limited support for non-linear arithmetic.
       - {b Remarks}: The number of arguments must be greater than zero.
    *)
external mk_mul : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_mul"

(**
       
        Summary: \[ [mk_sub c [| t_1; ...; t_n |]] \] Create the term: {e t_1 - ... - t_n}. 

       
       All arguments must have int or real type.

       - {b Remarks}: The number of arguments must be greater than zero.
    *)
external mk_sub : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_sub"

(** 
        Summary: \[ [ mk_lt c t1 t2 ] \] 
        Create less than.

        The nodes t1 and t2 must have the same type, and must be int or real.
    *)
external mk_lt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_lt"

(** 
        Summary: \[ [ mk_le c t1 t2 ] \]
        Create less than or equal to.
        
        The nodes t1 and t2 must have the same type, and must be int or real.
    *)
external mk_le : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_le"

(** 
        Summary: \[ [ mk_gt c t1 t2 ] \]
        Create greater than.
        
        The nodes t1 and t2 must have the same type, and must be int or real.
    *)
external mk_gt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_gt"

(** 
        Summary: \[ [ mk_ge c t1 t2 ] \]
        Create greater than or equal to.
        
        The nodes t1 and t2 must have the same type, and must be int or real.
    *)
external mk_ge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ge"

(**
       Summary: \[ [ mk_bvneg c t1 ] \]
       Bitwise negation.

       The node t1 must have a bit-vector type.
    *)
external mk_bvnot : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnot"

(**
       Summary: \[ [ mk_bvand c t1 t2 ] \]
       Bitwise and.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvand"

(**
       Summary: \[ [ mk_bvor c t1 t2 ] \]
       Bitwise or.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvor"

(**
       Summary: \[ [ mk_bvxor c t1 t2 ] \]
       Bitwise exclusive-or.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvxor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxor"

(**
       Summary: \[ [ mk_bvnand c t1 t2 ] \]
       Bitwise nand. 

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvnand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnand"

(**
       Summary: \[ [ mk_bvnor c t1 t2 ] \]
       Bitwise nor. 

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnor"

(**
       Summary: \[ [ mk_bvxnor c t1 t2 ] \]
       Bitwise xnor. 
       
       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvxnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxnor"

(**
       Summary: \[ [ mk_bvneg c t1 ] \]
       Standard two's complement unary minus. 

       The node t1 must have bit-vector type.
    *)
external mk_bvneg : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvneg"

(** 
        Summary: \[ [ mk_bvadd c t1 t2 ] \]
        Standard two's complement addition.
        
        The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvadd : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvadd"

(** 
        Summary: \[ [ mk_bvsub c t1 t2 ] \]
        Standard two's complement subtraction.
        
        The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvsub : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsub"

(** 
        Summary: \[ [ mk_bvmul c t1 t2 ] \]
        Standard two's complement multiplication.
        
        The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvmul : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvmul"

(** 
        Summary: \[ [ mk_bvudiv c t1 t2 ] \]
        Unsigned division. 

        It is defined as the floor of {e t1/t2 } if t2 is
        different from zero. If {e t2 } is zero, then the result
        is undefined.
        
        The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvudiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvudiv"

(** 
        Summary: \[ [ mk_bvsdiv c t1 t2 ] \]
        Two's complement signed division. 

        It is defined in the following way:

        - The floor of {e t1/t2 } if t2 is different from zero, and {e t1*t2 >= 0 }.

        - The ceiling of {e t1/t2 } if t2 is different from zero, and {e t1*t2 < 0 }.
        
        If {e t2 } is zero, then the result is undefined.
        
        The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvsdiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsdiv"

(**
       Summary: \[ [ mk_bvurem c t1 t2 ] \]
       Unsigned remainder.

       It is defined as {e t1 - (t1 /u t2) * t2 }, where {e /u } represents unsigned int division.
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvurem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvurem"

(**
       Summary: \[ [ mk_bvsrem c t1 t2 ] \]
       Two's complement signed remainder (sign follows dividend).

       It is defined as {e t1 - (t1 /s t2) * t2 }, where {e /s } represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of t1.

       If {e t2 } is zero, then the result is undefined.
       
       The nodes t1 and t2 must have the same bit-vector type.

       - {b See also}: {!Z3.mk_bvsmod}
    *)
external mk_bvsrem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsrem"

(**
       Summary: \[ [ mk_bvsmod c t1 t2 ] \]
       Two's complement signed remainder (sign follows divisor).
       
       If {e t2 } is zero, then the result is undefined.
       
       The nodes t1 and t2 must have the same bit-vector type.

       - {b See also}: {!Z3.mk_bvsrem}
    *)
external mk_bvsmod : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsmod"

(**
       Summary: \[ [ mk_bvult c t1 t2 ] \]
       Unsigned less than.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvult : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvult"

(**
       Summary: \[ [ mk_bvslt c t1 t2 ] \]
       Two's complement signed less than.
       
       It abbreviates:
       {v 
      (or (and (= (extract[|m-1|:|m-1|] s) bit1)
               (= (extract[|m-1|:|m-1|] t) bit0))
          (and (= (extract[|m-1|:|m-1|] s) (extract[|m-1|:|m-1|] t))
               (bvult s t)))
        v}

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvslt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvslt"

(**
       Summary: \[ [ mk_bvule c t1 t2 ] \]
       Unsigned less than or equal to.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvule : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvule"

(**
       Summary: \[ [ mk_bvsle c t1 t2 ] \]
       Two's complement signed less than or equal to.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvsle : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsle"

(**
       Summary: \[ [ mk_bvuge c t1 t2 ] \]
       Unsigned greater than or equal to.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvuge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvuge"

(**
       Summary: \[ [ mk_bvsge c t1 t2 ] \]
       Two's complement signed greater than or equal to.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvsge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsge"

(**
       Summary: \[ [ mk_bvugt c t1 t2 ] \]
       Unsigned greater than.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvugt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvugt"

(**
       Summary: \[ [ mk_bvsgt c t1 t2 ] \]
       Two's complement signed greater than.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvsgt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsgt"

(**
       Summary: \[ [ mk_concat c t1 t2 ] \]
       Concatenate the given bit-vectors.
       
       The nodes t1 and t2 must have (possibly different) bit-vector types

       The result is a bit-vector of size {e n1+n2 }, where n1 (n2) is the size
       of t1 (t2).
    *)
external mk_concat : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_concat"

(**
       Summary: \[ [ mk_extract c high low t1 ] \]
       Extract the bits high down to low from a bitvector of
       size m to yield a new bitvector of size n, where {e n =
       high - low + 1 }.

       The node t1 must have a bit-vector type.
    *)
external mk_extract : context -> int -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_extract"

(**
       Summary: \[ [ mk_sign_ext c i t1 ] \]
       Sign-extend of the given bit-vector to the (signed) equivalent bitvector of
       size {e m+i }, where m is the size of the given
       bit-vector.

       The node t1 must have a bit-vector type.
    *)
external mk_sign_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_sign_ext"

(**
       Summary: \[ [ mk_zero_ext c i t1 ] \]
       Extend the given bit-vector with zeros to the (unsigned int) equivalent
       bitvector of size {e m+i }, where m is the size of the
       given bit-vector.
       
       The node t1 must have a bit-vector type. 
    *)
external mk_zero_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_zero_ext"

(**
       Summary: \[ [ mk_bvshl c t1 t2 ] \]
       Shift left.

       It is equivalent to multiplication by {e 2^x } where x is the value of the
       third argument.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvshl : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvshl"

(**
       Summary: \[ [ mk_bvlshr c t1 t2 ] \]
       Logical shift right.

       It is equivalent to unsigned int division by {e 2^x } where x is the
       value of the third argument.

       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvlshr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvlshr"

(**
       Summary: \[ [ mk_bvashr c t1 t2 ] \]
       Arithmetic shift right.
       
       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.
       
       The nodes t1 and t2 must have the same bit-vector type.
    *)
external mk_bvashr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvashr"

(**
       Summary: \[ [ mk_rotate_left c i t1 ] \]
       Rotate bits of t1 to the left i times.
       
       The node t1 must have a bit-vector type. 
    *)
external mk_rotate_left : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_left"

(**
       Summary: \[ [ mk_rotate_right c i t1 ] \]
       Rotate bits of t1 to the right i times.
       
       The node t1 must have a bit-vector type. 
    *)
external mk_rotate_right : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_right"

(**
       Summary: \[ [ mk_select c a i ] \]
       Array read.

       The node a must have an array type {e [domain -> range] }, and i must have the type domain.
       The type of the result is range.

       - {b See also}: {!Z3.mk_array_type}
       - {b See also}: {!Z3.mk_store}
    *)
external mk_select : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_select"

(**
       Summary: \[ [ mk_store c a i v ] \]
       Array update.
       
       The node a must have an array type {e [domain -> range] }, i must have type domain,
       v must have type range. The type of the result is {e [domain -> range] }.
       
       - {b See also}: {!Z3.mk_array_type}
       - {b See also}: {!Z3.mk_store}
    *)
external mk_store : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_store"

(**
       {2 {L Numerals}}
    *)
(**
       Summary: Create a numeral of a given type. 

       
       
       
       
       - {b See also}: {!Z3.mk_int}
       - {b See also}: {!Z3.mk_unsigned_int}
    *)
external mk_numeral : context -> string -> type_ast -> ast
	= "camlidl_z3_Z3_mk_numeral"

(**
       Summary: Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than {!Z3.mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!Z3.mk_numeral}
    *)
external mk_int : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_int"

(**
       Summary: Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine unsinged integer.
       It is slightly faster than {!Z3.mk_numeral} since it is not necessary to parse a string.

       - {b See also}: {!Z3.mk_numeral}
    *)
external mk_unsigned_int : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_unsigned_int"

(**
       {2 {L Quantifiers}}
    *)
(**
       Summary: Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.
       
       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.


       - {b See also}: {!Z3.mk_forall}
       - {b See also}: {!Z3.mk_exists}
    *)
external mk_pattern : context -> ast array -> pattern_ast
	= "camlidl_z3_Z3_mk_pattern"

(**
       Summary: Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       {v  
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
        v}

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.
       
       
       
       

       - {b See also}: {!Z3.mk_forall}
       - {b See also}: {!Z3.mk_exists}
    *)
external mk_bound : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_bound"

(**
       Summary: Create a forall formula.

        [mk_forall c w p t n b] creates a forall formula, where
       [w] is the weight, [p] is an array of patterns, [t] is an array
       with the types of the bound variables, [n] is an array with the
       'names' of the bound variables, and [b] is the body of the
       quantifier. Quantifiers are associated with weights indicating
       the importance of using the quantifier during
       instantiation. 
       
       
       
       
       
       
       
       
       
       - {b See also}: {!Z3.mk_pattern}
       - {b See also}: {!Z3.mk_bound}
       - {b See also}: {!Z3.mk_exists}
    *)
external mk_forall : context -> int -> pattern_ast array -> type_ast array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_forall_bytecode" "camlidl_z3_Z3_mk_forall"

(**
       Summary: Create an exists formula. Similar to {!Z3.mk_forall}.
       
       - {b See also}: {!Z3.mk_pattern}
       - {b See also}: {!Z3.mk_bound}
       - {b See also}: {!Z3.mk_forall}
    *)
external mk_exists : context -> int -> pattern_ast array -> type_ast array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_exists_bytecode" "camlidl_z3_Z3_mk_exists"


(**
       {2 {L Accessors}}
    *)
(**
       Summary: Return INT_SYMBOL if the symbol was constructed
       using {!Z3.mk_int_symbol}, and STRING_SYMBOL if the symbol
       was constructed using {!Z3.mk_string_symbol}.
    *)
external get_symbol_kind : context -> symbol -> symbol_kind
	= "camlidl_z3_Z3_get_symbol_kind"

(**
       Summary: \[ [ get_symbol_int c s ] \]
       Return the symbol int value. 
       
       - {b Precondition}: get_symbol_kind s == INT_SYMBOL

       - {b See also}: {!Z3.mk_int_symbol}
    *)
external get_symbol_int : context -> symbol -> int
	= "camlidl_z3_Z3_get_symbol_int"

(**
       Summary: \[ [ get_symbol_string c s ] \]
       Return the symbol name. 

       - {b Precondition}: get_symbol_string s == STRING_SYMBOL

       
       
       

       - {b See also}: {!Z3.mk_string_symbol}
    *)
external get_symbol_string : context -> symbol -> string
	= "camlidl_z3_Z3_get_symbol_string"

(**
       Summary: Return TRUE if the two given AST nodes are equal.
    *)
external is_eq : context -> ast -> ast -> bool
	= "camlidl_z3_Z3_is_eq"

(**
       Summary: Return the kind of the given AST.
    *)
external get_ast_kind : context -> ast -> ast_kind
	= "camlidl_z3_Z3_get_ast_kind"

(**
       Summary: Return TRUE if the given AST is an expression.

       An expression is a constant, application, numeral, bound variable, or quantifier.
    *)
external is_expr : context -> ast -> bool
	= "camlidl_z3_Z3_is_expr"

(**
       Summary: Return the declaration of a constant or function application.
    *)
external get_const_ast_decl : context -> const_ast -> const_decl_ast
	= "camlidl_z3_Z3_get_const_ast_decl"

(**
       Summary: \[ [ get_const_ast_num_args c a ] \]
       Return the number of argument of an application. If t
       is an constant, then the number of arguments is 0.
    *)
external get_const_ast_num_args : context -> const_ast -> int
	= "camlidl_z3_Z3_get_const_ast_num_args"

(**
       Summary: \[ [ get_const_ast_arg c a i ] \]
       Return the i-th argument of the given application.
       
       - {b Precondition}: i < get_num_args c a
    *)
external get_const_ast_arg : context -> const_ast -> int -> ast
	= "camlidl_z3_Z3_get_const_ast_arg"

(**
       Summary: Return the constant declaration name as a symbol. 
    *)
external get_decl_name : context -> const_decl_ast -> symbol
	= "camlidl_z3_Z3_get_decl_name"

(**
       Summary: Return the type name as a symbol. 
    *)
external get_type_name : context -> type_ast -> symbol
	= "camlidl_z3_Z3_get_type_name"

(**
       Summary: Return the type of an AST node.
       
       The AST node must be a constant, application, numeral, bound variable, or quantifier.

       - {b See also}: {!Z3.is_expr}
    *)
external get_type : context -> ast -> type_ast
	= "camlidl_z3_Z3_get_type"

(**
       Summary: Return the number of parameters of the given declaration.

       - {b See also}: {!Z3.get_domain_size}
    *)
external get_domain_size : context -> const_decl_ast -> int
	= "camlidl_z3_Z3_get_domain_size"

(**
       Summary: \[ [ get_domain c d i ] \]
       Return the type of the i-th parameter of the given function declaration.
       
       - {b Precondition}: i < get_domain_size d

       - {b See also}: {!Z3.get_domain_size}
    *)
external get_domain : context -> const_decl_ast -> int -> type_ast
	= "camlidl_z3_Z3_get_domain"

(**
       Summary: \[ [ get_range c d ] \]
       Return the range of the given declaration. 

       If d is a constant (i.e., has zero arguments), then this
       function returns the type of the constant.
    *)
external get_range : context -> const_decl_ast -> type_ast
	= "camlidl_z3_Z3_get_range"

(**
       Summary: Return the type kind (e.g., array, tuple, int, bool, etc).

       - {b See also}: {!Z3.type_kind}
    *)
external get_type_kind : context -> type_ast -> type_kind
	= "camlidl_z3_Z3_get_type_kind"

(**
       Summary: \[ [ get_bv_type_size c t ] \]
       Return the size of the given bit-vector type. 

       - {b Precondition}: get_type_kind c t == BV_TYPE

       - {b See also}: {!Z3.mk_bv_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_bv_type_size : context -> type_ast -> int
	= "camlidl_z3_Z3_get_bv_type_size"

(**
       Summary: \[ [ get_array_type_domain c t ] \]
       Return the domain of the given array type.
       
       - {b Precondition}: get_type_kind c t == ARRAY_TYPE

       - {b See also}: {!Z3.mk_array_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_array_type_domain : context -> type_ast -> type_ast
	= "camlidl_z3_Z3_get_array_type_domain"

(**
       Summary: \[ [ get_array_type_range c t ] \] 
       Return the range of the given array type. 

       - {b Precondition}: get_type_kind c t == ARRAY_TYPE

       - {b See also}: {!Z3.mk_array_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_array_type_range : context -> type_ast -> type_ast
	= "camlidl_z3_Z3_get_array_type_range"

(**
       Summary: \[ [ get_tuple_type_mk_decl c t ] \]
       Return the constructor declaration of the given tuple
       type. 

       - {b Precondition}: get_type_kind c t == TUPLE_TYPE

       - {b See also}: {!Z3.mk_tuple_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_tuple_type_mk_decl : context -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_type_mk_decl"

(**
       Summary: \[ [ get_tuple_type_num_fields c t ] \]
       Return the number of fields of the given tuple type. 

       - {b Precondition}: get_type_kind c t == TUPLE_TYPE

        - {b Remarks}: Consider using the function {!Z3.get_tuple_type}, which 
       returns a tuple: tuple constructor, and an array of the tuple type fields. 

       - {b See also}: {!Z3.mk_tuple_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_tuple_type_num_fields : context -> type_ast -> int
	= "camlidl_z3_Z3_get_tuple_type_num_fields"

(**
       Summary: \[ [ get_tuple_type_field_decl c t i ] \]
       Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple type. 

        - {b Remarks}: Consider using the function {!Z3.get_tuple_type}, which 
       returns a tuple: tuple constructor, and an array of the tuple type fields. 

       - {b Precondition}: get_type_kind t == TUPLE_TYPE
       - {b Precondition}: i < get_tuple_type_num_fields c t i
       
       - {b See also}: {!Z3.mk_tuple_type}
       - {b See also}: {!Z3.get_type_kind}
    *)
external get_tuple_type_field_decl : context -> type_ast -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_type_field_decl"

(**
       Summary: Return declaration kind corresponding to declaration.
    *)
external get_decl_kind : context -> const_decl_ast -> decl_kind
	= "camlidl_z3_Z3_get_decl_kind"

(**
       Summary: Return index of de-Brujin bound variable.

       - {b Precondition}: get_type a == VAR_AST
    *)
external get_index_value : context -> ast -> int
	= "camlidl_z3_Z3_get_index_value"

(**
       Summary: Determine if quantifier is universal.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external is_quantifier_forall : context -> ast -> bool
	= "camlidl_z3_Z3_is_quantifier_forall"


(**
       Summary: Return number of patterns used in quantifier.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_num_patterns : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_patterns"

(**
       Summary: Return i'th pattern.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_pattern_ast : context -> ast -> int -> pattern_ast
	= "camlidl_z3_Z3_get_quantifier_pattern_ast"

(**
       Summary: Return symbol of the i'th bound variable.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_bound_name : context -> ast -> int -> symbol
	= "camlidl_z3_Z3_get_quantifier_bound_name"

(**
       Summary: Return type of the i'th bound variable.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_bound_type_ast : context -> ast -> int -> type_ast
	= "camlidl_z3_Z3_get_quantifier_bound_type_ast"

(**
       Summary: Return body of quantifier.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_body : context -> ast -> ast
	= "camlidl_z3_Z3_get_quantifier_body"

(**
       Summary: Return number of bound variables of quantifier.
       
       - {b Precondition}: get_type a == QUANTIFIER_AST
    *)
external get_quantifier_num_bound : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_bound"

(** 
        Summary: Return number of terms in pattern.
    *)
external get_pattern_num_terms : context -> pattern_ast -> int
	= "camlidl_z3_Z3_get_pattern_num_terms"

(**
       Summary: Return i'th ast in pattern.
    *)
external get_pattern_ast : context -> pattern_ast -> int -> ast
	= "camlidl_z3_Z3_get_pattern_ast"

(**
       {2 {L Coercions}}
    *)
(**
       Summary: Convert a TYPE_AST into an AST. This is just type casting.
    *)
external type_ast_to_ast : context -> type_ast -> ast
	= "camlidl_z3_Z3_type_ast_to_ast"

(**
       Summary: Convert a CONST_AST into an AST. This is just type casting.
    *)
external const_ast_to_ast : context -> const_ast -> ast
	= "camlidl_z3_Z3_const_ast_to_ast"

(**
       Summary: Convert a CONST_DECL_AST into an AST. This is just type casting.
    *)
external const_decl_ast_to_ast : context -> const_decl_ast -> ast
	= "camlidl_z3_Z3_const_decl_ast_to_ast"

(**
       Summary: Convert a PATTERN_AST into an AST. This is just type casting.
    *)
external pattern_ast_to_ast : context -> pattern_ast -> ast
	= "camlidl_z3_Z3_pattern_ast_to_ast"

(**
       Summary: Convert an AST into a CONST_AST. This is just type casting.
       
       - {b Warning}: This conversion is only safe if {!Z3.get_ast_kind} returns CONST_AST.
    *)
external to_const_ast : context -> ast -> const_ast
	= "camlidl_z3_Z3_to_const_ast"

(**
       Summary: Convert an AST into a NUMERAL_AST. This is just type casting.
       
       - {b Warning}: This conversion is only safe if {!Z3.get_ast_kind} returns NUMERAL_AST.
    *)
external to_numeral_ast : context -> ast -> numeral_ast
	= "camlidl_z3_Z3_to_numeral_ast"

(**
       {2 {L Constraints}}
    *)
(** 
        Summary: Create a backtracking point.
        
        The logical context can be viewed as a stack of contexts.  The
        scope level is the number of elements on this stack. The stack
        of contexts is simulated using trail (undo) stacks.

        - {b See also}: {!Z3.pop}
    *)
external push : context -> unit
	= "camlidl_z3_Z3_push"

(**
       Summary: Backtrack.
       
       Restores the context from the top of the stack, and pops it off the
       stack.  Any changes to the logical context (by {!Z3.assert_cnstr} or
       other functions) between the matching {!Z3.push} and pop
       operators are flushed, and the context is completely restored to
       what it was right before the {!Z3.push}.
       
       - {b See also}: {!Z3.push}
    *)
external pop : context -> int -> unit
	= "camlidl_z3_Z3_pop"

(**
       Summary: Assert a constraing into the logical context.
       
       After one assertion, the logical context may become
       inconsistent.  
       
       The functions {!Z3.check} or {!Z3.check_and_get_model} should be
       used to check whether the logical context is consistent or not.

       - {b See also}: {!Z3.check}
       - {b See also}: {!Z3.check_and_get_model}
    *)
external assert_cnstr : context -> ast -> unit
	= "camlidl_z3_Z3_assert_cnstr"

(**
       Summary: Check whether the given logical context is consistent or not.

       If the logical context is not unsatisfiable (i.e., the return value is different from L_FALSE)
       and model construction is enabled (see {!Z3.mk_config}), then a model is stored in m. Otherwise,
       the value 0 is stored in m.
       The caller is responsible for deleting the model using the function {!Z3.del_model}.
       
       - {b Remarks}: Model construction must be enabled using configuration
       parameters (See, {!Z3.mk_config}).

       - {b See also}: {!Z3.check}
       - {b See also}: {!Z3.del_model}
    *)
external check_and_get_model : context -> lbool * model
	= "camlidl_z3_Z3_check_and_get_model"

(**
       Summary: Check whether the given logical context is consistent or not.

       The function {!Z3.check_and_get_model} should be used when models are needed.

       - {b See also}: {!Z3.check_and_get_model}
    *)
external check : context -> lbool
	= "camlidl_z3_Z3_check"

(**
       Summary: Delete a model object.
       
       - {b See also}: {!Z3.check_and_get_model}
    *)
external del_model : model -> unit
	= "camlidl_z3_Z3_del_model"

(** 
        Summary: Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        It allows clients to piggyback on top of the AST simplifier
        for their own term manipulation.
    *)
external simplify : context -> ast -> ast
	= "camlidl_z3_Z3_simplify"



(**
       {2 {L Model navigation}}
     *)
(**
       Summary: Return the number of constants assigned by the given model.
       
        - {b Remarks}: Consider using {!Z3.get_model_constants}. 

       - {b See also}: {!Z3.get_model_constant}
    *)
external get_model_num_constants : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_constants"

(**
       Summary: \[ [ get_model_constant c m i ] \]
       Return the i-th constant in the given model. 

        - {b Remarks}: Consider using {!Z3.get_model_constants}. 

       - {b Precondition}: i < get_model_num_constants c m

       - {b See also}: {!Z3.get_value}
    *)
external get_model_constant : context -> model -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_model_constant"

(**
       Summary: Return the value of the given constant or application in the given model.
       
       - {b See also}: {!Z3.get_value_kind}
       - {b See also}: {!Z3.get_value_type}
    *)
external get_value : context -> model -> const_decl_ast -> value
	= "camlidl_z3_Z3_get_value"

(**
       Summary: Return the value type.
       
       - {b See also}: {!Z3.get_value}
    *)
external get_value_type : context -> value -> type_ast
	= "camlidl_z3_Z3_get_value_type"

(**
       Summary: Return the value kind (numeral, array, tuple, etc). 
       
       NUMERAL_VALUE stores the value of different types (int, real, bv, and uninterpreted).
    *)
external get_value_kind : context -> value -> value_kind
	= "camlidl_z3_Z3_get_value_kind"

(**
       Summary: \[ [ get_numeral_value_string c v ] \]
       Return a numeral value as a string. The representation is stored in decimal notation.

       - {b Precondition}: get_value_kind c v == NUMERAL_VALUE
       
       
       
       

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_numeral_value_string : context -> value -> string
	= "camlidl_z3_Z3_get_numeral_value_string"

(**
       Summary: \[ [ get_numeral_value_int c v ] \]
       Similar to {!Z3.get_numeral_value_string}, but only succeeds if
       the value can fit in a machine int. Return TRUE if the call succeeded.

       - {b Precondition}: get_value_kind c v == NUMERAL_VALUE
      
       - {b See also}: {!Z3.get_numeral_value_string}
    *)
external get_numeral_value_int : context -> value -> bool * int
	= "camlidl_z3_Z3_get_numeral_value_int"

(**
       Summary: \[ [ get_numeral_value_uint c v ] \]
       Similar to {!Z3.get_numeral_value_string}, but only succeeds if
       the value can fit in a machine unsigned int int. Return TRUE if the call succeeded.

       - {b Precondition}: get_value_kind c v == NUMERAL_VALUE
      
       - {b See also}: {!Z3.get_numeral_value_string}
    *)
external get_numeral_value_uint : context -> value -> bool * int
	= "camlidl_z3_Z3_get_numeral_value_uint"

(**
       Summary: \[ [ get_bool_value_bool c v ] \]
       Return the value of v as a Boolean value.

       - {b Precondition}: get_value_kind c v == BOOL_VALUE

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_bool_value_bool : context -> value -> bool
	= "camlidl_z3_Z3_get_bool_value_bool"

(**
       Summary: \[ [ get_tuple_value_mk_decl c v ] \]
       Return the constructor declaration of the given tuple
       value. 

        - {b Remarks}: Consider using {!Z3.get_tuple_value}. 
       
       - {b Precondition}: get_value_kind c v == TUPLE_VALUE

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_tuple_value_mk_decl : context -> value -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_value_mk_decl"

(**    
       Summary: \[ [ get_tuple_value_num_fields c v ] \]
       Return the number of fields of the given tuple value. 

        - {b Remarks}: Consider using {!Z3.get_tuple_value}. 

       - {b Precondition}: get_value_kind c t == TUPLE_VALUE

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_tuple_value_num_fields : context -> value -> int
	= "camlidl_z3_Z3_get_tuple_value_num_fields"

(**
       Summary: \[ [ get_tuple_value_field c v i ] \]
       Return the i-th field of the given tuple value.

        - {b Remarks}: Consider using {!Z3.get_tuple_value}. 

       - {b Precondition}: get_value_kind v == TUPLE_VALUE
       - {b Precondition}: i < get_tuple_value_num_fields c v
       
       - {b See also}: {!Z3.get_value_kind}
    *)
external get_tuple_value_field : context -> value -> int -> value
	= "camlidl_z3_Z3_get_tuple_value_field"

(**
       Summary: \[ [ get_array_value_size c v ] \]
       An array values is represented as a dictionary plus a
       default (else) value. This function returns the size of the
       dictionary. 

        - {b Remarks}: Consider using {!Z3.get_array_value}. 

       - {b Precondition}: get_value_kind v == ARRAY_VALUE

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_array_value_size : context -> value -> int
	= "camlidl_z3_Z3_get_array_value_size"

(**
       Summary: \[ [ get_array_value_else c v ] \]
       An array values is represented as a dictionary plus a
       default (else) value. This function returns the default (else) value.

        - {b Remarks}: Consider using {!Z3.get_array_value}. 
       
       - {b Precondition}: get_value_kind v == ARRAY_VALUE

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_array_value_else : context -> value -> value
	= "camlidl_z3_Z3_get_array_value_else"

(**
       Summary: \[ [ get_array_value_entry_index c v i ] \]
       An array values is represented as a dictionary plus a
       default (else) value. Each dictionary entry is a pair (index, value).
       This function return the i-th index of the array. 

       If v contains an entry (index, value), then 
       
        {e v\[index\] = value}. 

        - {b Remarks}: Consider using {!Z3.get_array_value}. 

       - {b Precondition}: get_value_kind v == ARRAY_VALUE
       - {b Precondition}: i < get_array_value_size c v

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_array_value_entry_index : context -> value -> int -> value
	= "camlidl_z3_Z3_get_array_value_entry_index"

(**
       Summary: \[ [ get_array_value_entry_value c v i ] \]
       An array values is represented as a dictionary plus a
       default (else) value. Each dictionary entry is a pair (index, value).
       This function return the i-th value of the array. 
       
       If v contains an entry (index, value), then 
       
        {e v\[index\] = value}. 

        - {b Remarks}: Consider using {!Z3.get_array_value}. 

       - {b Precondition}: get_value_kind v == ARRAY_VALUE
       - {b Precondition}: i < get_array_value_size c v

       - {b See also}: {!Z3.get_value_kind}
    *)
external get_array_value_entry_value : context -> value -> int -> value
	= "camlidl_z3_Z3_get_array_value_entry_value"

(**
       Summary: Return the number of function interpretations in the given model.
       
       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 
       
       - {b See also}: {!Z3.get_model_func_decl}
       - {b See also}: {!Z3.get_model_func_else}
       - {b See also}: {!Z3.get_model_func_num_entries}
       - {b See also}: {!Z3.get_model_func_entry_num_args}
       - {b See also}: {!Z3.get_model_func_entry_arg}
     *)
external get_model_num_funcs : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_funcs"

(**
       Summary: \[ [ is_model_func_internal c m i ] \]
       Return true if the i-th function in the model is \em internal.

       Internal functions are created by Z3, and their interpretations
       are not usually useful for users.

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!Z3.get_model_num_funcs}
    *)
external is_model_func_internal : context -> model -> int -> bool
	= "camlidl_z3_Z3_is_model_func_internal"

(**
       Summary: \[ [ get_model_func_decl c m i ] \]
       Return the declaration of the i-th function in the given model.

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!Z3.get_model_num_funcs}
    *)
external get_model_func_decl : context -> model -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_model_func_decl"

(**
       Summary: \[ [ get_model_func_else c m i ] \]
       Return the 'else' value of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 
       
       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!Z3.get_model_num_funcs}
       - {b See also}: {!Z3.get_model_func_num_entries}
       - {b See also}: {!Z3.get_model_func_entry_num_args}
       - {b See also}: {!Z3.get_model_func_entry_arg}
    *)
external get_model_func_else : context -> model -> int -> value
	= "camlidl_z3_Z3_get_model_func_else"

(**
       Summary: \[ [ get_model_func_num_entries c m i ] \]
       Return the number of entries of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 
       
       - {b Precondition}: i < get_model_num_funcs c m

       - {b See also}: {!Z3.get_model_num_funcs}
       - {b See also}: {!Z3.get_model_func_else}
       - {b See also}: {!Z3.get_model_func_entry_num_args}
       - {b See also}: {!Z3.get_model_func_entry_arg}
    *)
external get_model_func_num_entries : context -> model -> int -> int
	= "camlidl_z3_Z3_get_model_func_num_entries"

(**
       Summary: \[ [ get_model_func_entry_num_args c m i j ] \]
       Return the number of arguments of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i

       - {b See also}: {!Z3.get_model_num_funcs}
       - {b See also}: {!Z3.get_model_func_num_entries }
       - {b See also}: {!Z3.get_model_func_entry_arg}
    *)
external get_model_func_entry_num_args : context -> model -> int -> int -> int
	= "camlidl_z3_Z3_get_model_func_entry_num_args"

(**
       Summary: \[ [ get_model_func_entry_arg c m i j k ] \]
       Return the k-th argument of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i
       - {b Precondition}: k < get_model_func_entry_num_args c m i j

       - {b See also}: {!Z3.get_model_num_funcs}
       - {b See also}: {!Z3.get_model_func_num_entries }
       - {b See also}: {!Z3.get_model_func_entry_num_args}
    *)
external get_model_func_entry_arg : context -> model -> int -> int -> int -> value
	= "camlidl_z3_Z3_get_model_func_entry_arg"

(**
       Summary: \[ [ get_model_func_entry_value c m i j ] \]
       Return the return value of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       

        - {b Remarks}: Consider using {!Z3.get_model_funcs}. 

       - {b Precondition}: i < get_model_num_funcs c m
       - {b Precondition}: j < get_model_func_num_entries c m i

       - {b See also}: {!Z3.get_model_num_funcs}
       - {b See also}: {!Z3.get_model_func_num_entries }
    *)
external get_model_func_entry_value : context -> model -> int -> int -> value
	= "camlidl_z3_Z3_get_model_func_entry_value"

(**
       Summary: \[ [ eval c m t ] \]
       Evaluate the AST node t in the given model. 
       
        Return a pair: Boolean and value. The Boolean is true if the term was successfully evaluated. 

       The evaluation may fail for the following reasons:

       - t contains a quantifier or bound variable. 

       - the model m is partial, that is, it doesn't have a complete interpretation for free functions. That is, the option {e PARTIAL_MODELS=true } was used.

       - the evaluator doesn't have support for some interpreted operator.

       - t is type incorrect (see {!Z3.type_check}).

       - The result of an intepreted operator in t is undefined (e.g. division by zero).
    *)
external eval : context -> model -> ast -> bool * value
	= "camlidl_z3_Z3_eval"

(**
       {2 {L Timers}}
    *)
(**
       Summary: \[ [ set_soft_timeout c t ] \]
       Set a soft timeout in seconds. 

       A soft timeout limits the amount of time spent in
       {!Z3.check_and_get_model} and {!Z3.check}.  If a call to
       {!Z3.check_and_get_model} or {!Z3.check} consumes more than t 
       seconds, then it aborts and returns L_UNDEF.

       - {b See also}: {!Z3.reset_soft_timeout}
    *)
external set_soft_timeout : context -> int -> unit
	= "camlidl_z3_Z3_set_soft_timeout"

(**
       Summary: Disable soft timeouts.
       
       - {b See also}: {!Z3.set_soft_timeout}
    *)
external reset_soft_timeout : context -> unit
	= "camlidl_z3_Z3_reset_soft_timeout"

(**
       {2 {L Interaction logging.}}
    *)
(**
       Summary: Log interaction to a file.
    *)
external open_log : context -> string -> bool
	= "camlidl_z3_Z3_open_log"

(**
       Summary: Close interaction log.
    *)
external close_log : context -> unit
	= "camlidl_z3_Z3_close_log"

(**
       {2 {L String conversion}}
    *)
(**
       Summary: Convert the given AST node into a string.

       
       
       
    *)
external ast_to_string : context -> ast -> string
	= "camlidl_z3_Z3_ast_to_string"

(**
       Summary: Convert the given model into a string.

       
       
       
    *)
external model_to_string : context -> model -> string
	= "camlidl_z3_Z3_model_to_string"

(**
       Summary: Convert the given (model) value into a string.

       
       
       
    *)
external value_to_string : context -> value -> string
	= "camlidl_z3_Z3_value_to_string"

(**
       Summary: Convert the given logical context into a string.
       
       This function is mainly used for debugging purposes. It displays
       the internal structure of a logical context.

       
       
       
    *)
external context_to_string : context -> string
	= "camlidl_z3_Z3_context_to_string"

(**
       {2 {L Parser interface}}
    *)
(**
       Summary: \[ [ parse_smtlib_string c str type_names types decl_names decls ] \]
       Parse the given string using the SMT-LIB parser. 
              
       The symbol table of the parser can be initialized using the given types and declarations. 
       The symbols in the arrays type_names and decl_names don't need to match the names
       of the types and declarations in the arrays types and decls. This is an useful feature
       since we can use arbitrary names to reference types and declarations defined using the C API.

       The formulas, assumptions and declarations defined in str can be extracted using the functions:
       {!Z3.get_smtlib_num_formulas}, {!Z3.get_smtlib_formula}, {!Z3.get_smtlib_num_assumptions}, {!Z3.get_smtlib_assumption}, 
       {!Z3.get_smtlib_num_decls}, and {!Z3.get_smtlib_decl}.
     *)
external parse_smtlib_string : context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> unit
	= "camlidl_z3_Z3_parse_smtlib_string_bytecode" "camlidl_z3_Z3_parse_smtlib_string"

(**
       Summary: Similar to {!Z3.parse_smtlib_string}, but reads the benchmark from a file.
    *)
external parse_smtlib_file : context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> unit
	= "camlidl_z3_Z3_parse_smtlib_file_bytecode" "camlidl_z3_Z3_parse_smtlib_file"

(**
       Summary: Return the number of SMTLIB formulas parsed by the last call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.
    *)
external get_smtlib_num_formulas : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_formulas"

(**
       Summary: \[ [ get_smtlib_formula c i ] \]
       Return the i-th formula parsed by the last call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_formulas c
    *)
external get_smtlib_formula : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_formula"

(**
       Summary: Return the number of SMTLIB assumptions parsed by {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.
    *)
external get_smtlib_num_assumptions : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_assumptions"

(**
       Summary: \[ [ get_smtlib_assumption c i ] \]
       Return the i-th assumption parsed by the last call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_assumptions c
    *)
external get_smtlib_assumption : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_assumption"

(**
       Summary: Return the number of declarations parsed by {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.
    *)
external get_smtlib_num_decls : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_decls"

(**
       Summary: \[ [ get_smtlib_decl c i ] \]
       Return the i-th declaration parsed by the last call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

       - {b Precondition}: i < get_smtlib_num_decls c
    *)
external get_smtlib_decl : context -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_smtlib_decl"

(**
       {2 {L Miscellaneous}}
    *)
(**
       Summary: Return Z3 version number information.
    *)
external get_version : unit -> int * int * int * int
	= "camlidl_z3_Z3_get_version"

(**
       Summary: \[ [ type_check c t ] \]
       Return TRUE if t is type correct.
    *)
external type_check : context -> ast -> bool
	= "camlidl_z3_Z3_type_check"




(** {2 {L ML Extensions}} *)

(**
  \[ [ mk_context_x configs] \] is a shorthand for the context with configurations in [configs].
*)
val mk_context_x: (string * string) array -> context;;

(**
  \[ [ get_const_ast_args c a ] \] is the array of arguments of an application. If [t] is a constant, then the array is empty.

  - {b See also}: {!Z3.get_const_ast_num_args}
  - {b See also}: {!Z3.get_const_ast_arg}
*)
val get_const_ast_args:  context -> const_ast -> ast array

(**
  \[ [ get_const_ast_args c d ] \] is the array of parameters of [d].

  - {b See also}: {!Z3.get_domain_size}
  - {b See also}: {!Z3.get_domain}
*)
val get_domains: context -> const_decl_ast -> type_ast array

(**
  \[ [ get_array_type c t ] \] is the domain and the range of [t].

  - {b See also}: {!Z3.get_array_type_domain}
  - {b See also}: {!Z3.get_array_type_range}
*)
val get_array_type: context -> type_ast -> type_ast * type_ast

(**
  \[ [ get_tuple_type c ty ] \] is the pair [(mk_decl, fields)] where [mk_decl] is the constructor declaration of [ty], and [fields] is the array of fields in [ty].

  - {b See also}: {!Z3.get_tuple_type_mk_decl}
  - {b See also}: {!Z3.get_tuple_type_num_fields}
  - {b See also}: {!Z3.get_tuple_type_field_decl}
*)
val get_tuple_type: context -> type_ast -> (const_decl_ast * const_decl_ast array)

(**
  \[ [ get_model_constants c m ] \] is the array of constants in the model [m].

  - {b See also}: {!Z3.get_model_num_constants}
  - {b See also}: {!Z3.get_model_constant}
*)
val get_model_constants: context -> model -> const_decl_ast array

(**
  \[ [ get_tuple_value c v ] \] is the array of fields in the tuple [v].

  - {b See also}: {!Z3.get_tuple_value_num_fields}
  - {b See also}: {!Z3.get_tuple_value_field}
*)
val get_tuple_value: context -> value -> value array

(**
  \[ [ get_array_value c v ] \] is the pair [(dictionary, else)] where [dictionary] is the array of dictionary entries in [v], and [else] is the default (else) value of [v].

  - {b See also}: {!Z3.get_array_value_size}
  - {b See also}: {!Z3.get_array_value_entry_index}
  - {b See also}: {!Z3.get_array_value_entry_value}
  - {b See also}: {!Z3.get_array_value_else}
*)
val get_array_value: context -> value -> (value * value) array * value

(**
  \[ [ get_model_func_entry c m i j ] \] is the [j]'th entry in the [i]'th function in the model [m].

  - {b See also}: {!Z3.get_model_func_entry_num_args}
  - {b See also}: {!Z3.get_model_func_entry_arg}
  - {b See also}: {!Z3.get_model_func_entry_value}
*)
val get_model_func_entry: context -> model -> int -> int -> (value array * value);;

(**
  \[ [ get_model_func_entries c m i ] \] is the array of entries in the [i]'th function in the model [m].

  - {b See also}: {!Z3.get_model_func_num_entries}
  - {b See also}: {!Z3.get_model_func_entry}
*)
val get_model_func_entries: context -> model -> int -> (value array * value) array;;

(**
  \[ [ get_model_funcs c m ] \] is the array of functions in the model [m]. Each function is represented by the quadruple [(internal, decl, entries, else)], where [internal] is true iff the function is internal to Z3, [decl] is the declaration name for the function, [entries] is the array of entries in the function, and [else] is the default (else) value for the function.

  - {b See also}: {!Z3.get_model_num_funcs}
  - {b See also}: {!Z3.is_model_func_internal}
  - {b See also}: {!Z3.get_model_func_decl}
  - {b See also}: {!Z3.get_model_func_entries}
  - {b See also}: {!Z3.get_model_func_else}
*)
val get_model_funcs: context -> model -> 
  (bool * 
   symbol *
   (value array * value) array * 
   value) array

(**
  \[ [ get_smtlib_formulas c ] \] is the array of formulas created by a preceding call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

  Recommend use {!Z3.parse_smtlib_string_x} or {!Z3.parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.

  - {b See also}: {!Z3.parse_smtlib_string_x}
  - {b See also}: {!Z3.parse_smtlib_file_x}
  - {b See also}: {!Z3.parse_smtlib_string}
  - {b See also}: {!Z3.parse_smtlib_file}
  - {b See also}: {!Z3.get_smtlib_num_formulas}
  - {b See also}: {!Z3.get_smtlib_formula}
*)
val get_smtlib_formulas: context -> ast array

(**
  \[ [get_smtlib_assumptions c] \] is the array of assumptions created by a preceding call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

  Recommend use {!Z3.parse_smtlib_string_x} or {!Z3.parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!Z3.parse_smtlib_string_x}
  - {b See also}: {!Z3.parse_smtlib_file_x}
  - {b See also}: {!Z3.parse_smtlib_string}
  - {b See also}: {!Z3.parse_smtlib_file}
  - {b See also}: {!Z3.get_smtlib_num_assumptions}
  - {b See also}: {!Z3.get_smtlib_assumption}
*)
val get_smtlib_assumptions: context -> ast array

(**
  \[ [ get_smtlib_decls c ] \] is the array of declarations created by a preceding call to {!Z3.parse_smtlib_string} or {!Z3.parse_smtlib_file}.

  Recommend use {!Z3.parse_smtlib_string_x} or {!Z3.parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!Z3.parse_smtlib_string_x}
  - {b See also}: {!Z3.parse_smtlib_file_x}
  - {b See also}: {!Z3.parse_smtlib_string}
  - {b See also}: {!Z3.parse_smtlib_file}
  - {b See also}: {!Z3.get_smtlib_num_decls}
  - {b See also}: {!Z3.get_smtlib_decl}
*)
val get_smtlib_decls: context -> const_decl_ast array

(**
  \[ [ get_smtlib_parse_results c ] \] is the triple [(get_smtlib_formulas c, get_smtlib_assumptions c, get_smtlib_decls c)].

  Recommend use {!Z3.parse_smtlib_string_x} or {!Z3.parse_smtlib_file_x} for functional style interface to the SMT-LIB parser.


  - {b See also}: {!Z3.parse_smtlib_string_x}
  - {b See also}: {!Z3.parse_smtlib_file_x}
  - {b See also}: {!Z3.parse_smtlib_string}
  - {b See also}: {!Z3.parse_smtlib_file}
  - {b See also}: {!Z3.get_smtlib_formulas}
  - {b See also}: {!Z3.get_smtlib_assumptions}
  - {b See also}: {!Z3.get_smtlib_decls}
*)
val get_smtlib_parse_results: context -> (ast array * ast array * const_decl_ast array)

(**
  \[ [ parse_smtlib_string_formula c ... ] \] calls [(parse_smtlib_string c ...)] and returns the single formula produced. 

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!Z3.parse_smtlib_file_formula}
  - {b See also}: {!Z3.parse_smtlib_string_x}
*)
val parse_smtlib_string_formula: context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> ast

(**
  \[ [ parse_smtlib_file_formula c ... ] \] calls [(parse_smtlib_file c ...)] and returns the single formula produced. 

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!Z3.parse_smtlib_file_formula}
  - {b See also}: {!Z3.parse_smtlib_file_x}
*)
val parse_smtlib_file_formula: context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> ast

(**
  \[ [ parse_smtlib_string_x c ... ] \] is [(parse_smtlib_string c ...; get_smtlib_parse_results c)]

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!Z3.parse_smtlib_file_x}
  - {b See also}: {!Z3.parse_smtlib_string}
  - {b See also}: {!Z3.get_smtlib_parse_results}
*)
val parse_smtlib_string_x: context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> (ast array * ast array * const_decl_ast array)

(**
  \[ [ parse_smtlib_file_x c ... ] \] is [(parse_smtlib_file c ...; get_smtlib_parse_results c)]

  Recommended for functional style interface to the SMT-LIB parser.

  - {b See also}: {!Z3.parse_smtlib_string_x}
  - {b See also}: {!Z3.parse_smtlib_file}
  - {b See also}: {!Z3.get_smtlib_parse_results}
*)
val parse_smtlib_file_x: context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> (ast array * ast array * const_decl_ast array)

(**
  \[ [ symbol_refined ] \] is the refinement of a {!Z3.symbol} .

  - {b See also}: {!Z3.symbol_refine}
  - {b See also}: {!Z3.get_symbol_kind}
*)
type symbol_refined =
  | Symbol_int of int
  | Symbol_string of string
  | Symbol_unknown;;

(**
  \[ [ symbol_refine c s ] \] is the refined symbol of [s].

  - {b See also}:  {!Z3.symbol_refined}
  - {b See also}: {!Z3.get_symbol_kind}
*)
val symbol_refine: context -> symbol -> symbol_refined;;

(**
  \[ [ type_refined ] \] is the refinement of a {!Z3.type_ast} .

  - {b See also}: {!Z3.type_refine}
  - {b See also}: {!Z3.get_type_kind}
*)
type type_refined =
  | Type_uninterpreted of symbol
  | Type_bool
  | Type_int
  | Type_real
  | Type_bv of int
  | Type_array of (type_ast * type_ast)
  | Type_tuple of (const_decl_ast * const_decl_ast array)
  | Type_unknown of symbol

(**
  \[ [ type_refine c t ] \] is the refined type of [t].

  - {b See also}:  {!Z3.type_refined}
  - {b See also}: {!Z3.get_type_kind}
*)
val type_refine: context -> type_ast -> type_refined;;

(**
  \[ [ binder_type ] \] is a universal or existential quantifier.

  - {b See also}: {!Z3.term_refined}
*)
type binder_type = | Forall | Exists 

(**
  \[ [ numeral_refined ] \] is the refinement of a numeral .

  Numerals whose fractional representation can be fit with
  64 bit integers are treated as small.

*)
type numeral_refined = 
  | Numeral_small  of int64 * int64
  | Numeral_large  of string

(**
  \[ [ term_refined ] \] is the refinement of a {!Z3.ast} .

  - {b See also}: {!Z3.term_refine}
*)
type term_refined = 
  | Term_app        of decl_kind * const_decl_ast * ast array
  | Term_quantifier of binder_type * int * ast array array * (symbol *type_ast) array * ast
  | Term_numeral    of numeral_refined * type_ast
  | Term_var        of int * type_ast


(**
  \[ [ value_refined ] \] is the refinement of a {!Z3.value} .

  - {b See also}: {!Z3.value_refine}
  - {b See also}: {!Z3.get_value_kind}
*)
type value_refined =
  | Value_bool of bool
  | Value_numeral of string * type_ast
  | Value_array of ((value * value) array * value)
  | Value_tuple of value array
  | Value_unknown;;

(**
  \[ [ value_refine c v ] \] is the refined value of [v].

  - {b See also}:  {!Z3.value_refined}
  - {b See also}: {!Z3.get_value_kind}
*)
val value_refine: context -> value -> value_refined;;


