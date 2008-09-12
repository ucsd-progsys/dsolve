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

external mk_config : unit -> config
	= "camlidl_z3_Z3_mk_config"

external del_config : config -> unit
	= "camlidl_z3_Z3_del_config"

external set_param_value : config -> string -> string -> unit
	= "camlidl_z3_Z3_set_param_value"

external mk_context : config -> context
	= "camlidl_z3_Z3_mk_context"

external del_context : context -> unit
	= "camlidl_z3_Z3_del_context"

external trace_to_file : context -> string -> bool
	= "camlidl_z3_Z3_trace_to_file"

external trace_to_stderr : context -> unit
	= "camlidl_z3_Z3_trace_to_stderr"

external trace_to_stdout : context -> unit
	= "camlidl_z3_Z3_trace_to_stdout"

external trace_off : context -> unit
	= "camlidl_z3_Z3_trace_off"

external enable_arithmetic : context -> unit
	= "camlidl_z3_Z3_enable_arithmetic"

external enable_bv : context -> unit
	= "camlidl_z3_Z3_enable_bv"

external enable_arrays : context -> unit
	= "camlidl_z3_Z3_enable_arrays"

external enable_tuples : context -> unit
	= "camlidl_z3_Z3_enable_tuples"

external mk_int_symbol : context -> int -> symbol
	= "camlidl_z3_Z3_mk_int_symbol"

external mk_string_symbol : context -> string -> symbol
	= "camlidl_z3_Z3_mk_string_symbol"

external mk_uninterpreted_type : context -> symbol -> type_ast
	= "camlidl_z3_Z3_mk_uninterpreted_type"

external mk_bool_type : context -> type_ast
	= "camlidl_z3_Z3_mk_bool_type"

external mk_int_type : context -> type_ast
	= "camlidl_z3_Z3_mk_int_type"

external mk_real_type : context -> type_ast
	= "camlidl_z3_Z3_mk_real_type"

external mk_bv_type : context -> int -> type_ast
	= "camlidl_z3_Z3_mk_bv_type"

external mk_array_type : context -> type_ast -> type_ast -> type_ast
	= "camlidl_z3_Z3_mk_array_type"

external mk_tuple_type : context -> symbol -> symbol array -> type_ast array -> type_ast * const_decl_ast * const_decl_ast array
	= "camlidl_z3_Z3_mk_tuple_type"

external mk_func_decl : context -> symbol -> type_ast array -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_mk_func_decl"

external mk_app : context -> const_decl_ast -> ast array -> ast
	= "camlidl_z3_Z3_mk_app"

external mk_const : context -> symbol -> type_ast -> ast
	= "camlidl_z3_Z3_mk_const"

external mk_label : context -> symbol -> bool -> ast -> ast
	= "camlidl_z3_Z3_mk_label"

external mk_fresh_func_decl : context -> string -> type_ast array -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_mk_fresh_func_decl"

external mk_fresh_const : context -> string -> type_ast -> ast
	= "camlidl_z3_Z3_mk_fresh_const"

external mk_true : context -> ast
	= "camlidl_z3_Z3_mk_true"

external mk_false : context -> ast
	= "camlidl_z3_Z3_mk_false"

external mk_eq : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_eq"

external mk_distinct : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_distinct"

external mk_not : context -> ast -> ast
	= "camlidl_z3_Z3_mk_not"

external mk_ite : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ite"

external mk_iff : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_iff"

external mk_implies : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_implies"

external mk_xor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_xor"

external mk_and : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_and"

external mk_or : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_or"

external mk_add : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_add"

external mk_mul : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_mul"

external mk_sub : context -> ast array -> ast
	= "camlidl_z3_Z3_mk_sub"

external mk_lt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_lt"

external mk_le : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_le"

external mk_gt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_gt"

external mk_ge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_ge"

external mk_bvnot : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnot"

external mk_bvand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvand"

external mk_bvor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvor"

external mk_bvxor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxor"

external mk_bvnand : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnand"

external mk_bvnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvnor"

external mk_bvxnor : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvxnor"

external mk_bvneg : context -> ast -> ast
	= "camlidl_z3_Z3_mk_bvneg"

external mk_bvadd : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvadd"

external mk_bvsub : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsub"

external mk_bvmul : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvmul"

external mk_bvudiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvudiv"

external mk_bvsdiv : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsdiv"

external mk_bvurem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvurem"

external mk_bvsrem : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsrem"

external mk_bvsmod : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsmod"

external mk_bvult : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvult"

external mk_bvslt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvslt"

external mk_bvule : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvule"

external mk_bvsle : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsle"

external mk_bvuge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvuge"

external mk_bvsge : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsge"

external mk_bvugt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvugt"

external mk_bvsgt : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvsgt"

external mk_concat : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_concat"

external mk_extract : context -> int -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_extract"

external mk_sign_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_sign_ext"

external mk_zero_ext : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_zero_ext"

external mk_bvshl : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvshl"

external mk_bvlshr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvlshr"

external mk_bvashr : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_bvashr"

external mk_rotate_left : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_left"

external mk_rotate_right : context -> int -> ast -> ast
	= "camlidl_z3_Z3_mk_rotate_right"

external mk_select : context -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_select"

external mk_store : context -> ast -> ast -> ast -> ast
	= "camlidl_z3_Z3_mk_store"

external mk_numeral : context -> string -> type_ast -> ast
	= "camlidl_z3_Z3_mk_numeral"

external mk_int : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_int"

external mk_unsigned_int : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_unsigned_int"

external mk_pattern : context -> ast array -> pattern_ast
	= "camlidl_z3_Z3_mk_pattern"

external mk_bound : context -> int -> type_ast -> ast
	= "camlidl_z3_Z3_mk_bound"

external mk_forall : context -> int -> pattern_ast array -> type_ast array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_forall_bytecode" "camlidl_z3_Z3_mk_forall"

external mk_exists : context -> int -> pattern_ast array -> type_ast array -> symbol array -> ast -> ast
	= "camlidl_z3_Z3_mk_exists_bytecode" "camlidl_z3_Z3_mk_exists"

external mk_quantifier : context -> bool -> int -> int -> pattern_ast -> int -> ast -> int -> type_ast -> symbol -> ast -> ast
	= "camlidl_z3_Z3_mk_quantifier_bytecode" "camlidl_z3_Z3_mk_quantifier"

external get_symbol_kind : context -> symbol -> symbol_kind
	= "camlidl_z3_Z3_get_symbol_kind"

external get_symbol_int : context -> symbol -> int
	= "camlidl_z3_Z3_get_symbol_int"

external get_symbol_string : context -> symbol -> string
	= "camlidl_z3_Z3_get_symbol_string"

external is_eq : context -> ast -> ast -> bool
	= "camlidl_z3_Z3_is_eq"

external get_ast_kind : context -> ast -> ast_kind
	= "camlidl_z3_Z3_get_ast_kind"

external is_expr : context -> ast -> bool
	= "camlidl_z3_Z3_is_expr"

external get_const_ast_decl : context -> const_ast -> const_decl_ast
	= "camlidl_z3_Z3_get_const_ast_decl"

external get_const_ast_num_args : context -> const_ast -> int
	= "camlidl_z3_Z3_get_const_ast_num_args"

external get_const_ast_arg : context -> const_ast -> int -> ast
	= "camlidl_z3_Z3_get_const_ast_arg"

external get_decl_name : context -> const_decl_ast -> symbol
	= "camlidl_z3_Z3_get_decl_name"

external get_type_name : context -> type_ast -> symbol
	= "camlidl_z3_Z3_get_type_name"

external get_type : context -> ast -> type_ast
	= "camlidl_z3_Z3_get_type"

external get_domain_size : context -> const_decl_ast -> int
	= "camlidl_z3_Z3_get_domain_size"

external get_domain : context -> const_decl_ast -> int -> type_ast
	= "camlidl_z3_Z3_get_domain"

external get_range : context -> const_decl_ast -> type_ast
	= "camlidl_z3_Z3_get_range"

external get_type_kind : context -> type_ast -> type_kind
	= "camlidl_z3_Z3_get_type_kind"

external get_bv_type_size : context -> type_ast -> int
	= "camlidl_z3_Z3_get_bv_type_size"

external get_array_type_domain : context -> type_ast -> type_ast
	= "camlidl_z3_Z3_get_array_type_domain"

external get_array_type_range : context -> type_ast -> type_ast
	= "camlidl_z3_Z3_get_array_type_range"

external get_tuple_type_mk_decl : context -> type_ast -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_type_mk_decl"

external get_tuple_type_num_fields : context -> type_ast -> int
	= "camlidl_z3_Z3_get_tuple_type_num_fields"

external get_tuple_type_field_decl : context -> type_ast -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_type_field_decl"

external get_decl_kind : context -> const_decl_ast -> decl_kind
	= "camlidl_z3_Z3_get_decl_kind"

external get_numeral_ast_value : context -> ast -> string
	= "camlidl_z3_Z3_get_numeral_ast_value"

external get_numeral_ast_value_small : context -> ast -> bool * int64 * int64
	= "camlidl_z3_Z3_get_numeral_ast_value_small"

external get_index_value : context -> ast -> int
	= "camlidl_z3_Z3_get_index_value"

external is_quantifier_forall : context -> ast -> bool
	= "camlidl_z3_Z3_is_quantifier_forall"

external get_quantifier_weight : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_weight"

external get_quantifier_num_patterns : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_patterns"

external get_quantifier_pattern_ast : context -> ast -> int -> pattern_ast
	= "camlidl_z3_Z3_get_quantifier_pattern_ast"

external get_quantifier_bound_name : context -> ast -> int -> symbol
	= "camlidl_z3_Z3_get_quantifier_bound_name"

external get_quantifier_bound_type_ast : context -> ast -> int -> type_ast
	= "camlidl_z3_Z3_get_quantifier_bound_type_ast"

external get_quantifier_body : context -> ast -> ast
	= "camlidl_z3_Z3_get_quantifier_body"

external get_quantifier_num_bound : context -> ast -> int
	= "camlidl_z3_Z3_get_quantifier_num_bound"

external get_pattern_num_terms : context -> pattern_ast -> int
	= "camlidl_z3_Z3_get_pattern_num_terms"

external get_pattern_ast : context -> pattern_ast -> int -> ast
	= "camlidl_z3_Z3_get_pattern_ast"

external type_ast_to_ast : context -> type_ast -> ast
	= "camlidl_z3_Z3_type_ast_to_ast"

external const_ast_to_ast : context -> const_ast -> ast
	= "camlidl_z3_Z3_const_ast_to_ast"

external const_decl_ast_to_ast : context -> const_decl_ast -> ast
	= "camlidl_z3_Z3_const_decl_ast_to_ast"

external pattern_ast_to_ast : context -> pattern_ast -> ast
	= "camlidl_z3_Z3_pattern_ast_to_ast"

external to_const_ast : context -> ast -> const_ast
	= "camlidl_z3_Z3_to_const_ast"

external to_numeral_ast : context -> ast -> numeral_ast
	= "camlidl_z3_Z3_to_numeral_ast"

external push : context -> unit
	= "camlidl_z3_Z3_push"

external pop : context -> int -> unit
	= "camlidl_z3_Z3_pop"

external assert_cnstr : context -> ast -> unit
	= "camlidl_z3_Z3_assert_cnstr"

external check_and_get_model : context -> lbool * model
	= "camlidl_z3_Z3_check_and_get_model"

external check : context -> lbool
	= "camlidl_z3_Z3_check"

external del_model : model -> unit
	= "camlidl_z3_Z3_del_model"

external simplify : context -> ast -> ast
	= "camlidl_z3_Z3_simplify"

external get_relevant_labels : context -> labels
	= "camlidl_z3_Z3_get_relevant_labels"

external del_labels : context -> labels -> unit
	= "camlidl_z3_Z3_del_labels"

external get_num_labels : context -> labels -> int
	= "camlidl_z3_Z3_get_num_labels"

external get_label_symbol : context -> labels -> int -> symbol
	= "camlidl_z3_Z3_get_label_symbol"

external disable_label : context -> labels -> int -> unit
	= "camlidl_z3_Z3_disable_label"

external block_labels : context -> labels -> unit
	= "camlidl_z3_Z3_block_labels"

external get_model_num_constants : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_constants"

external get_model_constant : context -> model -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_model_constant"

external get_value : context -> model -> const_decl_ast -> value
	= "camlidl_z3_Z3_get_value"

external get_value_type : context -> value -> type_ast
	= "camlidl_z3_Z3_get_value_type"

external get_value_kind : context -> value -> value_kind
	= "camlidl_z3_Z3_get_value_kind"

external get_numeral_value_string : context -> value -> string
	= "camlidl_z3_Z3_get_numeral_value_string"

external get_numeral_value_int : context -> value -> bool * int
	= "camlidl_z3_Z3_get_numeral_value_int"

external get_numeral_value_uint : context -> value -> bool * int
	= "camlidl_z3_Z3_get_numeral_value_uint"

external get_bool_value_bool : context -> value -> bool
	= "camlidl_z3_Z3_get_bool_value_bool"

external get_tuple_value_mk_decl : context -> value -> const_decl_ast
	= "camlidl_z3_Z3_get_tuple_value_mk_decl"

external get_tuple_value_num_fields : context -> value -> int
	= "camlidl_z3_Z3_get_tuple_value_num_fields"

external get_tuple_value_field : context -> value -> int -> value
	= "camlidl_z3_Z3_get_tuple_value_field"

external get_array_value_size : context -> value -> int
	= "camlidl_z3_Z3_get_array_value_size"

external get_array_value_else : context -> value -> value
	= "camlidl_z3_Z3_get_array_value_else"

external get_array_value_entry_index : context -> value -> int -> value
	= "camlidl_z3_Z3_get_array_value_entry_index"

external get_array_value_entry_value : context -> value -> int -> value
	= "camlidl_z3_Z3_get_array_value_entry_value"

external get_model_num_funcs : context -> model -> int
	= "camlidl_z3_Z3_get_model_num_funcs"

external is_model_func_internal : context -> model -> int -> bool
	= "camlidl_z3_Z3_is_model_func_internal"

external get_model_func_decl : context -> model -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_model_func_decl"

external get_model_func_else : context -> model -> int -> value
	= "camlidl_z3_Z3_get_model_func_else"

external get_model_func_num_entries : context -> model -> int -> int
	= "camlidl_z3_Z3_get_model_func_num_entries"

external get_model_func_entry_num_args : context -> model -> int -> int -> int
	= "camlidl_z3_Z3_get_model_func_entry_num_args"

external get_model_func_entry_arg : context -> model -> int -> int -> int -> value
	= "camlidl_z3_Z3_get_model_func_entry_arg"

external get_model_func_entry_value : context -> model -> int -> int -> value
	= "camlidl_z3_Z3_get_model_func_entry_value"

external eval : context -> model -> ast -> bool * value
	= "camlidl_z3_Z3_eval"

external set_soft_timeout : context -> int -> unit
	= "camlidl_z3_Z3_set_soft_timeout"

external reset_soft_timeout : context -> unit
	= "camlidl_z3_Z3_reset_soft_timeout"

external open_log : context -> string -> bool
	= "camlidl_z3_Z3_open_log"

external close_log : context -> unit
	= "camlidl_z3_Z3_close_log"

external ast_to_string : context -> ast -> string
	= "camlidl_z3_Z3_ast_to_string"

external model_to_string : context -> model -> string
	= "camlidl_z3_Z3_model_to_string"

external value_to_string : context -> value -> string
	= "camlidl_z3_Z3_value_to_string"

external context_to_string : context -> string
	= "camlidl_z3_Z3_context_to_string"

external parse_smtlib_string : context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> unit
	= "camlidl_z3_Z3_parse_smtlib_string_bytecode" "camlidl_z3_Z3_parse_smtlib_string"

external parse_smtlib_file : context -> string -> symbol array -> type_ast array -> symbol array -> const_decl_ast array -> unit
	= "camlidl_z3_Z3_parse_smtlib_file_bytecode" "camlidl_z3_Z3_parse_smtlib_file"

external get_smtlib_num_formulas : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_formulas"

external get_smtlib_formula : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_formula"

external get_smtlib_num_assumptions : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_assumptions"

external get_smtlib_assumption : context -> int -> ast
	= "camlidl_z3_Z3_get_smtlib_assumption"

external get_smtlib_num_decls : context -> int
	= "camlidl_z3_Z3_get_smtlib_num_decls"

external get_smtlib_decl : context -> int -> const_decl_ast
	= "camlidl_z3_Z3_get_smtlib_decl"

external get_version : unit -> int * int * int * int
	= "camlidl_z3_Z3_get_version"

external type_check : context -> ast -> bool
	= "camlidl_z3_Z3_type_check"

external get_allocation_size : unit -> int
	= "camlidl_z3_Z3_get_allocation_size"




(* Internal auxillary functions: *)

(* Transform a pair of arrays into an array of pairs *)
let array_combine a b =
  if Array.length a <> Array.length b then raise (Invalid_argument "array_combine");
  Array.init (Array.length a) (fun i->(a.(i),b.(i)));;

(* [a |> b] is the pipeline operator for [b(a)] *)
let ( |> ) x f = f x;;


(* Extensions, except for refinement: *)
let mk_context_x configs = 
  let config = mk_config() in
  let f(param_id,param_value) = set_param_value config param_id param_value in
  Array.iter f configs;
  let context = mk_context config in
  del_config config;
  context;;

let get_const_ast_args c a =
  Array.init (get_const_ast_num_args c a) (get_const_ast_arg c a);;

let get_domains c d =
  Array.init (get_domain_size c d) (get_domain c d);;

let get_array_type c t = (get_array_type_domain c t, get_array_type_range c t);;

let get_tuple_type c ty = 
  (get_tuple_type_mk_decl c ty,
   Array.init (get_tuple_type_num_fields c ty) (get_tuple_type_field_decl c ty));;

let get_model_constants c m =
  Array.init (get_model_num_constants c m) (get_model_constant c m);;

let get_tuple_value c v =
  Array.init (get_tuple_value_num_fields c v) (get_tuple_value_field c v);;

let get_array_value c v =
  (array_combine
     (Array.init (get_array_value_size c v) (get_array_value_entry_index c v))
     (Array.init (get_array_value_size c v) (get_array_value_entry_value c v)),
   get_array_value_else c v);;

let get_model_func_entry c m i j =
  (Array.init
     (get_model_func_entry_num_args c m i j)
     (get_model_func_entry_arg c m i j),
   get_model_func_entry_value c m i j);;

let get_model_func_entries c m i =
  Array.init (get_model_func_num_entries c m i) (get_model_func_entry c m i);;

let get_model_funcs c m =
  Array.init (get_model_num_funcs c m)
    (fun i->(is_model_func_internal c m i,
             get_model_func_decl c m i |> get_decl_name c,
             get_model_func_entries c m i,
             get_model_func_else c m i));;
 
let get_smtlib_formulas c = 
  Array.init (get_smtlib_num_formulas c) (get_smtlib_formula c);;

let get_smtlib_assumptions c = 
  Array.init (get_smtlib_num_assumptions c) (get_smtlib_assumption c);;

let get_smtlib_decls c =
  Array.init (get_smtlib_num_decls c) (get_smtlib_decl c);;

let get_smtlib_parse_results c =
  (get_smtlib_formulas c, get_smtlib_assumptions c, get_smtlib_decls c);;

let parse_smtlib_string_formula c a1 a2 a3 a4 a5 = 
  (parse_smtlib_string c a1 a2 a3 a4 a5;
   match get_smtlib_formulas c with [|f|] -> f | _ -> failwith "Z3: parse_smtlib_string_formula");;

let parse_smtlib_file_formula c a1 a2 a3 a4 a5 = 
  (parse_smtlib_file c a1 a2 a3 a4 a5;
   match get_smtlib_formulas c with [|f|] -> f | _ -> failwith "Z3: parse_smtlib_file_formula");;

let parse_smtlib_string_x c a1 a2 a3 a4 a5 = 
  (parse_smtlib_string c a1 a2 a3 a4 a5; get_smtlib_parse_results c);;

let parse_smtlib_file_x c a1 a2 a3 a4 a5 = 
  (parse_smtlib_file c a1 a2 a3 a4 a5; get_smtlib_parse_results c);;

(* Refinement: *)

type symbol_refined =
  | Symbol_int of int
  | Symbol_string of string
  | Symbol_unknown;;

let symbol_refine c s =
  match get_symbol_kind c s with
  | INT_SYMBOL -> Symbol_int (get_symbol_int c s)
  | STRING_SYMBOL -> Symbol_string (get_symbol_string c s);;

type type_refined =
  | Type_uninterpreted of symbol
  | Type_bool
  | Type_int
  | Type_real
  | Type_bv of int
  | Type_array of (type_ast * type_ast)
  | Type_tuple of (const_decl_ast * const_decl_ast array)
  | Type_unknown of symbol;;

let type_refine c ty =
  match get_type_kind c ty with
  | UNINTERPRETED_TYPE -> Type_uninterpreted (get_type_name c ty)
  | BOOL_TYPE -> Type_bool
  | INT_TYPE -> Type_int
  | REAL_TYPE -> Type_real
  | BV_TYPE -> Type_bv (get_bv_type_size c ty)
  | ARRAY_TYPE -> Type_array (get_array_type_domain c ty, get_array_type_range c ty)
  | TUPLE_TYPE -> Type_tuple (get_tuple_type c ty)
  | UNKNOWN_TYPE -> Type_unknown (get_type_name c ty);;


let get_pattern_terms c p = 
  Array.init (get_pattern_num_terms c p) (get_pattern_ast c p)

type binder_type = | Forall | Exists 

type numeral_refined = 
  | Numeral_small  of int64 * int64
  | Numeral_large  of string

type term_refined = 
  | Term_app        of decl_kind * const_decl_ast * ast array
  | Term_quantifier of binder_type * int * ast array array * (symbol *type_ast) array * ast
  | Term_numeral    of numeral_refined * type_ast
  | Term_var        of int * type_ast

let term_refine c t = 
  match get_ast_kind c t with
  | NUMERAL_AST -> 
      let (is_small, n, d) = get_numeral_ast_value_small c t in
      if is_small then 
	Term_numeral(Numeral_small(n,d), get_type c t)
      else
	Term_numeral(Numeral_large(get_numeral_ast_value c t), get_type c t)
  | CONST_AST   -> 
      let t' = to_const_ast c t in
      let f =  get_const_ast_decl c t' in
      let num_args = get_const_ast_num_args c t' in
      let args = Array.init num_args (get_const_ast_arg c t') in
      let k = get_decl_kind c f in
      Term_app (k, f, args)
  | QUANTIFIER_AST -> 
      let bt = if is_quantifier_forall c t then Forall else Exists in
      let w = get_quantifier_weight c t                            in
      let np = get_quantifier_num_patterns c t                     in
      let pats = Array.init np (get_quantifier_pattern_ast c t)    in
      let pats = Array.map (get_pattern_terms c) pats              in
      let nb = get_quantifier_num_bound c t                        in
      let bound = Array.init nb 
	  (fun i -> (get_quantifier_bound_name c t i, get_quantifier_bound_type_ast c t i)) in
      let body = get_quantifier_body c t in
      Term_quantifier(bt, w, pats, bound, body)
  | VAR_AST -> 
      Term_var(get_index_value c t, get_type c t)
  | _ -> assert false

type value_refined =
  | Value_bool of bool
  | Value_numeral of string * type_ast
  | Value_array of ((value * value) array * value)
  | Value_tuple of value array
  | Value_unknown;;

let value_refine c v =
  match get_value_kind c v with
  | BOOL_VALUE -> Value_bool (get_bool_value_bool c v)
  | NUMERAL_VALUE -> Value_numeral (get_numeral_value_string c v, get_value_type c v)
  | TUPLE_VALUE -> Value_tuple (get_tuple_value c v)
  | ARRAY_VALUE -> Value_array (get_array_value c v)
  | UNKNOWN_VALUE -> Value_unknown;;

