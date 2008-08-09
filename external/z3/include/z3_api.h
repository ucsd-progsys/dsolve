DEFINE_TYPE(Z3_config);
DEFINE_TYPE(Z3_context);

DEFINE_TYPE(Z3_ast);
DEFINE_TYPE(Z3_type_ast);
DEFINE_TYPE(Z3_const_decl_ast);
DEFINE_TYPE(Z3_const_ast);
DEFINE_TYPE(Z3_numeral_ast);
DEFINE_TYPE(Z3_pattern_ast);
DEFINE_TYPE(Z3_symbol);
DEFINE_TYPE(Z3_value);
DEFINE_TYPE(Z3_parameter);
DEFINE_TYPE(Z3_model);
DEFINE_TYPE(Z3_labels);

/**
   \defgroup capi C API

*/
/*@{*/

/**
   \conly @name Types
   
   \conly Most of the types in the C API are opaque pointers.

   \conly - \c Z3_config: a configuration object used to initialize logical contexts.
   \conly - \c Z3_context: logical context. This is the main Z3 data-structure.
   \conly - \c Z3_symbol: a Lisp-link symbol. It is used to name types, constants, and functions.  A symbol can be created using
   \conly string or integers. 
   \conly - \c Z3_ast: abstract syntax tree node. That is, the data-structure used in Z3 to represent terms, formulas and types.
   \conly - \c Z3_type_ast: a kind of AST used to represent types.
   \conly - \c Z3_const_ast: a kind of AST used to represent constant and function declarations.
   \conly - \c Z3_numeral_ast: a kind of AST used to represent numerals.
   \conly - \c Z3_pattern_ast: a kind of AST used to represent pattern and multi-patterns used to guide quantifier instantiation.
   \conly - \c Z3_model: a model for the constraints asserted into the logical context.
   \conly - \c Z3_value: a value assigned to a constant in a model (#Z3_model).
*/

#ifndef CAMLIDL
/**
   \conly \brief Z3 Boolean type. It is just an alias for \c int.
*/
typedef int Z3_bool;
#else
/**
   \conly \brief Z3 Boolean type. It is just an alias for \c Boolean.
*/
#define Z3_bool boolean
#endif // CAMLIDL

#ifndef CAMLIDL
/**
   \conly \brief Z3 string type. It is just an alias for <tt>const char *</tt>.
*/
typedef const char * Z3_string;
#else
/**
   \conly \brief Z3 string type. It is just an alias for <tt>[string] const char *</tt>.
*/
#define Z3_string [string] const char *
#endif // CAMLIDL
    
#ifndef CAMLIDL
/**
   \conly \brief True value. It is just an alias for \c 1.
*/
#define Z3_TRUE  1

/**
   \conly \brief False value. It is just an alias for \c 0.
*/
#define Z3_FALSE 0

#endif // CAMLIDL

/**
   \conly \brief Lifted Boolean type: \c false, \c undefined, \c true.
*/
typedef enum 
{
    Z3_L_FALSE = -1,
    Z3_L_UNDEF,
    Z3_L_TRUE
} Z3_lbool;

/**
   \conly \brief In Z3, a symbol can be represented using integers and strings (See #Z3_get_symbol_kind).

   \conly \sa Z3_mk_int_symbol
   \conly \sa Z3_mk_string_symbol
*/
typedef enum 
{
    Z3_INT_SYMBOL,
    Z3_STRING_SYMBOL 
} Z3_symbol_kind;

/**
   \conly \brief The different kinds of Z3 types (See #Z3_get_type_kind).
*/
typedef enum 
{
    Z3_UNINTERPRETED_TYPE,
    Z3_BOOL_TYPE,
    Z3_INT_TYPE,
    Z3_REAL_TYPE,
    Z3_BV_TYPE,
    Z3_ARRAY_TYPE,
    Z3_TUPLE_TYPE,
    Z3_UNKNOWN_TYPE = 1000
} Z3_type_kind;

/**
   \conly \brief The different kinds of Z3 AST (abstract syntax trees). That is, terms, formulas and types.

   \conly - Z3_NUMERAL_AST:        numerals 
   \conly - Z3_CONST_DECL_AST:     constant and function declarations 
   \conly - Z3_CONST_AST:          constant and applications 
   \conly - Z3_TYPE_AST:           types 
   \conly - Z3_VAR_AST:            bound variables 
   \conly - Z3_PATTERN_AST:        patterns and multi-patterns 
   \conly - Z3_QUANTIFIER_AST:     quantifiers 
   \conly - Z3_UNKNOWN_AST:        internal 
*/
typedef enum 
{
    Z3_NUMERAL_AST,       
    Z3_CONST_DECL_AST,    
    Z3_CONST_AST,         
    Z3_TYPE_AST,          
    Z3_VAR_AST,          
    Z3_PATTERN_AST,       
    Z3_QUANTIFIER_AST,    
    Z3_UNKNOWN_AST = 1000 
} Z3_ast_kind;

/**
   \conly \brief The different kinds of interpreted function kinds.

*/
typedef enum {
    Z3_OP_TRUE = 0x100,
    Z3_OP_FALSE,
    Z3_OP_EQ,
    Z3_OP_DISTINCT,
    Z3_OP_ITE,
    Z3_OP_AND,
    Z3_OP_OR,
    Z3_OP_IFF,
    Z3_OP_XOR,
    Z3_OP_NOT,
    Z3_OP_IMPLIES,
    Z3_OP_LE = 0x200,
    Z3_OP_GE,
    Z3_OP_LT,
    Z3_OP_GT,
    Z3_OP_ADD,
    Z3_OP_SUB,
    Z3_OP_UMINUS,
    Z3_OP_MUL,
    Z3_OP_DIV,
    Z3_OP_IDIV,
    Z3_OP_REM,
    Z3_OP_MOD,
    Z3_OP_STORE = 0x300,
    Z3_OP_SELECT,
    Z3_OP_CONST_ARRAY,
    Z3_OP_ARRAY_DEFAULT,
    Z3_OP_STORE_ITE,
    Z3_OP_SET_UNION,
    Z3_OP_SET_INTERSECT,
    Z3_OP_SET_DIFFERENCE,
    Z3_OP_SET_COMPLEMENT,
    Z3_OP_SET_SUBSET,

    Z3_OP_BIT1 = 0x400,
    Z3_OP_BIT0,
    Z3_OP_BNEG,
    Z3_OP_BADD,
    Z3_OP_BSUB,
    Z3_OP_BMUL,
    
    Z3_OP_BSDIV,
    Z3_OP_BUDIV,
    Z3_OP_BSREM,
    Z3_OP_BUREM,
    Z3_OP_BSMOD,

    // special functions to record the division by 0 cases
    // these are internal functions 
    Z3_OP_BSDIV0, 
    Z3_OP_BUDIV0,
    Z3_OP_BSREM0,
    Z3_OP_BUREM0,
    Z3_OP_BSMOD0,
    
    Z3_OP_ULEQ,
    Z3_OP_SLEQ,
    Z3_OP_UGEQ,
    Z3_OP_SGEQ,
    Z3_OP_ULT,
    Z3_OP_SLT,
    Z3_OP_UGT,
    Z3_OP_SGT,

    Z3_OP_BAND,
    Z3_OP_BOR,
    Z3_OP_BNOT,
    Z3_OP_BXOR,
    Z3_OP_BNAND,
    Z3_OP_BNOR,
    Z3_OP_BXNOR,

    Z3_OP_CONCAT,
    Z3_OP_SIGN_EXT,
    Z3_OP_ZERO_EXT,
    Z3_OP_EXTRACT,
    Z3_OP_REPEAT,

    Z3_OP_BREDOR,
    Z3_OP_BREDAND,
    Z3_OP_BCOMP,

    Z3_OP_BSHL,
    Z3_OP_BLSHR,
    Z3_OP_BASHR,
    Z3_OP_ROTATE_LEFT,
    Z3_OP_ROTATE_RIGHT,

    Z3_OP_INT2BV,
    Z3_OP_BV2INT,

    Z3_OP_UNINTERPRETED
} Z3_decl_kind;

/**
   \conly \brief The different kinds of Z3 values (See #Z3_get_value_kind).
*/
typedef enum 
{
    Z3_BOOL_VALUE,
    Z3_NUMERAL_VALUE, 
    Z3_ARRAY_VALUE,
    Z3_TUPLE_VALUE,
    Z3_UNKNOWN_VALUE = 1000
} Z3_value_kind;

#ifndef CAMLIDL

/**
   \conly \brief Z3 error codes (See #Z3_get_error_code). 
   
   \conly - Z3_OK,            
   \conly - Z3_TYPE_ERROR:    User tried to build an invalid (type incorrect) AST.
   \conly - Z3_IOB:           Index out of bounds 
   \conly - Z3_INVALID_ARG:   Invalid argument was provided
   \conly - Z3_PARSER_ERROR:  An error occurred when parsing a string or file.
   \conly - Z3_NO_PARSER:     parser output is not available, that is, user didn't invoke Z3_parse_smtlib_string or Z3_parse_smtlib_file.
   \conly - Z3_INVALID_PATTERN: invalid pattern was used to build a quantifier.
*/
typedef enum
{
    Z3_OK,            
    Z3_TYPE_ERROR,    
    Z3_IOB,           
    Z3_INVALID_ARG,   
    Z3_PARSER_ERROR,  
    Z3_NO_PARSER,
    Z3_INVALID_PATTERN
} Z3_error_code;

/**
   \conly \brief Z3 custom error handler (See #Z3_set_error_handler). 
*/
typedef void Z3_error_handler(Z3_error_code e);

#endif // CAMLIDL

/*@}*/

#ifndef CAMLIDL
#ifdef __cplusplus
extern "C" {
#endif // __cplusplus
#else
[pointer_default(ref)] interface Z3 {
#endif // CAMLIDL
    
    /** 
        @name Create configuration
    */
    /*@{*/

    /**
       \brief Create a configuration.

       Configurations are created in order to assign parameters prior to creating 
       contexts for Z3 interaction. For example, if the users whishes to use model
       generation, then call:

       \ccode{Z3_set_param_value(cfg\, "MODEL"\, "true")}

       \mlonly \remark Consider using {!Z3.mk_context_x} instead of using
       explicit configuration objects. The function {!Z3.mk_context_x}
       receives an array of string pairs. This array represents the
       configuration options. \endmlonly

       \sa Z3_set_param_value
       \sa Z3_del_config
    */
    Z3_config Z3_API Z3_mk_config();

    /**
       \brief Delete the given configuration object.

       \sa Z3_mk_config
    */
    void Z3_API Z3_del_config(__in Z3_config c);
    
    /**
       \brief Set a configuration parameter.

       The list of all configuration parameters can be obtained using the Z3 executable:

       \verbatim
       z3.exe -ini?
       \endverbatim

       \sa Z3_mk_config
    */
    void Z3_API Z3_set_param_value(__in Z3_config c, __in_z Z3_string param_id, __in_z Z3_string param_value);
    /*@}*/

    /**
       @name Create context
    */
    /*@{*/

    /**
       \brief Create a logical context using the given configuration. 
    
       After a context is created, the configuration cannot be changed.
       All main interaction with Z3 happens in the context of a \c Z3_context.

       \mlonly \remark Consider using {!Z3.mk_context_x} instead of using
       explicit configuration objects. The function {!Z3.mk_context_x}
       receives an array of string pairs. This array represents the
       configuration options. \endmlonly

       \sa Z3_del_context
    */
    Z3_context Z3_API Z3_mk_context(__in Z3_config c);

    /**
       \brief Delete the given logical context.

       \sa Z3_mk_config
    */
    void Z3_API Z3_del_context(__in Z3_context c);
    
    /**
       \brief Enable trace messages to a file

       When trace messages are enabled, Z3 will record the operations performed on a context in the given file file.
       Return \c Z3_TRUE if the file was opened successfully, and \c Z3_FALSE otherwise.

       \sa Z3_trace_off
    */
    Z3_bool Z3_API Z3_trace_to_file(__in Z3_context c, __in_z Z3_string trace_file);

    /**
       \brief Enable trace messages to a standard error.

       \sa Z3_trace_off
    */
    void Z3_API Z3_trace_to_stderr(__in Z3_context c);

    /**
       \brief Enable trace messages to a standard output.

       \sa Z3_trace_off
    */
    void Z3_API Z3_trace_to_stdout(__in Z3_context c);

    /**
       \brief Disable trace messages.

       \sa Z3_trace_to_file
       \sa Z3_trace_to_stdout
       \sa Z3_trace_to_stderr
    */
    void Z3_API Z3_trace_off(__in Z3_context c);
    /*@}*/

    /**
       @name Theories
    */
    /*@{*/

    /**
       \brief Enable arithmetic theory in the given logical context.
    */
    void Z3_API Z3_enable_arithmetic(__in Z3_context c);

    /**
       \brief Enable bit-vector theory in the given logical context.
    */
    void Z3_API Z3_enable_bv(__in Z3_context c);

    /**
       \brief Enable array theory in the given logical context.
    */
    void Z3_API Z3_enable_arrays(__in Z3_context c);

    /**
       \brief Enable tuple theory in the given logical context.
    */
    void Z3_API Z3_enable_tuples(__in Z3_context c);
    /*@}*/

    /**
       @name Symbols
    */
    /*@{*/
    /**
       \brief Create a Z3 symbol using an integer.

       Symbols are used to name several term and type constructors.

       \sa Z3_mk_string_symbol
    */
    Z3_symbol Z3_API Z3_mk_int_symbol(__in Z3_context c, __in int i);

    /**
       \brief Create a Z3 symbol using a C string.

       Symbols are used to name several term and type constructors.

       \sa Z3_mk_int_symbol
    */
    Z3_symbol Z3_API Z3_mk_string_symbol(__in Z3_context c, __in_z Z3_string s);
    /*@}*/
    
    
    /**
       @name Types
    */
    /*@{*/
    
    /**
       \brief Create a free (uninterpreted) type using the given name (symbol).
       
       Two free types are considered the same iff the have the same name.
    */
    Z3_type_ast Z3_API Z3_mk_uninterpreted_type(__in Z3_context c, __in Z3_symbol s);
    

    /**
       \brief Create the Boolean type. 

       This type is used to create propositional variables and predicates.
    */
    Z3_type_ast Z3_API Z3_mk_bool_type(__in Z3_context c);
    
    /**
       \brief Create an integer type.

       This type is not the int type found in programming languages.
       A machine integer can be represented using bit-vectors. The function
       #Z3_mk_bv_type creates a bit-vector type.

       \sa Z3_mk_bv_type
    */
    Z3_type_ast Z3_API Z3_mk_int_type(__in Z3_context c);
    
    /**
       \brief Create a real type. 

       This type is not a floating point number.
       Z3 does not have support for floating point numbers yet.
    */
    Z3_type_ast Z3_API Z3_mk_real_type(__in Z3_context c);

    /**
       \brief Create a bit-vector type of the given size.
    
       This type can also be seen as a machine integer.

       \remark The size of the bitvector type must be greater than zero.
    */
    Z3_type_ast Z3_API Z3_mk_bv_type(__in Z3_context c, __in unsigned sz);

    /**
       \brief Create an array type. 
       
       We usually represent the array type as: <tt>[domain -> range]</tt>.
       Arrays are usually used to model the heap/memory in software verification.

       \sa Z3_mk_select
       \sa Z3_mk_store
    */
    Z3_type_ast Z3_API Z3_mk_array_type(__in Z3_context c, __in Z3_type_ast domain, __in Z3_type_ast range);

    /**
       \brief Create a tuple type.
       
       \mlonly [mk_tuple_type c name field_names field_types] creates a tuple with a constructor named [name],
       a [n] fields, where [n] is the size of the arrays [field_names] and [field_types].
       \endmlonly

       \conly A tuple with \c n fields has a constructor and \c n projections.
       \conly This function will also declare the constructor and projection functions.

       \param c logical context
       \param mk_tuple_name name of the constructor function associated with the tuple type.
       \param num_fields number of fields in the tuple type.
       \param field_names name of the projection functions.
       \param field_types type of the tuple fields.
       \param mk_tuple_decl output parameter that will contain the constructor declaration.
       \param proj_decl output parameter that will contain the projection function declarations. This field must be a buffer of size \c num_fields allocated by the user.
    */
    Z3_type_ast Z3_API Z3_mk_tuple_type(__in Z3_context c, 
                                        __in Z3_symbol mk_tuple_name, 
                                        __in unsigned num_fields, 
                                        __in_ecount(num_fields) Z3_symbol   const field_names[],
                                        __in_ecount(num_fields) Z3_type_ast const field_types[],
                                        __out Z3_const_decl_ast * mk_tuple_decl,
                                        __out_ecount(num_fields)  Z3_const_decl_ast proj_decl[]);
    /*@}*/

    /**
       @name Constants and Applications
     */
    /*@{*/

    /**
       \brief Declare a constant or function.

       \mlonly [mk_func_decl c n d r] creates a function with name [n], domain [d], and range [r].
       The arity of the function is the size of the array [d]. \endmlonly

       \param c logical context.
       \param s name of the constant or function.
       \param domain_size number of arguments. It is 0 when declaring a constant.
       \param domain array containing the type of each argument. The array must contain domain_size elements. It is 0 whe declaring a constant.
       \param range type of the constant or the return type of the function.

       After declaring a constant or function, the function
       #Z3_mk_app can be used to create a constant or function
       application.

       \sa Z3_mk_app
    */
    Z3_const_decl_ast Z3_API Z3_mk_func_decl(__in Z3_context c, __in Z3_symbol s,
                                             __in unsigned domain_size, __in_ecount(domain_size) Z3_type_ast const domain[],
                                             __in Z3_type_ast range);
    
    /**
       \brief Create a constant or function application.

       \sa Z3_mk_func_decl
    */
    Z3_ast Z3_API Z3_mk_app(
        __in Z3_context c, __in Z3_const_decl_ast d,
        __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);

    /**
       \brief Declare and create a constant.
       
       \conly This function is a shorthand for:
       \conly \code
       \conly Z3_const_decl_ast d = Z3_mk_func_decl(c, s, 0, 0, ty);
       \conly Z3_ast n            = Z3_mk_app(c, d, 0, 0);
       \conly \endcode
       
       \mlonly [mk_const c s t] is a shorthand for [mk_app c (mk_func_decl c s [||] t) [||]] \endmlonly

       \sa Z3_mk_func_decl
       \sa Z3_mk_app
    */
    Z3_ast Z3_API Z3_mk_const(__in Z3_context c, __in Z3_symbol s, __in Z3_type_ast ty);

    /**
       \brief Create a labeled formula.

       \param c logical context.
       \param s name of the label.
       \param is_pos label polarity.
       \param f formula being labeled.

       A label behaves as an identity function, so the truth value of the 
       labeled formula is unchanged. Labels are used for identifying 
       useful sub-formulas when generating counter-examples.
    */
    Z3_ast Z3_API Z3_mk_label(__in Z3_context c, __in Z3_symbol s, Z3_bool is_pos, Z3_ast f);

    /**
       \brief Declare a fresh constant or function.

       Z3 will generate an unique name for this function declaration.
       \conly If prefix is different from \c NULL, then the name generate by Z3 will start with \c prefix.
       
       \conly \remark If \c prefix is NULL, then it is assumed to be the empty string.

       \sa Z3_mk_func_decl
    */
    Z3_const_decl_ast Z3_API Z3_mk_fresh_func_decl(__in Z3_context c, __in_z Z3_string prefix,
                                                   __in unsigned domain_size, __in_ecount(domain_size) Z3_type_ast const domain[],
                                                   __in Z3_type_ast range);
    
    /**
       \brief Declare and create a fresh constant.
       
       \conly This function is a shorthand for:
       \conly \code
       \conly Z3_const_decl_ast d = Z3_mk_fresh_func_decl(c, prefix, 0, 0, ty);
       \conly Z3_ast n            = Z3_mk_app(c, d, 0, 0);
       \conly \endcode

       \mlonly [mk_fresh_const c p t] is a shorthand for [mk_app c (mk_fresh_func_decl c p [||] t) [||]]. \endmlonly

       \conly \remark If \c prefix is NULL, then it is assumed to be the empty string.
       
       \sa Z3_mk_func_decl
       \sa Z3_mk_app
    */
    Z3_ast Z3_API Z3_mk_fresh_const(__in Z3_context c, __in_z Z3_string prefix, __in Z3_type_ast ty);

    
    /** 
        \brief Create an AST node representing \c true.
    */
    Z3_ast Z3_API Z3_mk_true(__in Z3_context c);

    /** 
        \brief Create an AST node representing \c false.
    */
    Z3_ast Z3_API Z3_mk_false(__in Z3_context c);
    
    /** 
        \brief \mlh mk_eq c l r \endmlh
        Create an AST node representing <tt>l = r</tt>.
        
        The nodes \c l and \c r must have the same type. 
    */
    Z3_ast Z3_API Z3_mk_eq(__in Z3_context c, __in Z3_ast l, __in Z3_ast r);
    
    /**
       \conly \brief Create an AST node representing <tt>distinct(args[0], ..., args[num_args-1])</tt>.
       \mlonly \brief \[ [mk_distinct c [| t_1; ...; t_n |]] \] Create an AST
       node represeting a distinct construct. It is used for declaring
       the arguments t_i pairwise distinct. \endmlonly

       \conly The \c distinct construct is used for declaring the arguments pairwise distinct. 
       \conly That is, <tt>Forall 0 <= i < j < num_args. not args[i] = args[j]</tt>.
       
       All arguments must have the same type.

       \remark The number of arguments of a distinct construct must be greater than one.
    */
    Z3_ast Z3_API Z3_mk_distinct(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);

    /** 
        \brief \mlh mk_not c a \endmlh 
        Create an AST node representing <tt>not(a)</tt>.
        
        The node \c a must have Boolean type.
    */
    Z3_ast Z3_API Z3_mk_not(__in Z3_context c, __in Z3_ast a);
    
    /**
       \brief \mlh mk_ite c t1 t2 t2 \endmlh 
       Create an AST node representing an if-then-else: <tt>ite(t1, t2,
       t3)</tt>.

       The node \c t1 must have Boolean type, \c t2 and \c t3 must have the same type.
       The type of the new node is equal to the type of \c t2 and \c t3.
    */
    Z3_ast Z3_API Z3_mk_ite(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2, __in Z3_ast t3);

    /**
       \brief \mlh mk_iff c t1 t2 \endmlh
       Create an AST node representing <tt>t1 iff t2</tt>.

       The nodes \c t1 and \c t2 must have Boolean type.
    */
    Z3_ast Z3_API Z3_mk_iff(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_implies c t1 t2 \endmlh
       Create an AST node representing <tt>t1 implies t2</tt>.

       The nodes \c t1 and \c t2 must have Boolean type.
    */
    Z3_ast Z3_API Z3_mk_implies(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \brief \mlh mk_xor c t1 t2 \endmlh
       Create an AST node representing <tt>t1 xor t2</tt>.

       The nodes \c t1 and \c t2 must have Boolean type.
    */
    Z3_ast Z3_API Z3_mk_xor(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \conly \brief Create an AST node representing <tt>args[0] and ... and args[num_args-1]</tt>.
       \mlonly \brief \[ [mk_and c [| t_1; ...; t_n |]] \] Create the conjunction: {e t_1 and ... and t_n}. \endmlonly

       \conly The array \c args must have \c num_args elements. 
       All arguments must have Boolean type.
       
       \remark The number of arguments must be greater than zero.
    */
    Z3_ast Z3_API Z3_mk_and(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);
    
    /**
       \conly \brief Create an AST node representing <tt>args[0] or ... or args[num_args-1]</tt>.
       \mlonly \brief \[ [mk_or c [| t_1; ...; t_n |]] \] Create the disjunction: {e t_1 or ... or t_n}. \endmlonly

       \conly The array \c args must have \c num_args elements. 
       All arguments must have Boolean type.

       \remark The number of arguments must be greater than zero.
    */
    Z3_ast Z3_API Z3_mk_or(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);
    
    /**
       \conly \brief Create an AST node representing <tt>args[0] + ... + args[num_args-1]</tt>.
       \mlonly \brief \[ [mk_add c [| t_1; ...; t_n |]] \] Create the term: {e t_1 + ... + t_n}. \endmlonly

       \conly The array \c args must have \c num_args elements. 
       All arguments must have int or real type.

       \remark The number of arguments must be greater than zero.
    */
    Z3_ast Z3_API Z3_mk_add(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);
    
    /**
       \conly \brief Create an AST node representing <tt>args[0] * ... * args[num_args-1]</tt>.
       \mlonly \brief \[ [mk_mul c [| t_1; ...; t_n |]] \] Create the term: {e t_1 * ... * t_n}. \endmlonly

       \conly The array \c args must have \c num_args elements. 
       All arguments must have int or real type.
       
       \remark Z3 has limited support for non-linear arithmetic.
       \remark The number of arguments must be greater than zero.
    */
    Z3_ast Z3_API Z3_mk_mul(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);
    
    /**
       \conly \brief Create an AST node representing <tt>args[0] - ... - args[num_args - 1]</tt>.
       \mlonly \brief \[ [mk_sub c [| t_1; ...; t_n |]] \] Create the term: {e t_1 - ... - t_n}. \endmlonly

       \conly The array \c args must have \c num_args elements. 
       All arguments must have int or real type.

       \remark The number of arguments must be greater than zero.
    */
    Z3_ast Z3_API Z3_mk_sub(__in Z3_context c, __in unsigned num_args, __in_ecount(num_args) Z3_ast const args[]);

    /** 
        \brief \mlh mk_lt c t1 t2 \endmlh 
        Create less than.

        The nodes \c t1 and \c t2 must have the same type, and must be int or real.
    */
    Z3_ast Z3_API Z3_mk_lt(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_le c t1 t2 \endmlh
        Create less than or equal to.
        
        The nodes \c t1 and \c t2 must have the same type, and must be int or real.
    */
    Z3_ast Z3_API Z3_mk_le(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_gt c t1 t2 \endmlh
        Create greater than.
        
        The nodes \c t1 and \c t2 must have the same type, and must be int or real.
    */
    Z3_ast Z3_API Z3_mk_gt(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_ge c t1 t2 \endmlh
        Create greater than or equal to.
        
        The nodes \c t1 and \c t2 must have the same type, and must be int or real.
    */
    Z3_ast Z3_API Z3_mk_ge(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvneg c t1 \endmlh
       Bitwise negation.

       The node \c t1 must have a bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvnot(__in Z3_context c, __in Z3_ast t1);

    /**
       \brief \mlh mk_bvand c t1 t2 \endmlh
       Bitwise and.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvand(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvor c t1 t2 \endmlh
       Bitwise or.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvor(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvxor c t1 t2 \endmlh
       Bitwise exclusive-or.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvxor(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvnand c t1 t2 \endmlh
       Bitwise nand. 

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvnand(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvnor c t1 t2 \endmlh
       Bitwise nor. 

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvnor(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvxnor c t1 t2 \endmlh
       Bitwise xnor. 
       
       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvxnor(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvneg c t1 \endmlh
       Standard two's complement unary minus. 

       The node \c t1 must have bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvneg(__in Z3_context c, __in Z3_ast t1);
    
    /** 
        \brief \mlh mk_bvadd c t1 t2 \endmlh
        Standard two's complement addition.
        
        The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvadd(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_bvsub c t1 t2 \endmlh
        Standard two's complement subtraction.
        
        The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvsub(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /** 
        \brief \mlh mk_bvmul c t1 t2 \endmlh
        Standard two's complement multiplication.
        
        The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvmul(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_bvudiv c t1 t2 \endmlh
        Unsigned division. 

        It is defined as the \c floor of <tt>t1/t2</tt> if \c t2 is
        different from zero. If <tt>t2</tt> is zero, then the result
        is undefined.
        
        The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvudiv(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /** 
        \brief \mlh mk_bvsdiv c t1 t2 \endmlh
        Two's complement signed division. 

        It is defined in the following way:

        - The \c floor of <tt>t1/t2</tt> if \c t2 is different from zero, and <tt>t1*t2 >= 0</tt>.

        - The \c ceiling of <tt>t1/t2</tt> if \c t2 is different from zero, and <tt>t1*t2 < 0</tt>.
        
        If <tt>t2</tt> is zero, then the result is undefined.
        
        The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvsdiv(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvurem c t1 t2 \endmlh
       Unsigned remainder.

       It is defined as <tt>t1 - (t1 /u t2) * t2</tt>, where <tt>/u</tt> represents unsigned division.
       
       If <tt>t2</tt> is zero, then the result is undefined.
       
       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvurem(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvsrem c t1 t2 \endmlh
       Two's complement signed remainder (sign follows dividend).

       It is defined as <tt>t1 - (t1 /s t2) * t2</tt>, where <tt>/s</tt> represents signed division.
       The most significant bit (sign) of the result is equal to the most significant bit of \c t1.

       If <tt>t2</tt> is zero, then the result is undefined.
       
       The nodes \c t1 and \c t2 must have the same bit-vector type.

       \sa Z3_mk_bvsmod
    */
    Z3_ast Z3_API Z3_mk_bvsrem(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvsmod c t1 t2 \endmlh
       Two's complement signed remainder (sign follows divisor).
       
       If <tt>t2</tt> is zero, then the result is undefined.
       
       The nodes \c t1 and \c t2 must have the same bit-vector type.

       \sa Z3_mk_bvsrem
    */
    Z3_ast Z3_API Z3_mk_bvsmod(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvult c t1 t2 \endmlh
       Unsigned less than.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvult(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \brief \mlh mk_bvslt c t1 t2 \endmlh
       Two's complement signed less than.
       
       It abbreviates:
       \code
      (or (and (= (extract[|m-1|:|m-1|] s) bit1)
               (= (extract[|m-1|:|m-1|] t) bit0))
          (and (= (extract[|m-1|:|m-1|] s) (extract[|m-1|:|m-1|] t))
               (bvult s t)))
       \endcode

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvslt(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvule c t1 t2 \endmlh
       Unsigned less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvule(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvsle c t1 t2 \endmlh
       Two's complement signed less than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvsle(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvuge c t1 t2 \endmlh
       Unsigned greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvuge(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvsge c t1 t2 \endmlh
       Two's complement signed greater than or equal to.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvsge(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvugt c t1 t2 \endmlh
       Unsigned greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvugt(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvsgt c t1 t2 \endmlh
       Two's complement signed greater than.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvsgt(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_concat c t1 t2 \endmlh
       Concatenate the given bit-vectors.
       
       The nodes \c t1 and \c t2 must have (possibly different) bit-vector types

       The result is a bit-vector of size <tt>n1+n2</tt>, where \c n1 (\c n2) is the size
       of \c t1 (\c t2).
    */
    Z3_ast Z3_API Z3_mk_concat(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \brief \mlh mk_extract c high low t1 \endmlh
       Extract the bits \c high down to \c low from a bitvector of
       size \c m to yield a new bitvector of size \c n, where <tt>n =
       high - low + 1</tt>.

       The node \c t1 must have a bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_extract(__in Z3_context c, __in unsigned high, __in unsigned low, __in Z3_ast t1);

    /**
       \brief \mlh mk_sign_ext c i t1 \endmlh
       Sign-extend of the given bit-vector to the (signed) equivalent bitvector of
       size <tt>m+i</tt>, where \c m is the size of the given
       bit-vector.

       The node \c t1 must have a bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_sign_ext(__in Z3_context c, __in unsigned i, __in Z3_ast t1);

    /**
       \brief \mlh mk_zero_ext c i t1 \endmlh
       Extend the given bit-vector with zeros to the (unsigned) equivalent
       bitvector of size <tt>m+i</tt>, where \c m is the size of the
       given bit-vector.
       
       The node \c t1 must have a bit-vector type. 
    */
    Z3_ast Z3_API Z3_mk_zero_ext(__in Z3_context c, __in unsigned i, __in Z3_ast t1);

    /**
       \brief \mlh mk_bvshl c t1 t2 \endmlh
       Shift left.

       It is equivalent to multiplication by <tt>2^x</tt> where \c x is the value of the
       third argument.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvshl(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvlshr c t1 t2 \endmlh
       Logical shift right.

       It is equivalent to unsigned division by <tt>2^x</tt> where \c x is the
       value of the third argument.

       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvlshr(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);

    /**
       \brief \mlh mk_bvashr c t1 t2 \endmlh
       Arithmetic shift right.
       
       It is like logical shift right except that the most significant
       bits of the result always copy the most significant bit of the
       second argument.
       
       The nodes \c t1 and \c t2 must have the same bit-vector type.
    */
    Z3_ast Z3_API Z3_mk_bvashr(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \brief \mlh mk_rotate_left c i t1 \endmlh
       Rotate bits of \c t1 to the left \c i times.
       
       The node \c t1 must have a bit-vector type. 
    */
    Z3_ast Z3_API Z3_mk_rotate_left(__in Z3_context c, __in unsigned i, __in Z3_ast t1);
    
    /**
       \brief \mlh mk_rotate_right c i t1 \endmlh
       Rotate bits of \c t1 to the right \c i times.
       
       The node \c t1 must have a bit-vector type. 
    */
    Z3_ast Z3_API Z3_mk_rotate_right(__in Z3_context c, __in unsigned i, __in Z3_ast t1);


    /**
       \brief \mlh mk_select c a i \endmlh
       Array read.

       The node \c a must have an array type <tt>[domain -> range]</tt>, and \c i must have the type \c domain.
       The type of the result is \c range.

       \sa Z3_mk_array_type
       \sa Z3_mk_store
    */
    Z3_ast Z3_API Z3_mk_select(__in Z3_context c, __in Z3_ast a, __in Z3_ast i);
    
    /**
       \brief \mlh mk_store c a i v \endmlh
       Array update.
       
       The node \c a must have an array type <tt>[domain -> range]</tt>, \c i must have type \c domain,
       \c v must have type range. The type of the result is <tt>[domain -> range]</tt>.
       
       \sa Z3_mk_array_type
       \sa Z3_mk_store
    */
    Z3_ast Z3_API Z3_mk_store(__in Z3_context c, __in Z3_ast a, __in Z3_ast i, __in Z3_ast v);


    /*@}*/

    /**
       @name Numerals
    */
    /*@{*/

    /**
       \brief Create a numeral of a given type. 

       \param c logical context.
       \param numeral A string representing the numeral value in decimal notation. If the given type is a real, then the numeral can be a rational, that is, a string of the form <tt>[num]* / [num]*</tt>.
       \param ty The type of the numeral. In the current implementation, the given type can be an int, real, or bit-vectors of arbitrary size. 
       
       \sa Z3_mk_int
       \sa Z3_mk_unsigned_int
    */
    Z3_ast Z3_API Z3_mk_numeral(__in Z3_context c, __in_z Z3_string numeral, __in Z3_type_ast ty);
    
    /**
       \brief Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral
    */
    Z3_ast Z3_API Z3_mk_int(__in Z3_context c, __in int v, __in Z3_type_ast ty);
    
    /**
       \brief Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine unsinged integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral
    */
    Z3_ast Z3_API Z3_mk_unsigned_int(__in Z3_context c, __in unsigned v, __in Z3_type_ast ty);

#ifndef CAMLIDL
    /**
       \brief Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine long long integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral
    */
    Z3_ast Z3_API Z3_mk_int64(__in Z3_context c, __in long long v, __in Z3_type_ast ty);
#endif // CAMLIDL

#ifndef CAMLIDL
    /**
       \brief Create a numeral of a given type. 
       
       This function can be use to create numerals that fit in a machine unsigned long long integer.
       It is slightly faster than #Z3_mk_numeral since it is not necessary to parse a string.

       \sa Z3_mk_numeral
    */
    Z3_ast Z3_API Z3_mk_unsigned_int64(__in Z3_context c, __in unsigned long long v, __in Z3_type_ast ty);
#endif // CAMLIDL

    /*@}*/

    /**
       @name Quantifiers
    */
    /*@{*/

    /**
       \brief Create a pattern for quantifier instantiation.

       Z3 uses pattern matching to instantiate quantifiers. If a
       pattern is not provided for a quantifier, then Z3 will
       automatically compute a set of patterns for it. However, for
       optimal performance, the user should provide the patterns.

       Patterns comprise a list of terms. The list should be
       non-empty.  If the list comprises of more than one term, it is
       a called a multi-pattern.
       
       In general, one can pass in a list of (multi-)patterns in the
       quantifier constructor.


       \sa Z3_mk_forall
       \sa Z3_mk_exists
    */
    Z3_pattern_ast Z3_API Z3_mk_pattern(
        __in Z3_context c,
        __in unsigned num_patterns, __in_ecount(num_patterns) Z3_ast const terms[]);

    /**
       \brief Create a bound variable.

       Bound variables are indexed by de-Bruijn indices. It is perhaps easiest to explain
       the meaning of de-Bruijn indices by indicating the compilation process from
       non-de-Bruijn formulas to de-Bruijn format.

       \verbatim 
       abs(forall (x1) phi) = forall (x1) abs1(phi, x1, 0)
       abs(forall (x1, x2) phi) = abs(forall (x1) abs(forall (x2) phi))
       abs1(x, x, n) = b_n
       abs1(y, x, n) = y
       abs1(f(t1,...,tn), x, n) = f(abs1(t1,x,n), ..., abs1(tn,x,n))
       abs1(forall (x1) phi, x, n) = forall (x1) (abs1(phi, x, n+1))
       \endverbatim

       The last line is significant: the index of a bound variable is different depending
       on the scope in which it appears. The deeper x appears, the higher is its
       index.
       
       \param c logical context
       \param index de-Bruijn index
       \param ty type of the bound variable

       \sa Z3_mk_forall
       \sa Z3_mk_exists
    */
    Z3_ast Z3_API Z3_mk_bound(__in Z3_context c, __in unsigned index, __in Z3_type_ast ty);
    
    /**
       \brief Create a forall formula.

       \mlonly [mk_forall c w p t n b] creates a forall formula, where
       [w] is the weight, [p] is an array of patterns, [t] is an array
       with the types of the bound variables, [n] is an array with the
       'names' of the bound variables, and [b] is the body of the
       quantifier. Quantifiers are associated with weights indicating
       the importance of using the quantifier during
       instantiation. \endmlonly
       
       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_decls number of variables to be bound.
       \param decl_names names of the bound variables
       \param body the body of the quantifier.
       
       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_exists
    */
    Z3_ast Z3_API Z3_mk_forall(__in Z3_context c, __in unsigned weight,
                               __in unsigned num_patterns, __in_ecount(num_patterns) Z3_pattern_ast const patterns[],
                               __in unsigned num_decls, __in_ecount(num_decls) Z3_type_ast const types[],
                               __in_ecount(num_decls) Z3_symbol const decl_names[],
                               __in Z3_ast body);

    /**
       \brief Create an exists formula. Similar to #Z3_mk_forall.
       
       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
    */
    Z3_ast Z3_API Z3_mk_exists(__in Z3_context c, __in unsigned weight,
                               __in unsigned num_patterns, __in_ecount(num_patterns) Z3_pattern_ast const patterns[],
                               __in unsigned num_decls, __in_ecount(num_decls) Z3_type_ast const types[],
                               __in_ecount(num_decls) Z3_symbol const decl_names[],
                               __in Z3_ast body);

    /**
       \brief Create a quantifier - universal or existential, with pattern hints.
       
       \param c logical context.
       \param weight quantifiers are associated with weights indicating the importance of using the quantifier during instantiation. By default, pass the weight 0.
       \param num_patterns number of patterns.
       \param patterns array containing the patterns created using #Z3_mk_pattern.
       \param num_no_patterns number of elements in no_patterns.
       \param no_patterns array containing the terms that should not be used in patterns.
       \param num_decls number of variables to be bound.
       \param decl_names names of the bound variables
       \param body the body of the quantifier.
       
       \sa Z3_mk_pattern
       \sa Z3_mk_bound
       \sa Z3_mk_forall
       \sa Z3_mk_exists
    */

    Z3_ast Z3_mk_quantifier(
        Z3_context c, 
        Z3_bool is_forall, 
        unsigned weight, 
        unsigned num_patterns, Z3_pattern_ast const* patterns, 
        unsigned num_no_patterns, Z3_ast const* no_patterns, 
        unsigned num_decls, Z3_type_ast const* types, 
        Z3_symbol const* decl_names, 
        Z3_ast body);

    /*@}*/


    /**
       @name Accessors
    */
    /*@{*/

    /**
       \brief Return \c Z3_INT_SYMBOL if the symbol was constructed
       using #Z3_mk_int_symbol, and \c Z3_STRING_SYMBOL if the symbol
       was constructed using #Z3_mk_string_symbol.
    */
    Z3_symbol_kind Z3_API Z3_get_symbol_kind(__in Z3_context c, __in Z3_symbol s);

    /**
       \brief \mlh get_symbol_int c s \endmlh
       Return the symbol int value. 
       
       \pre Z3_get_symbol_kind(s) == Z3_INT_SYMBOL

       \sa Z3_mk_int_symbol
    */
    int Z3_API Z3_get_symbol_int(__in Z3_context c, __in Z3_symbol s);
    
    /**
       \brief \mlh get_symbol_string c s \endmlh
       Return the symbol name. 

       \pre Z3_get_symbol_string(s) == Z3_STRING_SYMBOL

       \conly \warning The returned buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_get_symbol_string.

       \sa Z3_mk_string_symbol
    */
    Z3_string Z3_API Z3_get_symbol_string(__in Z3_context c, __in Z3_symbol s);

    /**
       \brief Return \c Z3_TRUE if the two given AST nodes are equal.
    */
    Z3_bool Z3_API Z3_is_eq(__in Z3_context c, __in Z3_ast t1, __in Z3_ast t2);
    
    /**
       \brief Return the kind of the given AST.
    */
    Z3_ast_kind Z3_API Z3_get_ast_kind(__in Z3_context c, __in Z3_ast a);
    
    /**
       \brief Return \c Z3_TRUE if the given AST is an expression.

       An expression is a constant, application, numeral, bound variable, or quantifier.
    */
    Z3_bool Z3_API Z3_is_expr(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return the declaration of a constant or function application.
    */
    Z3_const_decl_ast Z3_API Z3_get_const_ast_decl(__in Z3_context c, __in Z3_const_ast a);

    /**
       \brief \mlh get_const_ast_num_args c a \endmlh
       Return the number of argument of an application. If \c t
       is an constant, then the number of arguments is 0.
    */
    unsigned Z3_API Z3_get_const_ast_num_args(__in Z3_context c, __in Z3_const_ast a);

    /**
       \brief \mlh get_const_ast_arg c a i \endmlh
       Return the i-th argument of the given application.
       
       \pre i < Z3_get_num_args(c, a)
    */
    Z3_ast Z3_API Z3_get_const_ast_arg(__in Z3_context c, __in Z3_const_ast a, __in unsigned i);

    /**
       \brief Return the constant declaration name as a symbol. 
    */
    Z3_symbol Z3_API Z3_get_decl_name(__in Z3_context c, __in Z3_const_decl_ast d);

    /**
       \brief Return the type name as a symbol. 
    */
    Z3_symbol Z3_API Z3_get_type_name(__in Z3_context c, __in Z3_type_ast d);

    /**
       \brief Return the type of an AST node.
       
       The AST node must be a constant, application, numeral, bound variable, or quantifier.

       \sa Z3_is_expr
    */
    Z3_type_ast Z3_API Z3_get_type(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return the number of parameters of the given declaration.

       \sa Z3_get_domain_size
    */
    unsigned Z3_API Z3_get_domain_size(__in Z3_context c, __in Z3_const_decl_ast d);

    /**
       \brief \mlh get_domain c d i \endmlh
       Return the type of the i-th parameter of the given function declaration.
       
       \pre i < Z3_get_domain_size(d)

       \sa Z3_get_domain_size
    */
    Z3_type_ast Z3_API Z3_get_domain(__in Z3_context c, __in Z3_const_decl_ast d, __in unsigned i);

    /**
       \brief \mlh get_range c d \endmlh
       Return the range of the given declaration. 

       If \c d is a constant (i.e., has zero arguments), then this
       function returns the type of the constant.
    */
    Z3_type_ast Z3_API Z3_get_range(__in Z3_context c, __in Z3_const_decl_ast d);

    /**
       \brief Return the type kind (e.g., array, tuple, int, bool, etc).

       \sa Z3_type_kind
    */
    Z3_type_kind Z3_API Z3_get_type_kind(__in Z3_context c, __in Z3_type_ast t);

    /**
       \brief \mlh get_bv_type_size c t \endmlh
       Return the size of the given bit-vector type. 

       \pre Z3_get_type_kind(c, t) == Z3_BV_TYPE

       \sa Z3_mk_bv_type
       \sa Z3_get_type_kind
    */
    unsigned Z3_API Z3_get_bv_type_size(__in Z3_context c, __in Z3_type_ast t);

    /**
       \brief \mlh get_array_type_domain c t \endmlh
       Return the domain of the given array type.
       
       \pre Z3_get_type_kind(c, t) == Z3_ARRAY_TYPE

       \sa Z3_mk_array_type
       \sa Z3_get_type_kind
    */
    Z3_type_ast Z3_API Z3_get_array_type_domain(__in Z3_context c, __in Z3_type_ast t);

    /**
       \brief \mlh get_array_type_range c t \endmlh 
       Return the range of the given array type. 

       \pre Z3_get_type_kind(c, t) == Z3_ARRAY_TYPE

       \sa Z3_mk_array_type
       \sa Z3_get_type_kind
    */
    Z3_type_ast Z3_API Z3_get_array_type_range(__in Z3_context c, __in Z3_type_ast t);

    /**
       \brief \mlh get_tuple_type_mk_decl c t \endmlh
       Return the constructor declaration of the given tuple
       type. 

       \pre Z3_get_type_kind(c, t) == Z3_TUPLE_TYPE

       \sa Z3_mk_tuple_type
       \sa Z3_get_type_kind
    */
    Z3_const_decl_ast Z3_API Z3_get_tuple_type_mk_decl(__in Z3_context c, __in Z3_type_ast t);
    
    /**
       \brief \mlh get_tuple_type_num_fields c t \endmlh
       Return the number of fields of the given tuple type. 

       \pre Z3_get_type_kind(c, t) == Z3_TUPLE_TYPE

       \mlonly \remark Consider using the function {!Z3.get_tuple_type}, which 
       returns a tuple: tuple constructor, and an array of the tuple type fields. \endmlonly

       \sa Z3_mk_tuple_type
       \sa Z3_get_type_kind
    */
    unsigned Z3_API Z3_get_tuple_type_num_fields(__in Z3_context c, __in Z3_type_ast t);

    /**
       \brief \mlh get_tuple_type_field_decl c t i \endmlh
       Return the i-th field declaration (i.e., projection function declaration)
       of the given tuple type. 

       \mlonly \remark Consider using the function {!Z3.get_tuple_type}, which 
       returns a tuple: tuple constructor, and an array of the tuple type fields. \endmlonly

       \pre Z3_get_type_kind(t) == Z3_TUPLE_TYPE
       \pre i < Z3_get_tuple_type_num_fields(c, t, i)
       
       \sa Z3_mk_tuple_type
       \sa Z3_get_type_kind
    */
    Z3_const_decl_ast Z3_API Z3_get_tuple_type_field_decl(__in Z3_context c, __in Z3_type_ast t, __in unsigned i);

    /**
       \brief Return declaration kind corresponding to declaration.
    */
    Z3_decl_kind Z3_API Z3_get_decl_kind(__in Z3_context c, __in Z3_const_decl_ast d);

    /**
       \brief Return numeral value, as a string of a numeric constant term

       \pre Z3_get_type(a) == Z3_NUMERAL_AST
    */
    Z3_string Z3_API Z3_get_numeral_ast_value(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return numeral value, as a pair of 64 bit numbers if the representation fits.

       \param c logical context.
       \param a term.
       \param num numerator.
       \param den denominator.
       
       Preturn \c Z3_TRUE if the numeral value fits in 64 bit numerals, \c Z3_FALSE otherwise.

       \pre Z3_get_type(a) == Z3_NUMERAL_AST
    */
    Z3_bool Z3_API Z3_get_numeral_ast_value_small(__in Z3_context c, __in Z3_ast a, __out long long* n, __out long long* d);

    /**
       \brief Return index of de-Brujin bound variable.

       \pre Z3_get_type(a) == Z3_VAR_AST
    */
    unsigned Z3_API Z3_get_index_value(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Determine if quantifier is universal.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    Z3_bool Z3_API Z3_is_quantifier_forall(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Obtain weight of quantifier.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    unsigned Z3_API Z3_get_quantifier_weight(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return number of patterns used in quantifier.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    unsigned Z3_API Z3_get_quantifier_num_patterns(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return i'th pattern.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    Z3_pattern_ast Z3_API Z3_get_quantifier_pattern_ast(__in Z3_context c, __in Z3_ast a, unsigned i);

    /**
       \brief Return symbol of the i'th bound variable.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    Z3_symbol Z3_API Z3_get_quantifier_bound_name(__in Z3_context c, __in Z3_ast a, unsigned i);

    /**
       \brief Return type of the i'th bound variable.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    Z3_type_ast Z3_API Z3_get_quantifier_bound_type_ast(__in Z3_context c, __in Z3_ast a, unsigned i);

    /**
       \brief Return body of quantifier.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    Z3_ast Z3_API Z3_get_quantifier_body(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Return number of bound variables of quantifier.
       
       \pre Z3_get_type(a) == Z3_QUANTIFIER_AST
    */
    unsigned Z3_API Z3_get_quantifier_num_bound(__in Z3_context c, __in Z3_ast a);


    /** 
        \brief Return number of terms in pattern.
    */
    unsigned Z3_API Z3_get_pattern_num_terms(__in Z3_context c, __in Z3_pattern_ast p);
    
    /**
       \brief Return i'th ast in pattern.
    */
    Z3_ast Z3_API Z3_get_pattern_ast(__in Z3_context c, __in Z3_pattern_ast p, __in unsigned idx);

    /*@}*/

    /**
       @name Coercions
    */
    /*@{*/
   
    /**
       \brief Convert a TYPE_AST into an AST. This is just type casting.
    */
    Z3_ast Z3_API Z3_type_ast_to_ast(__in Z3_context c, __in Z3_type_ast a);
    
    /**
       \brief Convert a CONST_AST into an AST. This is just type casting.
    */
    Z3_ast Z3_API Z3_const_ast_to_ast(__in Z3_context c, __in Z3_const_ast a);
    
    /**
       \brief Convert a CONST_DECL_AST into an AST. This is just type casting.
    */
    Z3_ast Z3_API Z3_const_decl_ast_to_ast(__in Z3_context c, __in Z3_const_decl_ast a);
    
    /**
       \brief Convert a PATTERN_AST into an AST. This is just type casting.
    */
    Z3_ast Z3_API Z3_pattern_ast_to_ast(__in Z3_context c, __in Z3_pattern_ast p);
    
    /**
       \brief Convert an AST into a CONST_AST. This is just type casting.
       
       \warning This conversion is only safe if #Z3_get_ast_kind returns \c Z3_CONST_AST.
    */
    Z3_const_ast Z3_API Z3_to_const_ast(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Convert an AST into a NUMERAL_AST. This is just type casting.
       
       \warning This conversion is only safe if #Z3_get_ast_kind returns \c Z3_NUMERAL_AST.
    */
    Z3_numeral_ast Z3_API Z3_to_numeral_ast(__in Z3_context c, __in Z3_ast a);
    /*@}*/
    
    /**
       @name Constraints
    */
    /*@{*/

    /** 
        \brief Create a backtracking point.
        
        The logical context can be viewed as a stack of contexts.  The
        scope level is the number of elements on this stack. The stack
        of contexts is simulated using trail (undo) stacks.

        \sa Z3_pop
    */
    void Z3_API Z3_push(__in Z3_context c);
    
    /**
       \brief Backtrack.
       
       Restores the context from the top of the stack, and pops it off the
       stack.  Any changes to the logical context (by #Z3_assert_cnstr or
       other functions) between the matching #Z3_push and \c Z3_pop
       operators are flushed, and the context is completely restored to
       what it was right before the #Z3_push.
       
       \sa Z3_push
    */
    void Z3_API Z3_pop(__in Z3_context c, __in unsigned num_scopes);

    /**
       \brief Assert a constraing into the logical context.
       
       After one assertion, the logical context may become
       inconsistent.  
       
       The functions #Z3_check or #Z3_check_and_get_model should be
       used to check whether the logical context is consistent or not.

       \sa Z3_check
       \sa Z3_check_and_get_model
    */
    void Z3_API Z3_assert_cnstr(__in Z3_context c, __in Z3_ast a);
    
    /**
       \brief Check whether the given logical context is consistent or not.

       If the logical context is not unsatisfiable (i.e., the return value is different from \c Z3_L_FALSE)
       and model construction is enabled (see #Z3_mk_config), then a model is stored in \c m. Otherwise,
       the value \c 0 is stored in \c m.
       The caller is responsible for deleting the model using the function #Z3_del_model.
       
       \remark Model construction must be enabled using configuration
       parameters (See, #Z3_mk_config).

       \sa Z3_check
       \sa Z3_del_model
    */
    Z3_lbool Z3_API Z3_check_and_get_model(__in Z3_context c, __out Z3_model * m);
    
    /**
       \brief Check whether the given logical context is consistent or not.

       The function #Z3_check_and_get_model should be used when models are needed.

       \sa Z3_check_and_get_model
    */
    Z3_lbool Z3_API Z3_check(__in Z3_context c);
    

    /**
       \brief Delete a model object.
       
       \sa Z3_check_and_get_model
    */
    void Z3_API Z3_del_model(__in Z3_model m);

    /** 
        \brief Interface to simplifier.

        Provides an interface to the AST simplifier used by Z3.
        It allows clients to piggyback on top of the AST simplifier
        for their own term manipulation.
    */
    Z3_ast Z3_API Z3_simplify(__in Z3_context c, __in Z3_ast a);


    /** 
        \brief Retrieve the set of labels that were relevant in
        the context of the current satisfied context.

        \sa Z3_del_labels
        \sa Z3_get_num_labels
        \sa Z3_get_label_symbol
    */
    Z3_labels Z3_API Z3_get_relevant_labels(__in Z3_context c);


    /**
       \brief Delete a labels context.
       
       \sa Z3_get_relevant_labels
    */
    void Z3_API Z3_del_labels(__in Z3_context c, __in Z3_labels lbls);

    /**
       \brief Retrieve the number of label symbols that were returned.
       
       \sa Z3_get_relevant_labels
    */
    unsigned Z3_API Z3_get_num_labels(__in Z3_context c, __in Z3_labels lbls);

    /**
       \brief Retrieve label symbol at idx.

    */
    Z3_symbol Z3_API Z3_get_label_symbol(__in Z3_context c, __in Z3_labels lbls, __in unsigned idx);

    /**
       \brief Disable label.
       
       The disabled label is not going to be used when blocking the subsequent search.

       \sa Z3_block_labels
    */
    void Z3_API Z3_disable_label(__in Z3_context c, __in Z3_labels lbls, __in unsigned idx);

    /**
       \brief Block subsequent checks using the remaining enabled labels.
    */
    void Z3_API Z3_block_labels(__in Z3_context c, __in Z3_labels lbls);

    /*@}*/

    /**
       @name Model navigation
     */
    /*@{*/
    
    /**
       \brief Return the number of constants assigned by the given model.
       
       \mlonly \remark Consider using {!Z3.get_model_constants}. \endmlonly

       \sa Z3_get_model_constant
    */
    unsigned Z3_API Z3_get_model_num_constants(__in Z3_context c, __in Z3_model m);

    /**
       \brief \mlh get_model_constant c m i \endmlh
       Return the i-th constant in the given model. 

       \mlonly \remark Consider using {!Z3.get_model_constants}. \endmlonly

       \pre i < Z3_get_model_num_constants(c, m)

       \sa Z3_get_value
    */
    Z3_const_decl_ast Z3_API Z3_get_model_constant(__in Z3_context c, __in Z3_model m, __in unsigned i);

    /**
       \brief Return the value of the given constant or application in the given model.
       
       \sa Z3_get_value_kind
       \sa Z3_get_value_type
    */
    Z3_value Z3_API Z3_get_value(__in Z3_context c, __in Z3_model m, __in Z3_const_decl_ast decl);

    /**
       \brief Return the value type.
       
       \sa Z3_get_value
    */
    Z3_type_ast Z3_API Z3_get_value_type(__in Z3_context c, __in Z3_value v);

    /**
       \brief Return the value kind (numeral, array, tuple, etc). 
       
       Z3_NUMERAL_VALUE stores the value of different types (int, real, bv, and uninterpreted).
    */
    Z3_value_kind Z3_API Z3_get_value_kind(__in Z3_context c, __in Z3_value v);

    /**
       \brief \mlh get_numeral_value_string c v \endmlh
       Return a numeral value as a string. The representation is stored in decimal notation.

       \pre Z3_get_value_kind(c, v) == Z3_NUMERAL_VALUE
       
       \conly \warning The resultant buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_get_numeral_value_string.

       \sa Z3_get_value_kind
    */
    Z3_string Z3_API Z3_get_numeral_value_string(__in Z3_context c, __in Z3_value v);

    /**
       \brief \mlh get_numeral_value_int c v \endmlh
       Similar to #Z3_get_numeral_value_string, but only succeeds if
       the value can fit in a machine int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_value_kind(c, v) == Z3_NUMERAL_VALUE
      
       \sa Z3_get_numeral_value_string
    */
    Z3_bool Z3_API Z3_get_numeral_value_int(__in Z3_context c, __in Z3_value v, __out int* i);

    /**
       \brief \mlh get_numeral_value_uint c v \endmlh
       Similar to #Z3_get_numeral_value_string, but only succeeds if
       the value can fit in a machine unsigned int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_value_kind(c, v) == Z3_NUMERAL_VALUE
      
       \sa Z3_get_numeral_value_string
    */
    Z3_bool Z3_API Z3_get_numeral_value_uint(__in Z3_context c, __in Z3_value v, __out unsigned* u);

#ifndef CAMLIDL
    /**
       \brief \mlh get_numeral_value_uint64 c v \endmlh
       Similar to #Z3_get_numeral_value_string, but only succeeds if
       the value can fit in a machine unsigned long long int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_value_kind(c, v) == Z3_NUMERAL_VALUE
      
       \sa Z3_get_numeral_value_string
    */
    Z3_bool Z3_API Z3_get_numeral_value_uint64(__in Z3_context c, __in Z3_value v, __out unsigned long long* u);
#endif // CAMLIDL

#ifndef CAMLIDL
    /**
       \brief \mlh get_numeral_value_int64 c v \endmlh
       Similar to #Z3_get_numeral_value_string, but only succeeds if
       the value can fit in a machine long long int. Return Z3_TRUE if the call succeeded.

       \pre Z3_get_value_kind(c, v) == Z3_NUMERAL_VALUE

       \sa Z3_get_numeral_value_string
    */
    Z3_bool Z3_API Z3_get_numeral_value_int64(__in Z3_context c, __in Z3_value v, __out long long* i);
#endif // CAMLIDL
    
    /**
       \brief \mlh get_bool_value_bool c v \endmlh
       Return the value of \c v as a Boolean value.

       \pre Z3_get_value_kind(c, v) == Z3_BOOL_VALUE

       \sa Z3_get_value_kind
    */
    Z3_bool Z3_API Z3_get_bool_value_bool(__in Z3_context c, __in Z3_value v);
    
    /**
       \brief \mlh get_tuple_value_mk_decl c v \endmlh
       Return the constructor declaration of the given tuple
       value. 

       \mlonly \remark Consider using {!Z3.get_tuple_value}. \endmlonly
       
       \pre Z3_get_value_kind(c, v) == Z3_TUPLE_VALUE

       \sa Z3_get_value_kind
    */
    Z3_const_decl_ast Z3_API Z3_get_tuple_value_mk_decl(__in Z3_context c, __in Z3_value v);

    /**    
       \brief \mlh get_tuple_value_num_fields c v \endmlh
       Return the number of fields of the given tuple value. 

       \mlonly \remark Consider using {!Z3.get_tuple_value}. \endmlonly

       \pre Z3_get_value_kind(c, t) == Z3_TUPLE_VALUE

       \sa Z3_get_value_kind
    */
    unsigned Z3_API Z3_get_tuple_value_num_fields(__in Z3_context c, __in Z3_value v);

    /**
       \brief \mlh get_tuple_value_field c v i \endmlh
       Return the i-th field of the given tuple value.

       \mlonly \remark Consider using {!Z3.get_tuple_value}. \endmlonly

       \pre Z3_get_value_kind(v) == Z3_TUPLE_VALUE
       \pre i < Z3_get_tuple_value_num_fields(c, v)
       
       \sa Z3_get_value_kind
    */
    Z3_value Z3_API Z3_get_tuple_value_field(__in Z3_context c, __in Z3_value v, __in unsigned i);

    /**
       \brief \mlh get_array_value_size c v \endmlh
       An array values is represented as a dictionary plus a
       default (else) value. This function returns the size of the
       dictionary. 

       \mlonly \remark Consider using {!Z3.get_array_value}. \endmlonly

       \pre Z3_get_value_kind(v) == Z3_ARRAY_VALUE

       \sa Z3_get_value_kind
    */
    unsigned Z3_API Z3_get_array_value_size(__in Z3_context c, __in Z3_value v);

    /**
       \brief \mlh get_array_value_else c v \endmlh
       An array values is represented as a dictionary plus a
       default (else) value. This function returns the default (else) value.

       \mlonly \remark Consider using {!Z3.get_array_value}. \endmlonly
       
       \pre Z3_get_value_kind(v) == Z3_ARRAY_VALUE

       \sa Z3_get_value_kind
    */
    Z3_value Z3_API Z3_get_array_value_else(__in Z3_context c, __in Z3_value v);

    /**
       \brief \mlh get_array_value_entry_index c v i \endmlh
       An array values is represented as a dictionary plus a
       default (else) value. Each dictionary entry is a pair (index, value).
       This function return the i-th index of the array. 

       If \c v contains an entry (index, value), then 
       \conly <tt>v[index] = value</tt>.
       \mlonly {e v\[index\] = value}. \endmlonly

       \mlonly \remark Consider using {!Z3.get_array_value}. \endmlonly

       \pre Z3_get_value_kind(v) == Z3_ARRAY_VALUE
       \pre i < Z3_get_array_value_size(c, v)

       \sa Z3_get_value_kind
    */
    Z3_value Z3_API Z3_get_array_value_entry_index(__in Z3_context c, __in Z3_value v, __in unsigned i);

    /**
       \brief \mlh get_array_value_entry_value c v i \endmlh
       An array values is represented as a dictionary plus a
       default (else) value. Each dictionary entry is a pair (index, value).
       This function return the i-th value of the array. 
       
       If \c v contains an entry (index, value), then 
       \conly <tt>v[index] = value</tt>.
       \mlonly {e v\[index\] = value}. \endmlonly

       \mlonly \remark Consider using {!Z3.get_array_value}. \endmlonly

       \pre Z3_get_value_kind(v) == Z3_ARRAY_VALUE
       \pre i < Z3_get_array_value_size(c, v)

       \sa Z3_get_value_kind
    */
    Z3_value Z3_API Z3_get_array_value_entry_value(__in Z3_context c, __in Z3_value v, __in unsigned i);

    /**
       \brief Return the number of function interpretations in the given model.
       
       A function interpretation is represented as a finite map and an 'else' value.
       Each entry in the finite map represents the value of a function given a set of arguments.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly
       
       \sa Z3_get_model_func_decl
       \sa Z3_get_model_func_else
       \sa Z3_get_model_func_num_entries
       \sa Z3_get_model_func_entry_num_args
       \sa Z3_get_model_func_entry_arg
     */
    unsigned Z3_API Z3_get_model_num_funcs(__in Z3_context c, __in Z3_model m);
    
    /**
       \brief \mlh is_model_func_internal c m i \endmlh
       Return \c Z3_true if the i-th function in the model is \em internal.

       Internal functions are created by Z3, and their interpretations
       are not usually useful for users.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly

       \pre i < Z3_get_model_num_funcs(c, m)

       \sa Z3_get_model_num_funcs
    */
    Z3_bool Z3_API Z3_is_model_func_internal(__in Z3_context c, __in Z3_model m, __in unsigned i);
    
    /**
       \brief \mlh get_model_func_decl c m i \endmlh
       Return the declaration of the i-th function in the given model.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly

       \pre i < Z3_get_model_num_funcs(c, m)

       \sa Z3_get_model_num_funcs
    */
    Z3_const_decl_ast Z3_API Z3_get_model_func_decl(__in Z3_context c, __in Z3_model m, __in unsigned i);

    /**
       \brief \mlh get_model_func_else c m i \endmlh
       Return the 'else' value of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly
       
       \pre i < Z3_get_model_num_funcs(c, m)

       \sa Z3_get_model_num_funcs
       \sa Z3_get_model_func_num_entries
       \sa Z3_get_model_func_entry_num_args
       \sa Z3_get_model_func_entry_arg
    */
    Z3_value Z3_API Z3_get_model_func_else(__in Z3_context c, __in Z3_model m, __in unsigned i);

    /**
       \brief \mlh get_model_func_num_entries c m i \endmlh
       Return the number of entries of the i-th function interpretation in the given model.
 
       A function interpretation is represented as a finite map and an 'else' value.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly
       
       \pre i < Z3_get_model_num_funcs(c, m)

       \sa Z3_get_model_num_funcs
       \sa Z3_get_model_func_else
       \sa Z3_get_model_func_entry_num_args
       \sa Z3_get_model_func_entry_arg
    */
    unsigned Z3_API Z3_get_model_func_num_entries(__in Z3_context c, __in Z3_model m, __in unsigned i);

    
    /**
       \brief \mlh get_model_func_entry_num_args c m i j \endmlh
       Return the number of arguments of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       \conly That is: it has the following format \<tt>f(args[0],...,args[num_args - 1]) = val</tt>.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly

       \pre i < Z3_get_model_num_funcs(c, m)
       \pre j < Z3_get_model_func_num_entries(c, m, i)

       \sa Z3_get_model_num_funcs
       \sa Z3_get_model_func_num_entries 
       \sa Z3_get_model_func_entry_arg
    */
    unsigned Z3_API Z3_get_model_func_entry_num_args(__in Z3_context c,
                                                     __in Z3_model m,
                                                     __in unsigned i,
                                                     __in unsigned j);
    
    /**
       \brief \mlh get_model_func_entry_arg c m i j k \endmlh
       Return the k-th argument of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       \conly That is: it has the following format \<tt>f(args[0],...,args[num_args - 1]) = val</tt>.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly

       \pre i < Z3_get_model_num_funcs(c, m)
       \pre j < Z3_get_model_func_num_entries(c, m, i)
       \pre k < Z3_get_model_func_entry_num_args(c, m, i, j)

       \sa Z3_get_model_num_funcs
       \sa Z3_get_model_func_num_entries 
       \sa Z3_get_model_func_entry_num_args
    */
    Z3_value Z3_API Z3_get_model_func_entry_arg(__in Z3_context c,
                                                __in Z3_model m,
                                                __in unsigned i,
                                                __in unsigned j,
                                                __in unsigned k);
    
    /**
       \brief \mlh get_model_func_entry_value c m i j \endmlh
       Return the return value of the j-th entry of the i-th function interpretation in the given
       model.

       A function interpretation is represented as a finite map and an 'else' value.
       This function returns the j-th entry of this map.
      
       An entry represents the value of a function given a set of arguments.
       \conly That is: it has the following format \<tt>f(args[0],...,args[num_args - 1]) = val</tt>.

       \mlonly \remark Consider using {!Z3.get_model_funcs}. \endmlonly

       \pre i < Z3_get_model_num_funcs(c, m)
       \pre j < Z3_get_model_func_num_entries(c, m, i)

       \sa Z3_get_model_num_funcs
       \sa Z3_get_model_func_num_entries 
    */
    Z3_value Z3_API Z3_get_model_func_entry_value(__in Z3_context c,
                                                  __in Z3_model m,
                                                  __in unsigned i,
                                                  __in unsigned j);
    
    /**
       \brief \mlh eval c m t \endmlh
       Evaluate the AST node \c t in the given model. 
       \conly Return \c Z3_TRUE if succeeded, and store the result in \c v.
       \mlonly Return a pair: Boolean and value. The Boolean is true if the term was successfully evaluated. \endmlonly

       The evaluation may fail for the following reasons:

       - \c t contains a quantifier or bound variable. 

       - the model \c m is partial, that is, it doesn't have a complete interpretation for free functions. That is, the option <tt>PARTIAL_MODELS=true</tt> was used.

       - the evaluator doesn't have support for some interpreted operator.

       - \c t is type incorrect (see #Z3_type_check).

       - The result of an intepreted operator in \c t is undefined (e.g. division by zero).
    */
    Z3_bool Z3_API Z3_eval(__in Z3_context c, __in Z3_model m, __in Z3_ast t, __out Z3_value * v);
    /*@}*/

    /**
       @name Timers
    */
    /*@{*/

    /**
       \brief \mlh set_soft_timeout c t \endmlh
       Set a soft timeout in seconds. 

       A soft timeout limits the amount of time spent in
       #Z3_check_and_get_model and #Z3_check.  If a call to
       #Z3_check_and_get_model or #Z3_check consumes more than \c t 
       seconds, then it aborts and returns \c Z3_L_UNDEF.

       \sa Z3_reset_soft_timeout
    */
    void Z3_API Z3_set_soft_timeout(__in Z3_context c, __in unsigned t);

    /**
       \brief Disable soft timeouts.
       
       \sa Z3_set_soft_timeout
    */
    void Z3_API Z3_reset_soft_timeout(__in Z3_context c);
    /*@}*/

    /**
       @name Interaction logging.
    */
    /*@{*/
    
    /**
       \brief Log interaction to a file.
    */
    Z3_bool Z3_API Z3_open_log(__in Z3_context c, __in_z Z3_string filename);

    /**
       \brief Close interaction log.
    */
    void Z3_API Z3_close_log(__in Z3_context c);
    /*@}*/


    /**
       @name String conversion
    */
    /*@{*/
    /**
       \brief Convert the given AST node into a string.

       \conly \warning The result buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_ast_to_string.
    */
    Z3_string Z3_API Z3_ast_to_string(__in Z3_context c, __in Z3_ast a);

    /**
       \brief Convert the given model into a string.

       \conly \warning The result buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_model_to_string.
    */
    Z3_string Z3_API Z3_model_to_string(__in Z3_context c, __in Z3_model m);
    
    /**
       \brief Convert the given (model) value into a string.

       \conly \warning The result buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_value_to_string.
    */
    Z3_string Z3_API Z3_value_to_string(__in Z3_context c, __in Z3_value v);
    
    /**
       \brief Convert the given logical context into a string.
       
       This function is mainly used for debugging purposes. It displays
       the internal structure of a logical context.

       \conly \warning The result buffer is statically allocated by Z3. It will
       \conly be automatically deallocated when #Z3_del_context is invoked.
       \conly So, the buffer is invalidated in the next call to \c Z3_context_to_string.
    */
    Z3_string Z3_API Z3_context_to_string(__in Z3_context c);
    /*@}*/

    /**
       @name Parser interface
    */
    /*@{*/
    /**
       \brief \mlh parse_smtlib_string c str type_names types decl_names decls \endmlh
       Parse the given string using the SMT-LIB parser. 
              
       The symbol table of the parser can be initialized using the given types and declarations. 
       The symbols in the arrays \c type_names and \c decl_names don't need to match the names
       of the types and declarations in the arrays \c types and \c decls. This is an useful feature
       since we can use arbitrary names to reference types and declarations defined using the C API.

       The formulas, assumptions and declarations defined in \c str can be extracted using the functions:
       #Z3_get_smtlib_num_formulas, #Z3_get_smtlib_formula, #Z3_get_smtlib_num_assumptions, #Z3_get_smtlib_assumption, 
       #Z3_get_smtlib_num_decls, and #Z3_get_smtlib_decl.
     */
    void Z3_API Z3_parse_smtlib_string(__in Z3_context c, 
                                       __in_z Z3_string str,
                                       __in unsigned num_types,
                                       __in_ecount(num_types) Z3_symbol type_names[],
                                       __in_ecount(num_types) Z3_type_ast types[],
                                       __in unsigned num_decls,
                                       __in_ecount(num_decls) Z3_symbol decl_names[],
                                       __in_ecount(num_decls) Z3_const_decl_ast decls[]
                                       );
    
    /**
       \brief Similar to #Z3_parse_smtlib_string, but reads the benchmark from a file.
    */
    void Z3_API Z3_parse_smtlib_file(__in Z3_context c, 
                                     __in_z Z3_string file_name,
                                     __in unsigned num_types,
                                     __in_ecount(num_types) Z3_symbol type_names[],
                                     __in_ecount(num_types) Z3_type_ast types[],
                                     __in unsigned num_decls,
                                     __in_ecount(num_decls) Z3_symbol decl_names[],
                                     __in_ecount(num_decls) Z3_const_decl_ast decls[]
                                     );

    /**
       \brief Return the number of SMTLIB formulas parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.
    */
    unsigned Z3_API Z3_get_smtlib_num_formulas(__in Z3_context c);

    /**
       \brief \mlh get_smtlib_formula c i \endmlh
       Return the i-th formula parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_formulas(c)
    */
    Z3_ast Z3_API Z3_get_smtlib_formula(__in Z3_context c, __in unsigned i);

    /**
       \brief Return the number of SMTLIB assumptions parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.
    */
    unsigned Z3_API Z3_get_smtlib_num_assumptions(__in Z3_context c);

    /**
       \brief \mlh get_smtlib_assumption c i \endmlh
       Return the i-th assumption parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_assumptions(c)
    */
    Z3_ast Z3_API Z3_get_smtlib_assumption(__in Z3_context c, __in unsigned i);

    /**
       \brief Return the number of declarations parsed by #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.
    */
    unsigned Z3_API Z3_get_smtlib_num_decls(__in Z3_context c);

    /**
       \brief \mlh get_smtlib_decl c i \endmlh
       Return the i-th declaration parsed by the last call to #Z3_parse_smtlib_string or #Z3_parse_smtlib_file.

       \pre i < Z3_get_smtlib_num_decls(c)
    */
    Z3_const_decl_ast Z3_API Z3_get_smtlib_decl(__in Z3_context c, __in unsigned i);
    /*@}*/

#ifndef CAMLIDL
    /**
       @name Error Handling
    */
    /*@{*/

    /**
       \brief Return the error code for the last API call.
       
       A call to a Z3 function may return a non Z3_OK error code,
       when it is not used correctly.

       \sa Z3_set_error_handler
    */
    Z3_error_code Z3_API Z3_get_error_code(__in Z3_context c);

    /**
       \brief Register a Z3 error handler.
       
       A call to a Z3 function may return a non Z3_OK error code, when
       it is not used correctly.  An error handler can be registered
       and will be called in this case.  To disable the use of the
       error handler, simply register with h=NULL.

       \sa Z3_get_error_code
    */
    void Z3_API Z3_set_error_handler(__in Z3_context c, __in Z3_error_handler h);

    /**
       \brief Return a string describing the given error code.
     */
    Z3_string Z3_API Z3_get_error_msg(__in Z3_error_code err);
    /*@}*/

#endif // CAMLIDL

    /**
       @name Miscellaneous
    */
    /*@{*/
    
    /**
       \brief Return Z3 version number information.
    */
    void Z3_API Z3_get_version(__out unsigned * major, 
                               __out unsigned * minor, 
                               __out unsigned * build_number, 
                               __out unsigned * revision_number);

    
    /**
       \brief \mlh type_check c t \endmlh
       Return Z3_TRUE if \c t is type correct.
    */
    Z3_bool Z3_API Z3_type_check(__in Z3_context c, __in Z3_ast t);

    /**
       \brief Return the amount of memory allocated by Z3.
    */
    unsigned Z3_API Z3_get_allocation_size();
    /*@}*/
    
#ifndef CAMLIDL
#ifdef __cplusplus
};
#endif // __cplusplus
#else
}
#endif // CAMLIDL

/*@}*/
