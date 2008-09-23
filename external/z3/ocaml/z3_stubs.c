/* File generated from z3.idl */

#include <stddef.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/callback.h>
#ifdef Custom_tag
#include <caml/custom.h>
#include <caml/bigarray.h>
#endif
#include <caml/camlidlruntime.h>


#include "z3.h"

#pragma warning(disable:4090)
Z3_error_handler caml_z3_error_handler;
void caml_z3_error_handler(Z3_error_code e) { static char buffer[128]; char * msg = Z3_get_error_msg(e); if (strlen(msg) > 100) { failwith("Z3: error message is too big"); } else { sprintf(buffer, "Z3: %s", msg); failwith(buffer); } }
void camlidl_ml2c_z3_Z3_config(value _v1, Z3_config * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_config *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_config(Z3_config * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_config) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_config *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_context(value _v1, Z3_context * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_context *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_context(Z3_context * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_context) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_context *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_ast(value _v1, Z3_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_ast(Z3_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_type_ast(value _v1, Z3_type_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_type_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_type_ast(Z3_type_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_type_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_type_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_const_decl_ast(value _v1, Z3_const_decl_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_const_decl_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_const_decl_ast(Z3_const_decl_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_const_decl_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_const_decl_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_const_ast(value _v1, Z3_const_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_const_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_const_ast(Z3_const_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_const_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_const_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_numeral_ast(value _v1, Z3_numeral_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_numeral_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_numeral_ast(Z3_numeral_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_numeral_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_numeral_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_pattern_ast(value _v1, Z3_pattern_ast * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_pattern_ast *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_pattern_ast(Z3_pattern_ast * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_pattern_ast) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_pattern_ast *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_symbol(value _v1, Z3_symbol * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_symbol *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_symbol(Z3_symbol * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_symbol) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_symbol *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_value(value _v1, Z3_value * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_value *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_value(Z3_value * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_value) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_value *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_parameter(value _v1, Z3_parameter * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_parameter *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_parameter(Z3_parameter * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_parameter) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_parameter *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_model(value _v1, Z3_model * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_model *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_model(Z3_model * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_model) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_model *) Bp_val(_v1)) = *_c2;
  return _v1;
}

void camlidl_ml2c_z3_Z3_labels(value _v1, Z3_labels * _c2, camlidl_ctx _ctx)
{
  *_c2 = *((Z3_labels *) Bp_val(_v1));
}

value camlidl_c2ml_z3_Z3_labels(Z3_labels * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_alloc((sizeof(Z3_labels) + sizeof(value) - 1) / sizeof(value), Abstract_tag);
  *((Z3_labels *) Bp_val(_v1)) = *_c2;
  return _v1;
}

int camlidl_transl_table_z3_enum_1[3] = {
  Z3_L_FALSE,
  Z3_L_UNDEF,
  Z3_L_TRUE,
};

void camlidl_ml2c_z3_Z3_lbool(value _v1, Z3_lbool * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_1[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_lbool(Z3_lbool * _c2, camlidl_ctx _ctx)
{
value _v1;
  switch((*_c2)) {
  case Z3_L_FALSE: _v1 = Val_int(0); break;
  case Z3_L_UNDEF: _v1 = Val_int(1); break;
  case Z3_L_TRUE: _v1 = Val_int(2); break;
  default: invalid_argument("typedef Z3_lbool: bad enum  value");
  }
  return _v1;
}

int camlidl_transl_table_z3_enum_2[2] = {
  Z3_INT_SYMBOL,
  Z3_STRING_SYMBOL,
};

void camlidl_ml2c_z3_Z3_symbol_kind(value _v1, Z3_symbol_kind * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_2[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_symbol_kind(Z3_symbol_kind * _c2, camlidl_ctx _ctx)
{
value _v1;
  switch((*_c2)) {
  case Z3_INT_SYMBOL: _v1 = Val_int(0); break;
  case Z3_STRING_SYMBOL: _v1 = Val_int(1); break;
  default: invalid_argument("typedef Z3_symbol_kind: bad enum  value");
  }
  return _v1;
}

int camlidl_transl_table_z3_enum_3[8] = {
  Z3_UNINTERPRETED_TYPE,
  Z3_BOOL_TYPE,
  Z3_INT_TYPE,
  Z3_REAL_TYPE,
  Z3_BV_TYPE,
  Z3_ARRAY_TYPE,
  Z3_TUPLE_TYPE,
  Z3_UNKNOWN_TYPE,
};

void camlidl_ml2c_z3_Z3_type_kind(value _v1, Z3_type_kind * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_3[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_type_kind(Z3_type_kind * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_z3_enum_3, 8, "typedef Z3_type_kind: bad enum  value");
  return _v1;
}

int camlidl_transl_table_z3_enum_4[8] = {
  Z3_NUMERAL_AST,
  Z3_CONST_DECL_AST,
  Z3_CONST_AST,
  Z3_TYPE_AST,
  Z3_VAR_AST,
  Z3_PATTERN_AST,
  Z3_QUANTIFIER_AST,
  Z3_UNKNOWN_AST,
};

void camlidl_ml2c_z3_Z3_ast_kind(value _v1, Z3_ast_kind * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_4[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_ast_kind(Z3_ast_kind * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_z3_enum_4, 8, "typedef Z3_ast_kind: bad enum  value");
  return _v1;
}

int camlidl_transl_table_z3_enum_5[80] = {
  Z3_OP_TRUE,
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
  Z3_OP_LE,
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
  Z3_OP_STORE,
  Z3_OP_SELECT,
  Z3_OP_CONST_ARRAY,
  Z3_OP_ARRAY_DEFAULT,
  Z3_OP_STORE_ITE,
  Z3_OP_SET_UNION,
  Z3_OP_SET_INTERSECT,
  Z3_OP_SET_DIFFERENCE,
  Z3_OP_SET_COMPLEMENT,
  Z3_OP_SET_SUBSET,
  Z3_OP_BIT1,
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
  Z3_OP_UNINTERPRETED,
};

void camlidl_ml2c_z3_Z3_decl_kind(value _v1, Z3_decl_kind * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_5[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_decl_kind(Z3_decl_kind * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_z3_enum_5, 80, "typedef Z3_decl_kind: bad enum  value");
  return _v1;
}

int camlidl_transl_table_z3_enum_6[5] = {
  Z3_BOOL_VALUE,
  Z3_NUMERAL_VALUE,
  Z3_ARRAY_VALUE,
  Z3_TUPLE_VALUE,
  Z3_UNKNOWN_VALUE,
};

void camlidl_ml2c_z3_Z3_value_kind(value _v1, Z3_value_kind * _c2, camlidl_ctx _ctx)
{
  (*_c2) = camlidl_transl_table_z3_enum_6[Int_val(_v1)];
}

value camlidl_c2ml_z3_Z3_value_kind(Z3_value_kind * _c2, camlidl_ctx _ctx)
{
value _v1;
  _v1 = camlidl_find_enum((*_c2), camlidl_transl_table_z3_enum_6, 5, "typedef Z3_value_kind: bad enum  value");
  return _v1;
}

value camlidl_z3_Z3_mk_config(value _unit)
{
  Z3_config _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  _res = Z3_mk_config();
  _vres = camlidl_c2ml_z3_Z3_config(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_del_config(
	value _v_c)
{
  Z3_config c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_config(_v_c, &c, _ctx);
  Z3_del_config(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_set_param_value(
	value _v_c,
	value _v_param_id,
	value _v_param_value)
{
  Z3_config c; /*in*/
  char const *param_id; /*in*/
  char const *param_value; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_config(_v_c, &c, _ctx);
  param_id = String_val(_v_param_id);
  param_value = String_val(_v_param_value);
  Z3_set_param_value(c, param_id, param_value);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_mk_context(
	value _v_c)
{
  Z3_config c; /*in*/
  Z3_context _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_config(_v_c, &c, _ctx);
  _res = Z3_mk_context(c);
  _vres = camlidl_c2ml_z3_Z3_context(&_res, _ctx);
  camlidl_free(_ctx);
  /* begin user-supplied deallocation sequence */
Z3_set_error_handler(_res, caml_z3_error_handler);
  /* end user-supplied deallocation sequence */
  return _vres;
}

value camlidl_z3_Z3_del_context(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_del_context(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_trace_to_file(
	value _v_c,
	value _v_trace_file)
{
  Z3_context c; /*in*/
  char const *trace_file; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  trace_file = String_val(_v_trace_file);
  _res = Z3_trace_to_file(c, trace_file);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_trace_to_stderr(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_trace_to_stderr(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_trace_to_stdout(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_trace_to_stdout(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_trace_off(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_trace_off(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_enable_arithmetic(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_enable_arithmetic(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_enable_bv(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_enable_bv(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_enable_arrays(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_enable_arrays(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_enable_tuples(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_enable_tuples(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_mk_int_symbol(
	value _v_c,
	value _v_i)
{
  Z3_context c; /*in*/
  int i; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  _res = Z3_mk_int_symbol(c, i);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_string_symbol(
	value _v_c,
	value _v_s)
{
  Z3_context c; /*in*/
  char const *s; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  s = String_val(_v_s);
  _res = Z3_mk_string_symbol(c, s);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_uninterpreted_type(
	value _v_c,
	value _v_s)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  _res = Z3_mk_uninterpreted_type(c, s);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bool_type(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_mk_bool_type(c);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_int_type(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_mk_int_type(c);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_real_type(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_mk_real_type(c);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bv_type(
	value _v_c,
	value _v_sz)
{
  Z3_context c; /*in*/
  unsigned int sz; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  sz = Int_val(_v_sz);
  _res = Z3_mk_bv_type(c, sz);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_array_type(
	value _v_c,
	value _v_domain,
	value _v_range)
{
  Z3_context c; /*in*/
  Z3_type_ast domain; /*in*/
  Z3_type_ast range; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_domain, &domain, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_range, &range, _ctx);
  _res = Z3_mk_array_type(c, domain, range);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_tuple_type(
	value _v_c,
	value _v_mk_tuple_name,
	value _v_field_names,
	value _v_field_types)
{
  Z3_context c; /*in*/
  Z3_symbol mk_tuple_name; /*in*/
  unsigned int num_fields; /*in*/
  Z3_symbol const *field_names; /*in*/
  Z3_type_ast const *field_types; /*in*/
  Z3_const_decl_ast *mk_tuple_decl; /*out*/
  Z3_const_decl_ast *proj_decl; /*out*/
  Z3_type_ast _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  Z3_const_decl_ast _c7;
  mlsize_t _c8;
  value _v9;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_mk_tuple_name, &mk_tuple_name, _ctx);
  _c1 = Wosize_val(_v_field_names);
  field_names = camlidl_malloc(_c1 * sizeof(Z3_symbol const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_field_names, _c2);
    camlidl_ml2c_z3_Z3_symbol(_v3, &field_names[_c2], _ctx);
  }
  num_fields = _c1;
  _c4 = Wosize_val(_v_field_types);
  field_types = camlidl_malloc(_c4 * sizeof(Z3_type_ast const ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_field_types, _c5);
    camlidl_ml2c_z3_Z3_type_ast(_v6, &field_types[_c5], _ctx);
  }
  num_fields = _c4;
  mk_tuple_decl = &_c7;
  proj_decl = camlidl_malloc(num_fields * sizeof(Z3_const_decl_ast ), _ctx);
  _res = Z3_mk_tuple_type(c, mk_tuple_name, num_fields, field_names, field_types, mk_tuple_decl, proj_decl);
  Begin_roots_block(_vres, 3)
    _vres[0] = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
    _vres[1] = camlidl_c2ml_z3_Z3_const_decl_ast(&*mk_tuple_decl, _ctx);
    _vres[2] = camlidl_alloc(num_fields, 0);
    Begin_root(_vres[2])
      for (_c8 = 0; _c8 < num_fields; _c8++) {
        _v9 = camlidl_c2ml_z3_Z3_const_decl_ast(&proj_decl[_c8], _ctx);
        modify(&Field(_vres[2], _c8), _v9);
      }
    End_roots()
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_mk_func_decl(
	value _v_c,
	value _v_s,
	value _v_domain,
	value _v_range)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  unsigned int domain_size; /*in*/
  Z3_type_ast const *domain; /*in*/
  Z3_type_ast range; /*in*/
  Z3_const_decl_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  _c1 = Wosize_val(_v_domain);
  domain = camlidl_malloc(_c1 * sizeof(Z3_type_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_domain, _c2);
    camlidl_ml2c_z3_Z3_type_ast(_v3, &domain[_c2], _ctx);
  }
  domain_size = _c1;
  camlidl_ml2c_z3_Z3_type_ast(_v_range, &range, _ctx);
  _res = Z3_mk_func_decl(c, s, domain_size, domain, range);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_app(
	value _v_c,
	value _v_d,
	value _v_args)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_app(c, d, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_const(
	value _v_c,
	value _v_s,
	value _v_ty)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_const(c, s, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_label(
	value _v_c,
	value _v_s,
	value _v_is_pos,
	value _v_f)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  int is_pos; /*in*/
  Z3_ast f; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  is_pos = Int_val(_v_is_pos);
  camlidl_ml2c_z3_Z3_ast(_v_f, &f, _ctx);
  _res = Z3_mk_label(c, s, is_pos, f);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_fresh_func_decl(
	value _v_c,
	value _v_prefix,
	value _v_domain,
	value _v_range)
{
  Z3_context c; /*in*/
  char const *prefix; /*in*/
  unsigned int domain_size; /*in*/
  Z3_type_ast const *domain; /*in*/
  Z3_type_ast range; /*in*/
  Z3_const_decl_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  prefix = String_val(_v_prefix);
  _c1 = Wosize_val(_v_domain);
  domain = camlidl_malloc(_c1 * sizeof(Z3_type_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_domain, _c2);
    camlidl_ml2c_z3_Z3_type_ast(_v3, &domain[_c2], _ctx);
  }
  domain_size = _c1;
  camlidl_ml2c_z3_Z3_type_ast(_v_range, &range, _ctx);
  _res = Z3_mk_fresh_func_decl(c, prefix, domain_size, domain, range);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_fresh_const(
	value _v_c,
	value _v_prefix,
	value _v_ty)
{
  Z3_context c; /*in*/
  char const *prefix; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  prefix = String_val(_v_prefix);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_fresh_const(c, prefix, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_true(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_mk_true(c);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_false(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_mk_false(c);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_eq(
	value _v_c,
	value _v_l,
	value _v_r)
{
  Z3_context c; /*in*/
  Z3_ast l; /*in*/
  Z3_ast r; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_l, &l, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_r, &r, _ctx);
  _res = Z3_mk_eq(c, l, r);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_distinct(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_distinct(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_not(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_mk_not(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_ite(
	value _v_c,
	value _v_t1,
	value _v_t2,
	value _v_t3)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast t3; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t3, &t3, _ctx);
  _res = Z3_mk_ite(c, t1, t2, t3);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_iff(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_iff(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_implies(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_implies(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_xor(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_xor(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_and(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_and(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_or(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_or(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_add(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_add(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_mul(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_mul(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_sub(
	value _v_c,
	value _v_args)
{
  Z3_context c; /*in*/
  unsigned int num_args; /*in*/
  Z3_ast const *args; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_args);
  args = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_args, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &args[_c2], _ctx);
  }
  num_args = _c1;
  _res = Z3_mk_sub(c, num_args, args);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_lt(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_lt(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_le(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_le(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_gt(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_gt(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_ge(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_ge(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvnot(
	value _v_c,
	value _v_t1)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_bvnot(c, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvand(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvand(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvor(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvor(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvxor(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvxor(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvnand(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvnand(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvnor(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvnor(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvxnor(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvxnor(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvneg(
	value _v_c,
	value _v_t1)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_bvneg(c, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvadd(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvadd(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsub(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsub(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvmul(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvmul(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvudiv(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvudiv(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsdiv(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsdiv(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvurem(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvurem(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsrem(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsrem(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsmod(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsmod(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvult(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvult(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvslt(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvslt(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvule(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvule(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsle(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsle(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvuge(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvuge(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsge(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsge(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvugt(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvugt(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvsgt(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvsgt(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_concat(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_concat(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_extract(
	value _v_c,
	value _v_high,
	value _v_low,
	value _v_t1)
{
  Z3_context c; /*in*/
  unsigned int high; /*in*/
  unsigned int low; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  high = Int_val(_v_high);
  low = Int_val(_v_low);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_extract(c, high, low, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_sign_ext(
	value _v_c,
	value _v_i,
	value _v_t1)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_sign_ext(c, i, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_zero_ext(
	value _v_c,
	value _v_i,
	value _v_t1)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_zero_ext(c, i, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvshl(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvshl(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvlshr(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvlshr(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bvashr(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_mk_bvashr(c, t1, t2);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_rotate_left(
	value _v_c,
	value _v_i,
	value _v_t1)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_rotate_left(c, i, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_rotate_right(
	value _v_c,
	value _v_i,
	value _v_t1)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  _res = Z3_mk_rotate_right(c, i, t1);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_select(
	value _v_c,
	value _v_a,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast i; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_i, &i, _ctx);
  _res = Z3_mk_select(c, a, i);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_store(
	value _v_c,
	value _v_a,
	value _v_i,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast i; /*in*/
  Z3_ast v; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_i, &i, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_v, &v, _ctx);
  _res = Z3_mk_store(c, a, i, v);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_numeral(
	value _v_c,
	value _v_numeral,
	value _v_ty)
{
  Z3_context c; /*in*/
  char const *numeral; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  numeral = String_val(_v_numeral);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_numeral(c, numeral, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_int(
	value _v_c,
	value _v_v,
	value _v_ty)
{
  Z3_context c; /*in*/
  int v; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  v = Int_val(_v_v);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_int(c, v, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_unsigned_int(
	value _v_c,
	value _v_v,
	value _v_ty)
{
  Z3_context c; /*in*/
  unsigned int v; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  v = Int_val(_v_v);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_unsigned_int(c, v, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_pattern(
	value _v_c,
	value _v_terms)
{
  Z3_context c; /*in*/
  unsigned int num_patterns; /*in*/
  Z3_ast const *terms; /*in*/
  Z3_pattern_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _c1 = Wosize_val(_v_terms);
  terms = camlidl_malloc(_c1 * sizeof(Z3_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_terms, _c2);
    camlidl_ml2c_z3_Z3_ast(_v3, &terms[_c2], _ctx);
  }
  num_patterns = _c1;
  _res = Z3_mk_pattern(c, num_patterns, terms);
  _vres = camlidl_c2ml_z3_Z3_pattern_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_bound(
	value _v_c,
	value _v_index,
	value _v_ty)
{
  Z3_context c; /*in*/
  unsigned int index; /*in*/
  Z3_type_ast ty; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  index = Int_val(_v_index);
  camlidl_ml2c_z3_Z3_type_ast(_v_ty, &ty, _ctx);
  _res = Z3_mk_bound(c, index, ty);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_forall(
	value _v_c,
	value _v_weight,
	value _v_patterns,
	value _v_types,
	value _v_decl_names,
	value _v_body)
{
  Z3_context c; /*in*/
  unsigned int weight; /*in*/
  unsigned int num_patterns; /*in*/
  Z3_pattern_ast const *patterns; /*in*/
  unsigned int num_decls; /*in*/
  Z3_type_ast const *types; /*in*/
  Z3_symbol const *decl_names; /*in*/
  Z3_ast body; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  weight = Int_val(_v_weight);
  _c1 = Wosize_val(_v_patterns);
  patterns = camlidl_malloc(_c1 * sizeof(Z3_pattern_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_patterns, _c2);
    camlidl_ml2c_z3_Z3_pattern_ast(_v3, &patterns[_c2], _ctx);
  }
  num_patterns = _c1;
  _c4 = Wosize_val(_v_types);
  types = camlidl_malloc(_c4 * sizeof(Z3_type_ast const ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_types, _c5);
    camlidl_ml2c_z3_Z3_type_ast(_v6, &types[_c5], _ctx);
  }
  num_decls = _c4;
  _c7 = Wosize_val(_v_decl_names);
  decl_names = camlidl_malloc(_c7 * sizeof(Z3_symbol const ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_decl_names, _c8);
    camlidl_ml2c_z3_Z3_symbol(_v9, &decl_names[_c8], _ctx);
  }
  num_decls = _c7;
  camlidl_ml2c_z3_Z3_ast(_v_body, &body, _ctx);
  _res = Z3_mk_forall(c, weight, num_patterns, patterns, num_decls, types, decl_names, body);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_forall_bytecode(value * argv, int argn)
{
  return camlidl_z3_Z3_mk_forall(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_z3_Z3_mk_exists(
	value _v_c,
	value _v_weight,
	value _v_patterns,
	value _v_types,
	value _v_decl_names,
	value _v_body)
{
  Z3_context c; /*in*/
  unsigned int weight; /*in*/
  unsigned int num_patterns; /*in*/
  Z3_pattern_ast const *patterns; /*in*/
  unsigned int num_decls; /*in*/
  Z3_type_ast const *types; /*in*/
  Z3_symbol const *decl_names; /*in*/
  Z3_ast body; /*in*/
  Z3_ast _res;
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  weight = Int_val(_v_weight);
  _c1 = Wosize_val(_v_patterns);
  patterns = camlidl_malloc(_c1 * sizeof(Z3_pattern_ast const ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_patterns, _c2);
    camlidl_ml2c_z3_Z3_pattern_ast(_v3, &patterns[_c2], _ctx);
  }
  num_patterns = _c1;
  _c4 = Wosize_val(_v_types);
  types = camlidl_malloc(_c4 * sizeof(Z3_type_ast const ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_types, _c5);
    camlidl_ml2c_z3_Z3_type_ast(_v6, &types[_c5], _ctx);
  }
  num_decls = _c4;
  _c7 = Wosize_val(_v_decl_names);
  decl_names = camlidl_malloc(_c7 * sizeof(Z3_symbol const ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_decl_names, _c8);
    camlidl_ml2c_z3_Z3_symbol(_v9, &decl_names[_c8], _ctx);
  }
  num_decls = _c7;
  camlidl_ml2c_z3_Z3_ast(_v_body, &body, _ctx);
  _res = Z3_mk_exists(c, weight, num_patterns, patterns, num_decls, types, decl_names, body);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_exists_bytecode(value * argv, int argn)
{
  return camlidl_z3_Z3_mk_exists(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_z3_Z3_mk_quantifier(
	value _v_c,
	value _v_is_forall,
	value _v_weight,
	value _v_num_patterns,
	value _v_patterns,
	value _v_num_no_patterns,
	value _v_no_patterns,
	value _v_num_decls,
	value _v_types,
	value _v_decl_names,
	value _v_body)
{
  Z3_context c; /*in*/
  int is_forall; /*in*/
  unsigned int weight; /*in*/
  unsigned int num_patterns; /*in*/
  Z3_pattern_ast const *patterns; /*in*/
  unsigned int num_no_patterns; /*in*/
  Z3_ast const *no_patterns; /*in*/
  unsigned int num_decls; /*in*/
  Z3_type_ast const *types; /*in*/
  Z3_symbol const *decl_names; /*in*/
  Z3_ast body; /*in*/
  Z3_ast _res;
  Z3_pattern_ast _c1;
  Z3_ast _c2;
  Z3_type_ast _c3;
  Z3_symbol _c4;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  is_forall = Int_val(_v_is_forall);
  weight = Int_val(_v_weight);
  num_patterns = Int_val(_v_num_patterns);
  patterns = &_c1;
  camlidl_ml2c_z3_Z3_pattern_ast(_v_patterns, &_c1, _ctx);
  num_no_patterns = Int_val(_v_num_no_patterns);
  no_patterns = &_c2;
  camlidl_ml2c_z3_Z3_ast(_v_no_patterns, &_c2, _ctx);
  num_decls = Int_val(_v_num_decls);
  types = &_c3;
  camlidl_ml2c_z3_Z3_type_ast(_v_types, &_c3, _ctx);
  decl_names = &_c4;
  camlidl_ml2c_z3_Z3_symbol(_v_decl_names, &_c4, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_body, &body, _ctx);
  _res = Z3_mk_quantifier(c, is_forall, weight, num_patterns, patterns, num_no_patterns, no_patterns, num_decls, types, decl_names, body);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_mk_quantifier_bytecode(value * argv, int argn)
{
  return camlidl_z3_Z3_mk_quantifier(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value camlidl_z3_Z3_get_symbol_kind(
	value _v_c,
	value _v_s)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  Z3_symbol_kind _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  _res = Z3_get_symbol_kind(c, s);
  _vres = camlidl_c2ml_z3_Z3_symbol_kind(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_symbol_int(
	value _v_c,
	value _v_s)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  _res = Z3_get_symbol_int(c, s);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_symbol_string(
	value _v_c,
	value _v_s)
{
  Z3_context c; /*in*/
  Z3_symbol s; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_symbol(_v_s, &s, _ctx);
  _res = Z3_get_symbol_string(c, s);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_is_eq(
	value _v_c,
	value _v_t1,
	value _v_t2)
{
  Z3_context c; /*in*/
  Z3_ast t1; /*in*/
  Z3_ast t2; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t1, &t1, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t2, &t2, _ctx);
  _res = Z3_is_eq(c, t1, t2);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_ast_kind(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast_kind _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_ast_kind(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast_kind(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_is_expr(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_is_expr(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_const_ast_decl(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_const_ast a; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_ast(_v_a, &a, _ctx);
  _res = Z3_get_const_ast_decl(c, a);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_const_ast_num_args(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_const_ast a; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_ast(_v_a, &a, _ctx);
  _res = Z3_get_const_ast_num_args(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_const_ast_arg(
	value _v_c,
	value _v_a,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_const_ast a; /*in*/
  unsigned int i; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_ast(_v_a, &a, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_const_ast_arg(c, a, i);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_decl_name(
	value _v_c,
	value _v_d)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  _res = Z3_get_decl_name(c, d);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_type_name(
	value _v_c,
	value _v_d)
{
  Z3_context c; /*in*/
  Z3_type_ast d; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_d, &d, _ctx);
  _res = Z3_get_type_name(c, d);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_type(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_type(c, a);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_domain_size(
	value _v_c,
	value _v_d)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  _res = Z3_get_domain_size(c, d);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_domain(
	value _v_c,
	value _v_d,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  unsigned int i; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_domain(c, d, i);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_range(
	value _v_c,
	value _v_d)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  _res = Z3_get_range(c, d);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_type_kind(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  Z3_type_kind _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_type_kind(c, t);
  _vres = camlidl_c2ml_z3_Z3_type_kind(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_bv_type_size(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_bv_type_size(c, t);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_type_domain(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_array_type_domain(c, t);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_type_range(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_array_type_range(c, t);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_type_mk_decl(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_tuple_type_mk_decl(c, t);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_type_num_fields(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  _res = Z3_get_tuple_type_num_fields(c, t);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_type_field_decl(
	value _v_c,
	value _v_t,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_type_ast t; /*in*/
  unsigned int i; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_t, &t, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_tuple_type_field_decl(c, t, i);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_decl_kind(
	value _v_c,
	value _v_d)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast d; /*in*/
  Z3_decl_kind _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_d, &d, _ctx);
  _res = Z3_get_decl_kind(c, d);
  _vres = camlidl_c2ml_z3_Z3_decl_kind(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_numeral_ast_value(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_numeral_ast_value(c, a);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_numeral_ast_value_small(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  int64 *n; /*out*/
  int64 *d; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int64 _c1;
  int64 _c2;
  value _vresult;
  value _vres[3] = { 0, 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  n = &_c1;
  d = &_c2;
  _res = Z3_get_numeral_ast_value_small(c, a, n, d);
  Begin_roots_block(_vres, 3)
    _vres[0] = Val_int(_res);
    _vres[1] = copy_int64(*n);
    _vres[2] = copy_int64(*d);
    _vresult = camlidl_alloc_small(3, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_get_index_value(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_index_value(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_is_quantifier_forall(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_is_quantifier_forall(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_weight(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_quantifier_weight(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_num_patterns(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_quantifier_num_patterns(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_pattern_ast(
	value _v_c,
	value _v_a,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int i; /*in*/
  Z3_pattern_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_quantifier_pattern_ast(c, a, i);
  _vres = camlidl_c2ml_z3_Z3_pattern_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_bound_name(
	value _v_c,
	value _v_a,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int i; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_quantifier_bound_name(c, a, i);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_bound_type_ast(
	value _v_c,
	value _v_a,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int i; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_quantifier_bound_type_ast(c, a, i);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_body(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_quantifier_body(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_quantifier_num_bound(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_get_quantifier_num_bound(c, a);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_pattern_num_terms(
	value _v_c,
	value _v_p)
{
  Z3_context c; /*in*/
  Z3_pattern_ast p; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_pattern_ast(_v_p, &p, _ctx);
  _res = Z3_get_pattern_num_terms(c, p);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_pattern_ast(
	value _v_c,
	value _v_p,
	value _v_idx)
{
  Z3_context c; /*in*/
  Z3_pattern_ast p; /*in*/
  unsigned int idx; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_pattern_ast(_v_p, &p, _ctx);
  idx = Int_val(_v_idx);
  _res = Z3_get_pattern_ast(c, p, idx);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_type_ast_to_ast(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_type_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_type_ast(_v_a, &a, _ctx);
  _res = Z3_type_ast_to_ast(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_const_ast_to_ast(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_const_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_ast(_v_a, &a, _ctx);
  _res = Z3_const_ast_to_ast(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_const_decl_ast_to_ast(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_const_decl_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_a, &a, _ctx);
  _res = Z3_const_decl_ast_to_ast(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_pattern_ast_to_ast(
	value _v_c,
	value _v_p)
{
  Z3_context c; /*in*/
  Z3_pattern_ast p; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_pattern_ast(_v_p, &p, _ctx);
  _res = Z3_pattern_ast_to_ast(c, p);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_to_const_ast(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_const_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_to_const_ast(c, a);
  _vres = camlidl_c2ml_z3_Z3_const_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_to_numeral_ast(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_numeral_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_to_numeral_ast(c, a);
  _vres = camlidl_c2ml_z3_Z3_numeral_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_push(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_push(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_pop(
	value _v_c,
	value _v_num_scopes)
{
  Z3_context c; /*in*/
  unsigned int num_scopes; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  num_scopes = Int_val(_v_num_scopes);
  Z3_pop(c, num_scopes);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_assert_cnstr(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  Z3_assert_cnstr(c, a);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_check_and_get_model(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_model *m; /*out*/
  Z3_lbool _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  Z3_model _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  m = &_c1;
  _res = Z3_check_and_get_model(c, m);
  Begin_roots_block(_vres, 2)
    _vres[0] = camlidl_c2ml_z3_Z3_lbool(&_res, _ctx);
    _vres[1] = camlidl_c2ml_z3_Z3_model(&*m, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_check(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_lbool _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_check(c);
  _vres = camlidl_c2ml_z3_Z3_lbool(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_del_model(
	value _v_m)
{
  Z3_model m; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  Z3_del_model(m);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_simplify(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_simplify(c, a);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_relevant_labels(
	value _v_c)
{
  Z3_context c; /*in*/
  Z3_labels _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_get_relevant_labels(c);
  _vres = camlidl_c2ml_z3_Z3_labels(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_del_labels(
	value _v_c,
	value _v_lbls)
{
  Z3_context c; /*in*/
  Z3_labels lbls; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_labels(_v_lbls, &lbls, _ctx);
  Z3_del_labels(c, lbls);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_get_num_labels(
	value _v_c,
	value _v_lbls)
{
  Z3_context c; /*in*/
  Z3_labels lbls; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_labels(_v_lbls, &lbls, _ctx);
  _res = Z3_get_num_labels(c, lbls);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_label_symbol(
	value _v_c,
	value _v_lbls,
	value _v_idx)
{
  Z3_context c; /*in*/
  Z3_labels lbls; /*in*/
  unsigned int idx; /*in*/
  Z3_symbol _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_labels(_v_lbls, &lbls, _ctx);
  idx = Int_val(_v_idx);
  _res = Z3_get_label_symbol(c, lbls, idx);
  _vres = camlidl_c2ml_z3_Z3_symbol(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_disable_label(
	value _v_c,
	value _v_lbls,
	value _v_idx)
{
  Z3_context c; /*in*/
  Z3_labels lbls; /*in*/
  unsigned int idx; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_labels(_v_lbls, &lbls, _ctx);
  idx = Int_val(_v_idx);
  Z3_disable_label(c, lbls, idx);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_block_labels(
	value _v_c,
	value _v_lbls)
{
  Z3_context c; /*in*/
  Z3_labels lbls; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_labels(_v_lbls, &lbls, _ctx);
  Z3_block_labels(c, lbls);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_get_model_num_constants(
	value _v_c,
	value _v_m)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  _res = Z3_get_model_num_constants(c, m);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_constant(
	value _v_c,
	value _v_m,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_model_constant(c, m, i);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_value(
	value _v_c,
	value _v_m,
	value _v_decl)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  Z3_const_decl_ast decl; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  camlidl_ml2c_z3_Z3_const_decl_ast(_v_decl, &decl, _ctx);
  _res = Z3_get_value(c, m, decl);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_value_type(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  Z3_type_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_value_type(c, v);
  _vres = camlidl_c2ml_z3_Z3_type_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_value_kind(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  Z3_value_kind _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_value_kind(c, v);
  _vres = camlidl_c2ml_z3_Z3_value_kind(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_numeral_value_string(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_numeral_value_string(c, v);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_numeral_value_int(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  int *i; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  int _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  i = &_c1;
  _res = Z3_get_numeral_value_int(c, v, i);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = Val_int(*i);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_get_numeral_value_uint(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int *u; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  unsigned int _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  u = &_c1;
  _res = Z3_get_numeral_value_uint(c, v, u);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = Val_int(*u);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_get_bool_value_bool(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_bool_value_bool(c, v);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_value_mk_decl(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_tuple_value_mk_decl(c, v);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_value_num_fields(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_tuple_value_num_fields(c, v);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_tuple_value_field(
	value _v_c,
	value _v_v,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int i; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_tuple_value_field(c, v, i);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_value_size(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_array_value_size(c, v);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_value_else(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_get_array_value_else(c, v);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_value_entry_index(
	value _v_c,
	value _v_v,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int i; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_array_value_entry_index(c, v, i);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_array_value_entry_value(
	value _v_c,
	value _v_v,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  unsigned int i; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_array_value_entry_value(c, v, i);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_num_funcs(
	value _v_c,
	value _v_m)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  _res = Z3_get_model_num_funcs(c, m);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_is_model_func_internal(
	value _v_c,
	value _v_m,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  _res = Z3_is_model_func_internal(c, m, i);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_decl(
	value _v_c,
	value _v_m,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_model_func_decl(c, m, i);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_else(
	value _v_c,
	value _v_m,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_model_func_else(c, m, i);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_num_entries(
	value _v_c,
	value _v_m,
	value _v_i)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_model_func_num_entries(c, m, i);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_entry_num_args(
	value _v_c,
	value _v_m,
	value _v_i,
	value _v_j)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  unsigned int j; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  j = Int_val(_v_j);
  _res = Z3_get_model_func_entry_num_args(c, m, i, j);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_entry_arg(
	value _v_c,
	value _v_m,
	value _v_i,
	value _v_j,
	value _v_k)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  unsigned int j; /*in*/
  unsigned int k; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  j = Int_val(_v_j);
  k = Int_val(_v_k);
  _res = Z3_get_model_func_entry_arg(c, m, i, j, k);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_model_func_entry_value(
	value _v_c,
	value _v_m,
	value _v_i,
	value _v_j)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  unsigned int i; /*in*/
  unsigned int j; /*in*/
  Z3_value _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  i = Int_val(_v_i);
  j = Int_val(_v_j);
  _res = Z3_get_model_func_entry_value(c, m, i, j);
  _vres = camlidl_c2ml_z3_Z3_value(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_eval(
	value _v_c,
	value _v_m,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  Z3_ast t; /*in*/
  Z3_value *v; /*out*/
  int _res;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  Z3_value _c1;
  value _vresult;
  value _vres[2] = { 0, 0, };

  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t, &t, _ctx);
  v = &_c1;
  _res = Z3_eval(c, m, t, v);
  Begin_roots_block(_vres, 2)
    _vres[0] = Val_int(_res);
    _vres[1] = camlidl_c2ml_z3_Z3_value(&*v, _ctx);
    _vresult = camlidl_alloc_small(2, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
  End_roots()
  camlidl_free(_ctx);
  return _vresult;
}

value camlidl_z3_Z3_set_soft_timeout(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  unsigned int t; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  t = Int_val(_v_t);
  Z3_set_soft_timeout(c, t);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_reset_soft_timeout(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_reset_soft_timeout(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_open_log(
	value _v_c,
	value _v_filename)
{
  Z3_context c; /*in*/
  char const *filename; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  filename = String_val(_v_filename);
  _res = Z3_open_log(c, filename);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_close_log(
	value _v_c)
{
  Z3_context c; /*in*/
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  Z3_close_log(c);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_ast_to_string(
	value _v_c,
	value _v_a)
{
  Z3_context c; /*in*/
  Z3_ast a; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_a, &a, _ctx);
  _res = Z3_ast_to_string(c, a);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_model_to_string(
	value _v_c,
	value _v_m)
{
  Z3_context c; /*in*/
  Z3_model m; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_model(_v_m, &m, _ctx);
  _res = Z3_model_to_string(c, m);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_value_to_string(
	value _v_c,
	value _v_v)
{
  Z3_context c; /*in*/
  Z3_value v; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_value(_v_v, &v, _ctx);
  _res = Z3_value_to_string(c, v);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_context_to_string(
	value _v_c)
{
  Z3_context c; /*in*/
  char const *_res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_context_to_string(c);
  _vres = copy_string(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_parse_smtlib_string(
	value _v_c,
	value _v_str,
	value _v_type_names,
	value _v_types,
	value _v_decl_names,
	value _v_decls)
{
  Z3_context c; /*in*/
  char const *str; /*in*/
  unsigned int num_types; /*in*/
  Z3_symbol *type_names; /*in*/
  Z3_type_ast *types; /*in*/
  unsigned int num_decls; /*in*/
  Z3_symbol *decl_names; /*in*/
  Z3_const_decl_ast *decls; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  mlsize_t _c10;
  mlsize_t _c11;
  value _v12;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  str = String_val(_v_str);
  _c1 = Wosize_val(_v_type_names);
  type_names = camlidl_malloc(_c1 * sizeof(Z3_symbol ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_type_names, _c2);
    camlidl_ml2c_z3_Z3_symbol(_v3, &type_names[_c2], _ctx);
  }
  num_types = _c1;
  _c4 = Wosize_val(_v_types);
  types = camlidl_malloc(_c4 * sizeof(Z3_type_ast ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_types, _c5);
    camlidl_ml2c_z3_Z3_type_ast(_v6, &types[_c5], _ctx);
  }
  num_types = _c4;
  _c7 = Wosize_val(_v_decl_names);
  decl_names = camlidl_malloc(_c7 * sizeof(Z3_symbol ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_decl_names, _c8);
    camlidl_ml2c_z3_Z3_symbol(_v9, &decl_names[_c8], _ctx);
  }
  num_decls = _c7;
  _c10 = Wosize_val(_v_decls);
  decls = camlidl_malloc(_c10 * sizeof(Z3_const_decl_ast ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    _v12 = Field(_v_decls, _c11);
    camlidl_ml2c_z3_Z3_const_decl_ast(_v12, &decls[_c11], _ctx);
  }
  num_decls = _c10;
  Z3_parse_smtlib_string(c, str, num_types, type_names, types, num_decls, decl_names, decls);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_parse_smtlib_string_bytecode(value * argv, int argn)
{
  return camlidl_z3_Z3_parse_smtlib_string(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_z3_Z3_parse_smtlib_file(
	value _v_c,
	value _v_file_name,
	value _v_type_names,
	value _v_types,
	value _v_decl_names,
	value _v_decls)
{
  Z3_context c; /*in*/
  char const *file_name; /*in*/
  unsigned int num_types; /*in*/
  Z3_symbol *type_names; /*in*/
  Z3_type_ast *types; /*in*/
  unsigned int num_decls; /*in*/
  Z3_symbol *decl_names; /*in*/
  Z3_const_decl_ast *decls; /*in*/
  mlsize_t _c1;
  mlsize_t _c2;
  value _v3;
  mlsize_t _c4;
  mlsize_t _c5;
  value _v6;
  mlsize_t _c7;
  mlsize_t _c8;
  value _v9;
  mlsize_t _c10;
  mlsize_t _c11;
  value _v12;
  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  file_name = String_val(_v_file_name);
  _c1 = Wosize_val(_v_type_names);
  type_names = camlidl_malloc(_c1 * sizeof(Z3_symbol ), _ctx);
  for (_c2 = 0; _c2 < _c1; _c2++) {
    _v3 = Field(_v_type_names, _c2);
    camlidl_ml2c_z3_Z3_symbol(_v3, &type_names[_c2], _ctx);
  }
  num_types = _c1;
  _c4 = Wosize_val(_v_types);
  types = camlidl_malloc(_c4 * sizeof(Z3_type_ast ), _ctx);
  for (_c5 = 0; _c5 < _c4; _c5++) {
    _v6 = Field(_v_types, _c5);
    camlidl_ml2c_z3_Z3_type_ast(_v6, &types[_c5], _ctx);
  }
  num_types = _c4;
  _c7 = Wosize_val(_v_decl_names);
  decl_names = camlidl_malloc(_c7 * sizeof(Z3_symbol ), _ctx);
  for (_c8 = 0; _c8 < _c7; _c8++) {
    _v9 = Field(_v_decl_names, _c8);
    camlidl_ml2c_z3_Z3_symbol(_v9, &decl_names[_c8], _ctx);
  }
  num_decls = _c7;
  _c10 = Wosize_val(_v_decls);
  decls = camlidl_malloc(_c10 * sizeof(Z3_const_decl_ast ), _ctx);
  for (_c11 = 0; _c11 < _c10; _c11++) {
    _v12 = Field(_v_decls, _c11);
    camlidl_ml2c_z3_Z3_const_decl_ast(_v12, &decls[_c11], _ctx);
  }
  num_decls = _c10;
  Z3_parse_smtlib_file(c, file_name, num_types, type_names, types, num_decls, decl_names, decls);
  camlidl_free(_ctx);
  return Val_unit;
}

value camlidl_z3_Z3_parse_smtlib_file_bytecode(value * argv, int argn)
{
  return camlidl_z3_Z3_parse_smtlib_file(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

value camlidl_z3_Z3_get_smtlib_num_formulas(
	value _v_c)
{
  Z3_context c; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_get_smtlib_num_formulas(c);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_smtlib_formula(
	value _v_c,
	value _v_i)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_smtlib_formula(c, i);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_smtlib_num_assumptions(
	value _v_c)
{
  Z3_context c; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_get_smtlib_num_assumptions(c);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_smtlib_assumption(
	value _v_c,
	value _v_i)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_smtlib_assumption(c, i);
  _vres = camlidl_c2ml_z3_Z3_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_smtlib_num_decls(
	value _v_c)
{
  Z3_context c; /*in*/
  unsigned int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  _res = Z3_get_smtlib_num_decls(c);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_smtlib_decl(
	value _v_c,
	value _v_i)
{
  Z3_context c; /*in*/
  unsigned int i; /*in*/
  Z3_const_decl_ast _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  i = Int_val(_v_i);
  _res = Z3_get_smtlib_decl(c, i);
  _vres = camlidl_c2ml_z3_Z3_const_decl_ast(&_res, _ctx);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_version(value _unit)
{
  unsigned int *major; /*out*/
  unsigned int *minor; /*out*/
  unsigned int *build_number; /*out*/
  unsigned int *revision_number; /*out*/
  unsigned int _c1;
  unsigned int _c2;
  unsigned int _c3;
  unsigned int _c4;
  value _vresult;
  value _vres[4] = { 0, 0, 0, 0, };

  major = &_c1;
  minor = &_c2;
  build_number = &_c3;
  revision_number = &_c4;
  Z3_get_version(major, minor, build_number, revision_number);
  Begin_roots_block(_vres, 4)
    _vres[0] = Val_int(*major);
    _vres[1] = Val_int(*minor);
    _vres[2] = Val_int(*build_number);
    _vres[3] = Val_int(*revision_number);
    _vresult = camlidl_alloc_small(4, 0);
    Field(_vresult, 0) = _vres[0];
    Field(_vresult, 1) = _vres[1];
    Field(_vresult, 2) = _vres[2];
    Field(_vresult, 3) = _vres[3];
  End_roots()
  return _vresult;
}

value camlidl_z3_Z3_type_check(
	value _v_c,
	value _v_t)
{
  Z3_context c; /*in*/
  Z3_ast t; /*in*/
  int _res;
  value _vres;

  struct camlidl_ctx_struct _ctxs = { CAMLIDL_TRANSIENT, NULL };
  camlidl_ctx _ctx = &_ctxs;
  camlidl_ml2c_z3_Z3_context(_v_c, &c, _ctx);
  camlidl_ml2c_z3_Z3_ast(_v_t, &t, _ctx);
  _res = Z3_type_check(c, t);
  _vres = Val_int(_res);
  camlidl_free(_ctx);
  return _vres;
}

value camlidl_z3_Z3_get_allocation_size(value _unit)
{
  unsigned int _res;
  value _vres;

  _res = Z3_get_allocation_size();
  _vres = Val_int(_res);
  return _vres;
}

