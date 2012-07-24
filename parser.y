%{
/****************************************************************************\
Copyright (c) 2002, NVIDIA Corporation.

NVIDIA Corporation("NVIDIA") supplies this software to you in
consideration of your agreement to the following terms, and your use,
installation, modification or redistribution of this NVIDIA software
constitutes acceptance of these terms.  If you do not agree with these
terms, please do not use, install, modify or redistribute this NVIDIA
software.

In consideration of your agreement to abide by the following terms, and
subject to these terms, NVIDIA grants you a personal, non-exclusive
license, under NVIDIA's copyrights in this original NVIDIA software (the
"NVIDIA Software"), to use, reproduce, modify and redistribute the
NVIDIA Software, with or without modifications, in source and/or binary
forms; provided that if you redistribute the NVIDIA Software, you must
retain the copyright notice of NVIDIA, this notice and the following
text and disclaimers in all such redistributions of the NVIDIA Software.
Neither the name, trademarks, service marks nor logos of NVIDIA
Corporation may be used to endorse or promote products derived from the
NVIDIA Software without specific prior written permission from NVIDIA.
Except as expressly stated in this notice, no other rights or licenses
express or implied, are granted by NVIDIA herein, including but not
limited to any patent rights that may be infringed by your derivative
works or by other works in which the NVIDIA Software may be
incorporated. No hardware is licensed hereunder. 

THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE,
NON-INFRINGEMENT, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
ITS USE AND OPERATION EITHER ALONE OR IN COMBINATION WITH OTHER
PRODUCTS.

IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT,
INCIDENTAL, EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, LOST PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY
OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE
NVIDIA SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT,
TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF
NVIDIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
\****************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#define NO_PARSER 1
#include "slglobals.h"

%}

/* Grammar semantic type: */

%union {
    int    sc_token;
    int    sc_int;
    float  sc_fval;
    int    sc_ident;
    spec   sc_specifiers;
    dtype  sc_type;
    Type   *sc_ptype;
    decl   *sc_decl;
    expr   *sc_expr;
    symb   *sc_symb;
    stmt   *sc_stmt;
    /* Dummy palce holder: */
    int    dummy;
}

%token <sc_token> AND_SY
%token <sc_token> ASM_SY
%token <sc_token> ASSIGNMINUS_SY
%token <sc_token> ASSIGNMOD_SY
%token <sc_token> ASSIGNPLUS_SY
%token <sc_token> ASSIGNSLASH_SY
%token <sc_token> ASSIGNSTAR_SY
%token <sc_token> BOOLEAN_SY
%token <sc_token> BREAK_SY
%token <sc_token> CASE_SY
%token <sc_fval>  CFLOATCONST_SY
%token <sc_token> COLONCOLON_SY
%token <sc_token> CONST_SY
%token <sc_token> CONTINUE_SY
%token <sc_token> DEFAULT_SY
%token <sc_token> DISCARD_SY
%token <sc_token> DO_SY
%token <sc_token> EQ_SY
%token <sc_token> ELSE_SY
%token <sc_token> ERROR_SY
%token <sc_token> EXTERN_SY
%token <sc_token> FLOAT_SY
%token <sc_fval>  FLOATCONST_SY
%token <sc_fval>  FLOATHCONST_SY
%token <sc_fval>  FLOATXCONST_SY
%token <sc_token> FOR_SY
%token <sc_token> GE_SY
%token <sc_token> GG_SY
%token <sc_token> GOTO_SY
%token <sc_ident> IDENT_SY
%token <sc_token> IF_SY
%token <sc_token> IN_SY
%token <sc_token> INLINE_SY
%token <sc_token> INOUT_SY
%token <sc_token> INT_SY
%token <sc_int>   INTCONST_SY
%token <sc_token> INTERNAL_SY
%token <sc_token> LE_SY
%token <sc_token> LL_SY
%token <sc_token> MINUSMINUS_SY
%token <sc_token> NE_SY
%token <sc_token> OR_SY
%token <sc_token> OUT_SY
%token <sc_token> PACKED_SY
%token <sc_token> PLUSPLUS_SY
%token <sc_token> RETURN_SY
%token <sc_token> STATIC_SY
%token <sc_token> STRCONST_SY
%token <sc_token> STRUCT_SY
%token <sc_token> SWITCH_SY
%token <sc_token> TEXOBJ_SY
%token <sc_token> THIS_SY
%token <sc_token> TYPEDEF_SY
%token <sc_ident> TYPEIDENT_SY
%token <sc_token> UNIFORM_SY
%token <sc_token> VARYING_SY
%token <sc_token> VOID_SY
%token <sc_token> WHILE_SY

%token <sc_token> FIRST_USER_TOKEN_SY  /* Must be last token declaration */

/*************<<<<<<<<<<<<<<<<<<<********************
%type <dummy> abstract_parameter_declaration
**************>>>>>>>>>>>>>>>>>***********************/
%type <dummy> compilation_unit
%type <dummy> compound_header
%type <dummy> compound_tail
%type <dummy> external_declaration
%type <dummy> function_definition
%type <dummy> struct_compound_header

%type <sc_int> function_specifier
%type <sc_int> in_out
/***
%type <sc_int> integer_constant
***/
%type <sc_int> type_domain
%type <sc_int> type_qualifier
%type <sc_int> storage_class

%type <sc_ident> identifier
%type <sc_ident> member_identifier
%type <sc_ident> scope_identifier
%type <sc_ident> semantics_identifier
%type <sc_ident> struct_identifier
%type <sc_ident> type_identifier
%type <sc_ident> variable_identifier

%type <sc_decl> abstract_declaration
%type <sc_decl> abstract_declarator
%type <sc_decl> abstract_parameter_list
%type <sc_decl> declarator
%type <sc_decl> basic_declarator
%type <sc_decl> semantic_declarator
%type <sc_decl> function_decl_header
%type <sc_decl> function_definition_header
%type <sc_decl> non_empty_abstract_parameter_list
%type <sc_decl> parameter_declaration
%type <sc_decl> parameter_list

%type <sc_type> abstract_declaration_specifiers
%type <sc_type> abstract_declaration_specifiers2
%type <sc_type> declaration_specifiers
%type <sc_ptype> struct_or_connector_header
%type <sc_ptype> struct_or_connector_specifier
/***
%type <sc_type> type_name
***/
%type <sc_ptype> type_specifier
%type <sc_ptype> untagged_struct_header

%type <sc_expr> actual_argument_list
%type <sc_expr> additive_expression
%type <sc_expr> AND_expression
%type <sc_expr> basic_variable
%type <sc_expr> boolean_expression_opt
%type <sc_expr> boolean_scalar_expression
%type <sc_expr> cast_expression
%type <sc_expr> conditional_expression
%type <sc_expr> constant
/***
%type <sc_expr> constant_expression
***/
%type <sc_expr> conditional_test
%type <sc_expr> equality_expression
%type <sc_expr> exclusive_OR_expression
%type <sc_expr> expression
%type <sc_expr> expression_list
%type <sc_expr> inclusive_OR_expression
%type <sc_expr> initializer
%type <sc_expr> initializer_list
%type <sc_expr> logical_AND_expression
%type <sc_expr> logical_OR_expression
%type <sc_expr> multiplicative_expression
%type <sc_expr> non_empty_argument_list
%type <sc_expr> postfix_expression
%type <sc_expr> primary_expression
%type <sc_expr> relational_expression
%type <sc_expr> shift_expression
%type <sc_expr> unary_expression
%type <sc_expr> variable

%type <sc_stmt> annotation
%type <sc_stmt> annotation_decl_list
%type <sc_stmt> balanced_statement
%type <sc_stmt> block_item
%type <sc_stmt> block_item_list
%type <sc_stmt> compound_statement
%type <sc_stmt> dangling_if
%type <sc_stmt> dangling_iteration
%type <sc_stmt> dangling_statement
%type <sc_stmt> declaration
%type <sc_stmt> discard_statement
%type <sc_stmt> expression_statement
%type <sc_stmt> expression_statement2
%type <sc_stmt> for_expression
%type <sc_stmt> for_expression_opt
%type <sc_stmt> if_header
%type <sc_stmt> if_statement
%type <sc_stmt> init_declarator
%type <sc_stmt> init_declarator_list
%type <sc_stmt> iteration_statement
%type <sc_stmt> return_statement
%type <sc_stmt> statement
%type <sc_stmt> struct_declaration
%type <sc_stmt> struct_declaration_list

/* Operator precedence rules: */

/* Don't even THINK about it! */

%%

compilation_unit:         external_declaration
                        | compilation_unit external_declaration
;

/****************/
/* Declarations */
/****************/

external_declaration:     declaration
                              { $$ = GlobalInitStatements(CurrentScope, $1); }
                        | function_definition
;

declaration:              declaration_specifiers ';'
                              { $$ = NULL; }
                        | declaration_specifiers init_declarator_list ';'
                              { $$ = $2; }
                        | ERROR_SY ';'
                              { RecordErrorPos(Cg->tokenLoc); $$ = NULL; }
;

abstract_declaration:     abstract_declaration_specifiers abstract_declarator
                              { $$ = $2; }
/***
                        | abstract_declarator
***/
;

declaration_specifiers:   abstract_declaration_specifiers
                              { $$ = $1; }
                        | TYPEDEF_SY abstract_declaration_specifiers
                              { SetTypeMisc(Cg->tokenLoc, &CurrentDeclTypeSpecs, TYPE_MISC_TYPEDEF); $$ = $2; }
;

abstract_declaration_specifiers:
                          abstract_declaration_specifiers2
                              { $$ = $1; }
                        | type_qualifier abstract_declaration_specifiers
                              { SetTypeQualifiers(Cg->tokenLoc, &CurrentDeclTypeSpecs, $1); $$ = CurrentDeclTypeSpecs; }
                        | storage_class abstract_declaration_specifiers
                              { SetStorageClass(Cg->tokenLoc, &CurrentDeclTypeSpecs, $1); $$ = CurrentDeclTypeSpecs; }
                        | type_domain abstract_declaration_specifiers
                              { SetTypeDomain(Cg->tokenLoc, &CurrentDeclTypeSpecs, $1); $$ = CurrentDeclTypeSpecs; }
                        | in_out abstract_declaration_specifiers
                              { SetTypeQualifiers(Cg->tokenLoc, &CurrentDeclTypeSpecs, $1); $$ = CurrentDeclTypeSpecs; }
                        | function_specifier abstract_declaration_specifiers
                              { SetTypeMisc(Cg->tokenLoc, &CurrentDeclTypeSpecs, $1); $$ = CurrentDeclTypeSpecs; }
                        | PACKED_SY abstract_declaration_specifiers
                              { SetTypePacked(Cg->tokenLoc, &CurrentDeclTypeSpecs); $$ = CurrentDeclTypeSpecs; }
;

abstract_declaration_specifiers2:
                          type_specifier
                              { $$ = *SetDType(&CurrentDeclTypeSpecs, $1); }
                        | abstract_declaration_specifiers2 type_qualifier
                              { SetTypeQualifiers(Cg->tokenLoc, &CurrentDeclTypeSpecs, $2); $$ = CurrentDeclTypeSpecs; }
                        | abstract_declaration_specifiers2 storage_class
                              { SetStorageClass(Cg->tokenLoc, &CurrentDeclTypeSpecs, $2); $$ = CurrentDeclTypeSpecs; }
                        | abstract_declaration_specifiers2 type_domain
                              { SetTypeDomain(Cg->tokenLoc, &CurrentDeclTypeSpecs, $2); $$ = CurrentDeclTypeSpecs; }
                        | abstract_declaration_specifiers2 in_out
                              { SetTypeQualifiers(Cg->tokenLoc, &CurrentDeclTypeSpecs, $2); $$ = CurrentDeclTypeSpecs; }
                        | abstract_declaration_specifiers2 function_specifier
                              { SetTypeMisc(Cg->tokenLoc, &CurrentDeclTypeSpecs, $2); $$ = CurrentDeclTypeSpecs; }
                        | abstract_declaration_specifiers2 PACKED_SY
                              { SetTypePacked(Cg->tokenLoc, &CurrentDeclTypeSpecs); $$ = CurrentDeclTypeSpecs; }
;

init_declarator_list:     init_declarator
                              { $$ = $1; }
                        | init_declarator_list ',' init_declarator
                              { $$ = AddStmt($1, $3); }
;

init_declarator:          declarator
                              { $$ = Init_Declarator(Cg->tokenLoc, CurrentScope, $1, NULL); }
                        | declarator '=' initializer
                              { $$ = Init_Declarator(Cg->tokenLoc, CurrentScope, $1, $3); }
;

/*******************/
/* Type Specifiers */
/*******************/

type_specifier:           INT_SY
                              { $$ = LookUpTypeSymbol(NULL, INT_SY); }
                        | FLOAT_SY
                              { $$ = LookUpTypeSymbol(NULL, FLOAT_SY); }
                        | VOID_SY
                              { $$ = LookUpTypeSymbol(NULL, VOID_SY); }
                        | BOOLEAN_SY
                              { $$ = LookUpTypeSymbol(NULL, BOOLEAN_SY); }
                        | TEXOBJ_SY
                              { $$ = LookUpTypeSymbol(NULL, TEXOBJ_SY); }
                        | struct_or_connector_specifier
                              { $$ = $1; }
                        | type_identifier
                              { $$ = LookUpTypeSymbol(NULL, $1); }
                        | error
                              { 
                                SemanticParseError(Cg->tokenLoc, ERROR_S_TYPE_NAME_EXPECTED,
                                                   GetAtomString(atable, Cg->mostRecentToken /* yychar */));
                                $$ = UndefinedType;
                              }
;

/*******************/
/* Type Qualifiers */
/*******************/

type_qualifier:           CONST_SY
                              { $$ = TYPE_QUALIFIER_CONST; }
;

/****************/
/* Type Domains */
/****************/

type_domain:              UNIFORM_SY
                              { $$ = TYPE_DOMAIN_UNIFORM; }
;

/*******************/
/* Storage Classes */
/*******************/

storage_class:            STATIC_SY
                              { $$ = (int) SC_STATIC; }
                        | EXTERN_SY
                              { $$ = (int) SC_EXTERN; }
;

/**********************/
/* Function Specifier */
/**********************/

function_specifier:       INLINE_SY
                              { $$ = TYPE_MISC_INLINE; }
                        | INTERNAL_SY
                              { $$ = TYPE_MISC_INTERNAL; }
;

/**********/
/* In Out */
/**********/

in_out:                   IN_SY
                              { $$ = TYPE_QUALIFIER_IN; }
                        | OUT_SY
                              { $$ = TYPE_QUALIFIER_OUT; }
                        | INOUT_SY
                              { $$ = TYPE_QUALIFIER_INOUT; }
;

/****************/
/* Struct Types */
/****************/

struct_or_connector_specifier:
                          struct_or_connector_header struct_compound_header struct_declaration_list '}'
                              { $$ = SetStructMembers(Cg->tokenLoc, $1, PopScope()); }
                        | untagged_struct_header struct_compound_header struct_declaration_list '}'
                              { $$ = SetStructMembers(Cg->tokenLoc, $1, PopScope()); }
                        | struct_or_connector_header
                              { $$ = $1; }
;

struct_compound_header:   compound_header
                              { CurrentScope->IsStructScope = 1; $$ = $1; }
;

struct_or_connector_header:
                          STRUCT_SY struct_identifier
                              { $$ = StructHeader(Cg->tokenLoc, CurrentScope, 0, $2); }
                        | STRUCT_SY struct_identifier ':' semantics_identifier
                              { $$ = StructHeader(Cg->tokenLoc, CurrentScope, $4, $2); }
;

struct_identifier:        identifier
                        | type_identifier
;

untagged_struct_header:   STRUCT_SY
                              { $$ = StructHeader(Cg->tokenLoc, CurrentScope, 0, 0); }
;

struct_declaration_list:  struct_declaration
                        | struct_declaration_list struct_declaration
;

struct_declaration:       declaration
                            { $$ = $1; }
;

/**************/
/* Type Names */
/**************/

/*** Not used -- Use abstract_declaration" instead ***
type_name:                type_specifier
                        | type_qualifier type_name
                              { $$ = $2; }
;
***/

/***************/
/* Annotations */
/***************/

annotation:               '<' { PushScope(NewScope()); } annotation_decl_list '>'
                              { $$ = $3; PopScope(); }
;

annotation_decl_list:     /* empty */
                              { $$ = 0; }
                        | annotation_decl_list declaration
;

/***************/
/* Declarators */
/***************/

declarator:               semantic_declarator
                              { $$ = $1; }
                        | semantic_declarator annotation
                              { $$ = $1; }
;

semantic_declarator:      basic_declarator
                              { $$ = Declarator(Cg->tokenLoc, $1, 0); }
                        | basic_declarator ':' semantics_identifier
                              { $$ = Declarator(Cg->tokenLoc, $1, $3); }
;

basic_declarator:         identifier
                              { $$ = NewDeclNode(Cg->tokenLoc, $1, &CurrentDeclTypeSpecs); }
                        | basic_declarator '[' INTCONST_SY /* constant_expression */ ']'
                              { $$ = Array_Declarator(Cg->tokenLoc, $1, $3, 0); }
                        | basic_declarator '[' ']'
                              { $$ = Array_Declarator(Cg->tokenLoc, $1, 0 , 1); }
                        | function_decl_header parameter_list ')'
                              { $$ = SetFunTypeParams(CurrentScope, $1, $2, $2); }
                        | function_decl_header abstract_parameter_list ')'
                              { $$ = SetFunTypeParams(CurrentScope, $1, $2, NULL); }
;

function_decl_header:     basic_declarator '('
                              { $$ = FunctionDeclHeader(&$1->loc, CurrentScope, $1); }
;

abstract_declarator:      /* empty */
                              { $$ = NewDeclNode(Cg->tokenLoc, 0, &CurrentDeclTypeSpecs); }
                        | abstract_declarator '[' INTCONST_SY /* constant_expression */  ']'
                              { $$ = Array_Declarator(Cg->tokenLoc, $1, $3, 0); }
                        | abstract_declarator '[' ']'
                              { $$ = Array_Declarator(Cg->tokenLoc, $1, 0 , 1); }
/***
 *** This rule causes a major shift reduce conflict with:
 ***
 ***      primary_expression  :;=  type_specifier '(' expression_list ')'
 ***
 *** Cannot be easily factored.  Would force: "( expr -list )" to be merged with "( abstract-param-list )"
 ***
 *** Matches other shading languages' syntax.
 *** Will disallow abstract literal function parameter declarations should we ever defide to
 ***      support function parameters in the future.
 ***
                        | abstract_declarator '(' abstract_parameter_list ')'
***/
;

parameter_list:           parameter_declaration
                              { $$ = $1; }
                        | parameter_list ',' parameter_declaration
                              { $$ = AddDecl($1, $3); }
;

parameter_declaration:    declaration_specifiers declarator
                              { $$ = Param_Init_Declarator(Cg->tokenLoc, CurrentScope, $2, NULL); }
                        | declaration_specifiers declarator '=' initializer
                              { $$ = Param_Init_Declarator(Cg->tokenLoc, CurrentScope, $2, $4); }
;

abstract_parameter_list:  /* empty */
                              { $$ = NULL; }
                        | non_empty_abstract_parameter_list
;

non_empty_abstract_parameter_list:  abstract_declaration
                              {
                                if (IsVoid(&$1->type.type))
                                    CurrentScope->HasVoidParameter = 1;
                                $$ = $1;
                              }
                        | non_empty_abstract_parameter_list ',' abstract_declaration
                              {
                                if (CurrentScope->HasVoidParameter || IsVoid(&$1->type.type)) {
                                    SemanticError(Cg->tokenLoc, ERROR___VOID_NOT_ONLY_PARAM);
                                }
                                $$ = AddDecl($1, $3);
                              }
;

/******************/
/* Initialization */
/******************/

initializer:              expression
                              { $$ = Initializer(Cg->tokenLoc, $1); }
                        | '{' initializer_list '}'
                              { $$ = Initializer(Cg->tokenLoc, $2); }
                        | '{' initializer_list ',' '}'
                              { $$ = Initializer(Cg->tokenLoc, $2); }
;

initializer_list:         initializer
                              { $$ = InitializerList(Cg->tokenLoc, $1, NULL); }
                        | initializer_list ',' initializer
                              { $$ = InitializerList(Cg->tokenLoc, $1, $3); }
;

/***************/
/* EXPRESSIONS */
/***************/

/************/
/* Variable */
/************/

variable:                 basic_variable
                              { $$ = $1; }
                        | scope_identifier COLONCOLON_SY basic_variable
                              { $$ = $3; }
;

basic_variable:           variable_identifier
                              { $$ = BasicVariable(Cg->tokenLoc, $1); }
;

/**********************/
/* Primary Expression */
/**********************/

primary_expression:       variable
                        | constant
                        | '(' expression ')'
                              { $$ = $2; }
                        | type_specifier '(' expression_list ')'
                              { $$ = NewVectorConstructor(Cg->tokenLoc, $1, $3); }
;

/*********************/
/* Postfix Operators */
/*********************/

postfix_expression:       primary_expression
                        | postfix_expression PLUSPLUS_SY
                              { $$ = (expr *) NewUnopNode(POSTINC_OP, $1); }
                        | postfix_expression MINUSMINUS_SY
                              { $$ = (expr *) NewUnopNode(POSTDEC_OP, $1); }
                        | postfix_expression '.' member_identifier
                              { $$ = NewMemberSelectorOrSwizzleOrWriteMaskOperator(Cg->tokenLoc, $1, $3); }
                        | postfix_expression '[' expression ']'
                              { $$ = NewIndexOperator(Cg->tokenLoc, $1, $3); }
                        | postfix_expression '(' actual_argument_list ')'
                              { $$ = NewFunctionCallOperator(Cg->tokenLoc, $1, $3); }
;

actual_argument_list:     /* empty */
                                { $$ = NULL; }
                        | non_empty_argument_list
;

non_empty_argument_list:  expression
                              { $$ = ArgumentList(Cg->tokenLoc, NULL, $1); }
                        | non_empty_argument_list ',' expression
                              { $$ = ArgumentList(Cg->tokenLoc, $1, $3); }
;

expression_list:          expression
                              { $$ = ExpressionList(Cg->tokenLoc, NULL, $1); }
                        | expression_list ',' expression
                              { $$ = ExpressionList(Cg->tokenLoc, $1, $3); }
;

/*******************/
/* Unary Operators */
/*******************/

unary_expression:         postfix_expression
                        | PLUSPLUS_SY unary_expression
                              { $$ = (expr *) NewUnopNode(PREINC_OP, $2); }
                        | MINUSMINUS_SY unary_expression
                              { $$ = (expr *) NewUnopNode(PREDEC_OP, $2); }
                        | '+' unary_expression
                              { $$ = NewUnaryOperator(Cg->tokenLoc, POS_OP, '+', $2, 0); }
                        | '-' unary_expression
                              { $$ = NewUnaryOperator(Cg->tokenLoc, NEG_OP, '-', $2, 0); }
                        | '!' unary_expression
                              { $$ = NewUnaryOperator(Cg->tokenLoc, BNOT_OP, '!', $2, 0); }
                        | '~' unary_expression
                              { $$ = NewUnaryOperator(Cg->tokenLoc, NOT_OP, '~', $2, 1); }
;

/*****************/
/* Cast Operator */
/*****************/

cast_expression:          unary_expression
/* *** reduce/reduce conflict: (var-ident) (type-ident) ***
                        | '(' type_name ')' cast_expression
*/
                        | '(' abstract_declaration ')' cast_expression
                              { $$ = NewCastOperator(Cg->tokenLoc, $4, GetTypePointer(&$2->loc, &$2->type)); }
;

/****************************/
/* Multiplicative Operators */
/****************************/

multiplicative_expression: cast_expression
                        | multiplicative_expression '*' cast_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, MUL_OP, '*', $1, $3, 0); }
                        | multiplicative_expression '/' cast_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, DIV_OP, '/', $1, $3, 0); }
                        | multiplicative_expression '%' cast_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, MOD_OP, '%', $1, $3, 1); }
;

/**********************/
/* Addative Operators */
/**********************/

additive_expression:      multiplicative_expression
                        | additive_expression '+' multiplicative_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, ADD_OP, '+', $1, $3, 0); }
                        | additive_expression '-' multiplicative_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, SUB_OP, '-', $1, $3, 0); }
;

/***************************/
/* Bitwise Shift Operators */
/***************************/

shift_expression:         additive_expression
                        | shift_expression LL_SY additive_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, SHL_OP, LL_SY, $1, $3, 1); }
                        | shift_expression GG_SY additive_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, SHR_OP, GG_SY, $1, $3, 1); }
;

/************************/
/* Relational Operators */
/************************/

relational_expression:    shift_expression
                        | relational_expression '<' shift_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, LT_OP, '<', $1, $3); }
                        | relational_expression '>' shift_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, GT_OP, '>', $1, $3); }
                        | relational_expression LE_SY shift_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, LE_OP, LE_SY, $1, $3); }
                        | relational_expression GE_SY shift_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, GE_OP, GE_SY, $1, $3); }
;

/**********************/
/* Equality Operators */
/**********************/

equality_expression:      relational_expression
                        | equality_expression EQ_SY relational_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, EQ_OP, EQ_SY, $1, $3); }
                        | equality_expression NE_SY relational_expression
                              { $$ = NewBinaryComparisonOperator(Cg->tokenLoc, NE_OP, NE_SY, $1, $3); }
;

/************************/
/* Bitwise AND Operator */
/************************/

AND_expression:           equality_expression
                        | AND_expression '&' equality_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, AND_OP, '&', $1, $3, 1); }
;

/*********************************/
/* Bitwise Exclusive OR Operator */
/*********************************/

exclusive_OR_expression: AND_expression
                        | exclusive_OR_expression '^' AND_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, XOR_OP, '^', $1, $3, 1); }
;

/*********************************/
/* Bitwise Inclusive OR Operator */
/*********************************/

inclusive_OR_expression:  exclusive_OR_expression
                        | inclusive_OR_expression '|' exclusive_OR_expression
                              { $$ = NewBinaryOperator(Cg->tokenLoc, OR_OP, '|', $1, $3, 1); }
;

/************************/
/* Logical AND Operator */
/************************/

logical_AND_expression:   inclusive_OR_expression
                        | logical_AND_expression AND_SY inclusive_OR_expression
                              { $$ = NewBinaryBooleanOperator(Cg->tokenLoc, BAND_OP, AND_SY, $1, $3); }
;

/***********************/
/* Logical OR Operator */
/***********************/

logical_OR_expression:    logical_AND_expression
                        | logical_OR_expression OR_SY logical_AND_expression
                              { $$ = NewBinaryBooleanOperator(Cg->tokenLoc, BOR_OP, OR_SY, $1, $3); }
;

/************************/
/* Conditional Operator */
/************************/

conditional_expression:   logical_OR_expression
                        | conditional_test '?' expression ':' conditional_expression
                              { $$ = NewConditionalOperator(Cg->tokenLoc, $1, $3, $5); }
;

conditional_test:         logical_OR_expression
                              {  $$ = CheckBooleanExpr(Cg->tokenLoc, $1, 1); }
;

/***********************/
/* Assignment operator */
/***********************/

expression:               conditional_expression
/***
                        | basic_variable '=' expression
                              { $$ = (expr *) NewBinopNode(ASSIGN_OP, $1, $3); }
***/
;

/***********************/
/* Function Definition */
/***********************/

function_definition:      function_definition_header block_item_list '}'
                              { DefineFunction(Cg->tokenLoc, CurrentScope, $1, $2); PopScope(); }
                        | function_definition_header '}'
                              { DefineFunction(Cg->tokenLoc, CurrentScope, $1, NULL); PopScope(); }
;

function_definition_header: declaration_specifiers declarator '{'
                              { $$ = Function_Definition_Header(Cg->tokenLoc, $2); }
;

/*************/
/* Statement */
/*************/

statement:                balanced_statement
                        | dangling_statement
;

balanced_statement:       compound_statement
                        | discard_statement
                        | expression_statement
                        | iteration_statement
                        | if_statement
                        | return_statement
;

dangling_statement:       dangling_if
                        | dangling_iteration
;

/*********************/
/* Default Statement */
/*********************/

discard_statement:        DISCARD_SY ';'
                              { $$ = (stmt *) NewDiscardStmt(Cg->tokenLoc, NULL); }
                        | DISCARD_SY expression ';'
                              { $$ = (stmt *) NewDiscardStmt(Cg->tokenLoc, CheckBooleanExpr(Cg->tokenLoc, $2, 1)); }
;

/****************/
/* If Statement */
/****************/

if_statement:             if_header balanced_statement ELSE_SY balanced_statement
                              { $$ = (stmt *) SetThenElseStmts(Cg->tokenLoc, $1, $2, $4); }
;

dangling_if:              if_header statement
                              { $$ = (stmt *) SetThenElseStmts(Cg->tokenLoc, $1, $2, NULL); }
                        | if_header balanced_statement ELSE_SY dangling_statement
                              { $$ = (stmt *) SetThenElseStmts(Cg->tokenLoc, $1, $2, $4); }
;

if_header:                IF_SY '(' boolean_scalar_expression ')'
                              { $$ = (stmt *) NewIfStmt(Cg->tokenLoc, $3, NULL, NULL); ; }
;

/**********************/
/* Compound Statement */
/**********************/

compound_statement:       compound_header block_item_list compound_tail
                              { $$ = (stmt *) NewBlockStmt(Cg->tokenLoc, $2); }
                        | compound_header compound_tail
                              { $$ = NULL; }
;

compound_header:          '{'
                              { PushScope(NewScope()); CurrentScope->funindex = NextFunctionIndex; }
;

compound_tail:            '}'
                              {
                                if (Cg->options.DumpParseTree)
                                    PrintScopeDeclarations();
                                PopScope();
                              }
;

block_item_list:          block_item
                        | block_item_list block_item
                              { $$ = AddStmt($1, $2); }
;

block_item:               declaration
                        | statement
                              { $$ = CheckStmt($1); }
;

/************************/
/* Expression Stetement */
/************************/

expression_statement:     expression_statement2 ';'
                        | ';'
                              { $$ = NULL; }
;

expression_statement2:    postfix_expression /* basic_variable */ '=' expression
                              { $$ = NewSimpleAssignmentStmt(Cg->tokenLoc, $1, $3, 0); }
                        | expression
                              { $$ = (stmt *) NewExprStmt(Cg->tokenLoc, $1); }
                        | postfix_expression ASSIGNMINUS_SY expression
                              { $$ = NewCompoundAssignmentStmt(Cg->tokenLoc, ASSIGNMINUS_OP, $1, $3); }
                        | postfix_expression ASSIGNMOD_SY expression
                              { $$ = NewCompoundAssignmentStmt(Cg->tokenLoc, ASSIGNMOD_OP, $1, $3); }
                        | postfix_expression ASSIGNPLUS_SY expression
                              { $$ = NewCompoundAssignmentStmt(Cg->tokenLoc, ASSIGNPLUS_OP, $1, $3); }
                        | postfix_expression ASSIGNSLASH_SY expression
                              { $$ = NewCompoundAssignmentStmt(Cg->tokenLoc, ASSIGNSLASH_OP, $1, $3); }
                        | postfix_expression ASSIGNSTAR_SY expression
                              { $$ = NewCompoundAssignmentStmt(Cg->tokenLoc, ASSIGNSTAR_OP, $1, $3); }
;

/***********************/
/* Iteration Statement */
/***********************/

iteration_statement:      WHILE_SY '(' boolean_scalar_expression ')' balanced_statement
                              { $$ = (stmt *) NewWhileStmt(Cg->tokenLoc, WHILE_STMT, $3, $5); }
                        | DO_SY statement WHILE_SY '(' boolean_scalar_expression ')' ';'
                              { $$ = (stmt *) NewWhileStmt(Cg->tokenLoc, DO_STMT, $5, $2); }
                        | FOR_SY '(' for_expression_opt ';' boolean_expression_opt ';' for_expression_opt ')' balanced_statement
                              { $$ = (stmt *) NewForStmt(Cg->tokenLoc, $3, $5, $7, $9); }
;

dangling_iteration:       WHILE_SY '(' boolean_scalar_expression ')' dangling_statement
                              { $$ = (stmt *) NewWhileStmt(Cg->tokenLoc, WHILE_STMT, $3, $5); }
                        | FOR_SY '(' for_expression_opt ';' boolean_expression_opt ';' for_expression_opt ')' dangling_statement
                              { $$ = (stmt *) NewForStmt(Cg->tokenLoc, $3, $5, $7, $9); }
;

boolean_scalar_expression:
                          expression
                              {  $$ = CheckBooleanExpr(Cg->tokenLoc, $1, 0); }
;

for_expression_opt:       for_expression
                        | /* empty */
                              { $$ = NULL; }
;

for_expression:           expression_statement2
                        | for_expression ',' expression_statement2
                              {
                                stmt *lstmt = $1;
                                if (lstmt) {
                                    while (lstmt->exprst.next)
                                        lstmt = lstmt->exprst.next;
                                    lstmt->exprst.next = $3;
                                    $$ = $1;
                                } else {
                                    $$ = $3;
                                }
                              }
;

boolean_expression_opt: boolean_scalar_expression
                        | /* empty */
                              { $$ = NULL; }
;

/*******************/
/*Return Statement */
/*******************/

return_statement:         RETURN_SY expression ';'
                              { $$ = (stmt *) NewReturnStmt(Cg->tokenLoc, CurrentScope, $2); }
                        | RETURN_SY ';'
                              { $$ = (stmt *) NewReturnStmt(Cg->tokenLoc, CurrentScope, NULL); }
;

/*********/
/* Misc. */
/*********/

member_identifier:        identifier
;

scope_identifier:         identifier
;

semantics_identifier:     identifier
;

type_identifier:          TYPEIDENT_SY
;

variable_identifier:      identifier
;

identifier:               IDENT_SY
;

constant:                 INTCONST_SY /* Temporary! */
                              { $$ = (expr *) NewIConstNode(ICONST_OP, $1, TYPE_BASE_CINT); }
                        | CFLOATCONST_SY /* Temporary! */
                              { int base = Cg->theHAL->GetFloatSuffixBase(Cg->tokenLoc, ' ');
                                $$ = (expr *) NewFConstNode(FCONST_OP, $1, base);
                              }
                        | FLOATCONST_SY /* Temporary! */
                              { int base = Cg->theHAL->GetFloatSuffixBase(Cg->tokenLoc, 'f');
                                $$ = (expr *) NewFConstNode(FCONST_OP, $1, base);
                              }
                        | FLOATHCONST_SY /* Temporary! */
                              { int base = Cg->theHAL->GetFloatSuffixBase(Cg->tokenLoc, 'h');
                                $$ = (expr *) NewFConstNode(FCONST_OP, $1, base);
                              }
                        | FLOATXCONST_SY /* Temporary! */
                              {int base = Cg->theHAL->GetFloatSuffixBase(Cg->tokenLoc, 'x');
                                $$ = (expr *) NewFConstNode(FCONST_OP, $1, base);
                              }
;

/***
integer_constant:         INTCONST_SY
                              { $$ = $1; }
;
***/

/***
constant_expression:      expression
;
***/

%%

