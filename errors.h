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

//
// errors.h
//

#if !defined(__ERRORS_H)
#define __ERRORS_H 1

// Preprocessor related errors

#define ERROR___CPP_EOL_IN_STRING            101, "EOF/EOL inside preprocessor string"
#define ERROR___CPP_BIND_PRAGMA_ERROR        102, "error in #pragma bind arguments"
#define ERROR_SSSD_DUPLICATE_BINDING         103, "connector binding \"%s.%s\" previously defined at %s(%d)"
#define ERROR___CPP_UNKNOWN_DIRECTIVE        104, "Unknown cpp directive #%s"
#define ERROR___CPP_SYNTAX                   105, "Syntax error in #%s"
#define ERROR___CPP_MACRO_EOF                106, "Unexpected EOF in macro argument list"
#define ERROR___CPP_MACRO_TOOMANY            107, "Too many arguments to macro %s"
#define ERROR___CPP_MACRO_TOOFEW             108, "Not enough arguments to macro %s"

// Scanner errors

#define ERROR___EOF_IN_COMMENT               121, "EOF inside comment"
#define ERROR___FP_CONST_TOO_LONG            122, "floating point constant too long"
#define ERROR___FP_CONST_OVERFLOW            123, "floating point constant overflow"
#define ERROR___ERROR_IN_EXPONENT            124, "error in floating point exponent"
#define ERROR___INTEGER_CONST_OVERFLOW       125, "integer constant overflow"
#define ERROR___HEX_CONST_OVERFLOW           126, "hex constant overflow"
#define ERROR___ERROR_IN_HEX_CONSTANT        127, "error in hex constant"
#define ERROR___OCT_CONST_OVERFLOW           128, "octal constant overflow"

// Parser errors

#define ERROR_S_TYPE_NAME_EXPECTED           501, "type name expected at token \"%s\""

// Semantic errors

#define ERROR_S_INIT_STRUCT_MEMBER          1001, "initialization of struct members not allowed \"%s\""
#define ERROR_S_NAME_ALREADY_DEFINED        1002, "the name \"%s\" is already defined"
#define ERROR_S_VOID_TYPE_INVALID           1003, "void type not allowed \"%s\""
#define ERROR_S_IN_OUT_PARAMS_ONLY          1004, "in and out only apply to formal parameters \"%s\""
#define ERROR_S_INLINE_FOR_FUN              1005, "inline modifier only for functions \"%s\""
#define ERROR_S_UNSIZED_ARRAY               1006, "unsized array type not allowed \"%s\""
#define ERROR___VOID_NOT_ONLY_PARAM         1007, "void must be only parameter"
#define ERROR_S_UNDEFINED_VAR               1008, "undefined variable \"%s\""
#define ERROR_SS_NOT_A_MEMBER               1009, "\"%s\" is not member of struct/category \"%s\""
#define ERROR_S_LEFT_EXPR_NOT_STRUCT_ARRAY  1010, "expression left of .\"%s\" is not a struct or array"
#define ERROR___INDEX_OF_NON_ARRAY          1011, "cannot index a non-array value"
#define ERROR_S_ABSTRACT_NOT_ALLOWED        1012, "abstract parameters not allowed in function definition \"%s\""
#define ERROR_S_FUN_ALREADY_DEFINED         1013, "function \"%s\" is already defined"
#define ERROR_S_NOT_A_FUN                   1014, "\"%s\" is not a function"
#define ERROR___VOID_FUN_RETURNS_VALUE      1015, "void function cannot return a value"
#define ERROR___RETURN_EXPR_INCOMPAT        1016, "expression type incompatible with function return type"
#define ERROR_S_PARAM_NAME_TWICE            1017, "parameter name \"%s\" appears twice in function declaration"
#define ERROR___BOOL_EXPR_EXPECTED          1018, "Boolean expression expected"
#define ERROR___SCALAR_BOOL_EXPR_EXPECTED   1019, "scalar Boolean expression expected"
#define ERROR_S_INVALID_OPERANDS            1020, "invalid operands to \"%s\""
#define ERROR_S_OPERANDS_NOT_INTEGRAL       1021, "operands to \"%s\" must be integral"
#define ERROR_S_OPERANDS_NOT_NUMERIC        1022, "operands to \"%s\" must be numeric"
#define ERROR_S_OPERANDS_NOT_BOOLEAN        1023, "operands to \"%s\" must be Boolean"
#define ERROR_S_OPERANDS_NOT_SCALAR_VECTOR  1024, "operands to \"%s\" must be scalar or vector"
#define ERROR_S_OPERANDS_NOT_MATRIX         1025, "expected matrix operand to \"%s\""
#define ERROR_S_SCALAR_OP_VECTOR_INVALID    1026, "\"scalar %s vector\" not allowed"
#define ERROR_S_VECTOR_OP_SCALAR_INVALID    1027, "\"vector %s scalar\" not allowed"
#define ERROR_S_VECTOR_OPERANDS_DIFF_LEN    1028, "vector operands to \"%s\" must be of equal length"
#define ERROR_S_VECTOR_MATRIX_MISMATCH      1029, "length of vector must equal number of rows in matrix in \"V %s M\""
#define ERROR_S_MATRIX_VECTOR_MISMATCH      1030, "number of columns in matrix must equal length of vector in \"M %s V\""
#define ERROR_S_SWIZZLE_MASK_EL_MISSING     1031, "swizzle mask element not present in operand \"%s\""
#define ERROR___MIXED_NUM_NONNUM_VECT_CNSTR 1032, "cannot mix numeric and non numeric elements in constructed vectors"
#define ERROR___INVALID_CAST                1033, "cast not allowed"
#define ERROR___ASSIGN_TO_NON_LVALUE        1034, "assignment to non-lvalue"
#define ERROR___ASSIGN_INCOMPATIBLE_TYPES   1035, "assignment of incompatible types"
#define ERROR___ASSIGN_TO_CONST_VALUE       1036, "assign to const variable"
#define ERROR___ASSIGN_VARYING_TO_UNIFORM   1037, "assignment of varying value to uniform variable"
#define ERROR___MASKED_ASSIGN_TO_VAR        1036, "masked assignment only allowed on vectors"
#define ERROR_S_MASKED_ASSIGN_NON_EXISTENT  1037, "masked assignment to non-existent element(s) \"%s\""
#define ERROR_S_CONFLICTING_DECLARATION     1038, "declaration of \"%s\" conflicts with previous declaration"
#define ERROR___CONST_OUT_INVALID           1039, "const and out qualifiers not allowed together"
#define ERROR___CONFLICTING_DOMAIN          1040, "domain declaration conflicts with previous declaration"
#define ERROR___REPEATED_TYPE_ATTRIB        1041, "repeated type attribite"
#define ERROR_S_TAG_IS_NOT_A_STRUCT         1042, "tag \"%s\" is not a struct"
#define ERROR___DIMENSION_LT_1              1043, "size of dimension cannot be less than 1"
#define ERROR___ARRAY_OF_VOID               1044, "array of void not allowed"
#define ERROR___ARRAY_OF_FUNS               1045, "array of functions not allowed"
#define ERROR_D_EXCEEDS_MAX_DIMS            1046, "array declaration exceeds %d dimensions"
#define ERROR_SSD_STRUCT_ALREADY_DEFINED    1047, "struct \"%s\" previously defined at %s(%d)"
#define ERROR_CS_INVALID_SWIZZLE_CHAR       1048, "invalid character '%c' in swizzle \"%s\""
#define ERROR_S_SWIZZLE_TOO_LONG            1049, "swizzle too long \"%s\""
#define ERROR_CS_INVALID_CHAR_IN_MASK       1050, "invalid character '%c' in write mask \"%s\""
#define ERROR_CS_DUPLICATE_CHAR_IN_MASK     1051, "duplicate character '%c' in write mask \"%s\""
#define ERROR_S_OPERANDS_HAVE_SIDE_EFFECTS  1052, "operands to \"%s\" can't have side effects"
#define ERROR_S_INVALID_CONDITION_OPERAND   1053, "invalid Boolean condition expression"
#define ERROR_S_INIT_NON_VARIABLE           1054, "initialization of non-variable \"%s\""

#define ERROR___INVALID_INITIALIZATION      1056, "invalid initialization"
#define ERROR___TOO_LITTLE_DATA             1057, "too little data in initialization"
#define ERROR___TOO_MUCH_DATA               1058, "too much data in initialization"
#define ERROR___NON_CONST_INITIALIZATION    1059, "non constant expression in initialization"
#define ERROR___INCOMPAT_TYPE_INIT          1060, "incompatible types in initialization"
#define ERROR_S_INIT_EXTERN                 1061, "initialization of extern variable \"%s\""
#define ERROR___STORAGE_SPECIFIED_TWICE     1062, "storage class specified twice"
#define ERROR___CONFLICTING_STORAGE         1063, "storage class conflicts with previous specification"
#define ERROR_S_STORAGE_NOT_ALLOWED         1064, "storage class specifier not allowed"
#define ERROR_S_STORAGE_NOT_ALLOWED_TYPEDEF 1065, "storage class specifier not allowed in typedef"
#define ERROR___INVALID_TYPE_FUNCTION       1066, "invalid type in type constructor"
#define ERROR___TOO_LITTLE_DATA_TYPE_FUN    1067, "too little data in type constructor"
#define ERROR___TOO_MUCH_DATA_TYPE_FUN      1068, "too much data in type constructor"

#define ERROR_S_AMBIGUOUS_FUN_REFERENCE     1101, "ambiguous overloaded function reference \"%s\""
#define ERROR_D_INCOMPATIBLE_PARAMETER      1102, "incompatible type for parameter #%d"
#define ERROR___TOO_FEW_PARAMS              1103, "too few parameters in function call"
#define ERROR___TOO_MANY_PARAMS             1104, "too many parameters in function call"
#define ERROR___CALL_OF_NON_FUNCTION        1105, "cannot call a non-function"
#define ERROR_S_OVERLOAD_DIFF_ONLY_QUALS    1106, "overloaded function declaration \"%s\" differs only in parameter qualifiers"
#define ERROR_S_OVERLOAD_DIFF_ONLY_RETURN   1107, "overloaded function declaration \"%s\" differs only in return type"
#define ERROR_S_NO_STATEMENTS               1108, "function \"%s\" has no statements"
#define ERROR_S_FUN_TYPE_INVALID            1109, "function type not allowed for parameter \"%s\""
#define ERROR_S_FUNCTION_HAS_NO_RETURN      1110, "function \"%s\" has no return statement"
#define ERROR_D_OUT_PARAM_NOT_LVALUE        1111, "non-lvalue actual parameter #%d cannot be out parameter"
#define ERROR_D_OUT_PARAM_IS_CONST          1112, "const qualified actual parameter #%d cannot be out parameter"
#define ERROR_D_OUT_PARAM_NOT_SAME_TYPE     1113, "actual parameter #%d must be same type as formal out parameter"
#define ERROR_S_NON_UNIFORM_PARAM_INIT      1114, "only uniform parameters can be initialized \"%s\""
#define ERROR_S_NO_COMPAT_OVERLOADED_FUN    1115, "unable to find compatible overloaded function \"%s\""

#define ERROR___QSTN_SCALAR_3RD_OPND_EXPECTED   1201, "expected scalar third operand to \"? :\""
#define ERROR___QSTN_VECTOR_3RD_OPND_EXPECTED   1202, "expected vector third operand to \"? :\""
#define ERROR___QSTN_VECTOR_23_OPNDS_EXPECTED   1203, "expected vector second and third operands to \"? :\""
#define ERROR___QSTN_23_OPNDS_INCOMPAT      1204, "incompatible second and third operands to \"? :\""
#define ERROR___QSTN_23_OPNDS_INVALID       1205, "invalid second and third operands to \"? :\""
#define ERROR___QSTN_INVALID_1ST_OPERAND    1206, "invalid first operand to \"? :\""
#define ERROR___QSTN_1ST_OPERAND_NOT_SCALAR 1207, "expected scalar first operand to \"? :\""

#define ERROR___REF_TO_VAR_WITH_NO_VALUE    1301, "use of variable with no defined value"

// Errors detected when compiling

#define ERROR___NO_PROGRAM                  3001, "no program defined"
#define ERROR_S_CALL_UNDEF_FUN              3002, "call to undefined function \"%s\""
#define ERROR___NO_ERROR                    3003, "no error detected since previous error token"

// Non-supported language features

#define ERROR_S_UNSUPPORTED_OPERATOR        4001, "the C operator \"%s\" is not defined in this language"
#define ERROR_S_UNEXPECTED_COLON            4002, "unexpected \": %s\""

// Profile specific limitations globally shared by any profile

#define ERROR_S_CONNECTOR_TYPE_INVALID      5001, "connector type \"%s\" is not valid for this profile"
#define ERROR_S_CONNECT_FOR_OUTPUT          5002, "connector type \"%s\" only valid as output for this profile"
#define ERROR_S_CONNECT_FOR_INPUT           5003, "connector type \"%s\" only valid as input for this profile"
#define ERROR_S_INTERNAL_FOR_FUN            5004, "__internal modifier only for functions \"%s\""
#define ERROR_S_RECURSION                   5005, "recursive call to function \"%s\""
#define ERROR___PACKED_DIM_EXCEEDS_4        5006, "size of packed dimension cannot exceed 4"
#define ERROR___LOW_DIM_UNSPECIFIED         5007, "only size of greatest dimension can be unspecified"
#define ERROR___NUM_DIMS_EXCEEDS_3          5008, "number of dimensions of array cannot exceed 3"
#define ERROR_S_INT_NOT_ALLOWED             5009, "type \"int\" definitions not allowed \"%s\""
#define ERROR___IF_NOT_SUPPORTED            5010, "profile does not support \"if\" statements"
#define ERROR___WHILE_NOT_SUPPORTED         5011, "profile does not support \"while\" statements"
#define ERROR___DO_NOT_SUPPORTED            5012, "profile does not support \"do\" statements"
#define ERROR___FOR_NOT_SUPPORTED           5013, "profile does not support \"for\" statements"
#define ERROR___DISCARD_NOT_SUPPORTED       5014, "profile does not support \"discard\" statements"
#define ERROR___VECTOR_EXPR_LEN_GR_4        5015, "length of vector expressions cannot exceed 4"
#define ERROR_S_VECTOR_OPERAND_GR_4         5016, "length of vector operands to \"%s\" cannot exceed 4"
#define ERROR_S_MATRIX_OPERAND_GR_4         5017, "dimensions of matrix operands to \"%s\" cannot exceed 4"
#define ERROR___CONSTRUCTER_VECTOR_LEN_GR_4 5018, "length of constructed vectors cannot exceed 4"
#define ERROR_S_ONE_PROGRAM                 5019, "one program per compilation, program \"%s\" also defined"
#define ERROR_C_UNSUPPORTED_FP_SUFFIX       5020, "profile does not support float constant suffix \"%c\""

#define ERROR_S_PROGRAM_PARAM_NOT_UNIFORM   5028, "non-connector program parameter \"%s\" must have uniform domain"
#define ERROR_S_PROGRAM_MUST_RETURN_STRUCT  5029, "program \"%s\" must return a struct"

//#define ERROR_SS_CMEMBER_NOT_SCALAR_VECTOR  5032, "connector member \"%s.%s\" is not a scalar or vector"
//#define ERROR_SS_CMEMBER_NOT_NUMERIC        5033, "connector member \".%s%s\" is not a numeric type"
//#define ERROR_SS_CMEMBER_SIZE_TOO_LARGE     5034, "connector member \".%s%s\" has more elements than bound register does"
#define ERROR_S_CMEMBER_NOT_WRITABLE        5035, "connector member \"%s\" is not writable in this profile"
#define ERROR_S_CMEMBER_NOT_READABLE        5036, "connector member \"%s\" is not readable in this profile"
#define ERROR_S_CMEMBER_NOT_VISIBLE         5037, "connector member \"%s\" is not visible in this profile"
//#define ERROR_S_CMEMBER_BINDING_ERROR       5038, "cannot locate suitable register to bind connector member \"%s\""
//#define ERROR_SSS_CMEMBER_RESERVED_NAME     5039, "reserved connector member \"%s.%s\" cannot be bound to register \"%s\""
#define ERROR_S_INCOMPATIBLE_BIND_DIRECTIVE 5040, "type of parameter \"%s\" incompatible with bind directive"
#define ERROR_S_PARAMETER_BINDING_ERROR     5041, "cannot locate suitable resource to bind parameter \"%s\""
#define ERROR___RETURN_NOT_LAST             5042, "profile requires \"return\" statement to be last statement in function"
#define ERROR___ARRAY_INDEX_NOT_CONSTANT    5043, "profile requires index expression to be compile-time constant"
#define ERROR___MATRIX_NOT_SIMPLE           5044, "profile requires matrices to be simple variables"
#define ERROR_S_ILLEGAL_PARAM_TO_MAIN       5045, "illegal parameter to main \"%s\""
#define ERROR_S_MAIN_PARAMS_CANT_BE_INOUT   5046, "parameters to main cannot be in out \"%s\""
#define ERROR_S_UNIF_BIND_TO_NON_UNIF_VAR   5047, "non-uniform variable bound to uniform register in bind directive \"%s\""
#define ERROR_S_NON_UNIF_BIND_TO_UNIF_VAR   5048, "uniform variable bound to non-uniform register in bind directive \"%s\""
#define ERROR_S_ILLEGAL_TYPE_UNIFORM_VAR    5049, "type not allowed for uniform variables \"%s\""
#define ERROR_S_UNPACKED_ARRAY              5050, "profile does not support arrays of non-packed types \"%s\""

//#define ERROR_S_SEMANTICS_CANT_HAVE_INDEX   5101, "semantics attribute \"%s\" cannot have a numeric index"
#define ERROR_S_SEMANTICS_INDEX_TOO_BIG     5102, "semantics attribute \"%s\" has too big of a numeric index"
#define ERROR_S_STATIC_CANT_HAVE_SEMANTICS  5103, "static variables cannot have semantics \"%s\""
#define ERROR_S_NO_LOCAL_SEMANTICS          5104, "local variables cannot have semantics \"%s\""
#define ERROR_S_NON_STATIC_SEM_NOT_UNIFORM  5105, "non-static global variables with semantics must be declared uniform \"%s\""
#define ERROR_S_SEMANTICS_NON_VARIABLE      5106, "semantics not allowed on non-variable \"%s\""
#define ERROR_S_SEM_VAR_NOT_SCALAR_VECTOR   5107, "semantics not allowed on variable which is not a scalar or vector \"%s\""
#define ERROR_S_UNKNOWN_SEMANTICS           5108, "unknown semantics specified \"%s\""
#define ERROR_S_SEMANTIC_DOMAIN_CONFLICT    5109, "variable domain conflicts with semantics \"%s\""
#define ERROR_S_UNIFORM_ARG_CANT_BE_OUT     5110, "parameters with uniform domain cannot be out parameters \"%s\""
#define ERROR_S_OUT_QUALIFIER_IN_SEMANTIC   5111, "out qualifier cannot be used with in sematics \"%s\""
#define ERROR_S_IN_QUALIFIER_OUT_SEMANTIC   5112, "in qualifier cannot be used with out sematics \"%s\""
#define ERROR_S_MAIN_ARG_NO_SEM_NOT_UNIFORM 5113, "parameters to main with no semantics must be uniform domain \"%s\""

#define ERROR_S_NESTED_SEMANTIC_STRUCT      5115, "nested structs cannot have semantics \"%s\""
#define ERROR_S_SEMANTIC_NOT_DEFINED_VOUT   5116, "no varying output semantics defined that match variable/member name \"%s\""
#define ERROR_S_NON_STATIC_GLOBAL_TYPE_ERR  5117, "unsupported type for uniform semantics; not allowed for non-static globals \"%s\""
#define ERROR_S_SEMANTIC_TYPE_CONFLICT      5118, "variable type conflicts with semantics \"%s\""
#define ERROR_SS_VAR_SEMANTIC_NOT_VISIBLE   5119, "variable/member \"%s\" has semantic \"%s\" which is not visible in this profile"
#define ERROR_S_NO_TEXUNITS_AVAILABLE       5120, "no texture units left to bind to \"%s\""

#define ERROR_S_INVALID_INTERNAL_FUNCTION   5201, "invalid internal function declaration for \"%s\""

// Profile specific features not yet implemented

#define ERROR___IF_NOT_YET_IMPLEMENTED      5501, "\"if\" statements not yet implemented"
#define ERROR___VECTOR_CONSTR_NOT_NUM_BOOL  5502, "constructed vector requires numeric or bool elements"
#define ERROR___VECTOR_CONSTR_NOT_SCALAR    5503, "non-scalar not allowed in vector constructor"
#define ERROR_S_STRUCT_PARAM_NOT_YET        5504, "struct parameters not yet implemented \"%s\""
#define ERROR_S_CONNECT_PARAM_NOT_YET       5505, "connector parameters not yet implemented \"%s\""
#define ERROR___ARRAY2_INIT_NOT_DONE        5507, "initialization of arrays with more than 1 dimension not yet implemented"
#define ERROR_S_UNSUPPORTED_PROFILE_OP      5508, "the operator \"%s\" is not supported by this profile"

// Numbers 6000 to 6999 are reserved for profile specific messages

// Warnings:

#define WARNING___QUALIFIER_SPECIFIED_TWICE 7001, "qualifier specified twice"
#define WARNING___DOMAIN_SPECIFIED_TWICE    7002, "domain specified twice"
#define WARNING_S_VOID_FUN_HAS_NO_OUT_ARGS  7003, "void function \"%s\" has no out parameters"
#define WARNING_S_UNBOUND_CONNECTOR_MEMBER  7004, "unbound connector member \"%s\""
#define WARNING_S_CMEMBER_NOT_WRITTEN       7005, "no value written to connector member \"%s\""
#define WARNING_S_UNINITIALIZED_VAR_USED    7006, "uninitialized variable \"%s\" used"
#define WARNING_S_FORWARD_SEMANTICS_IGNORED 7007, "\"%s\" semantics in forward declaration ignored"
#define WARNING_S_SEMANTICS_AND_BINDING     7008, "non-uniform variable has both semantics and binding \"%s\""
#define WARNING_S_CANT_BIND_UNIFORM_VAR     7009, "cannot locate default uniform binding for \"%s\""

// CPP wornings
#define WARNING___CPP_MACRO_REDEFINED       7101, "Macro %s redefined"
#define WARNING___CPP_IF_MISMATCH           7102, "unmatched #%s"


// "Helper" messages, trying to explain common errors

// Internal compiler error messages

#define ERROR_S_TYPE_NAME_NOT_FOUND         9001, "type name not found \"%s\""
#define ERROR_S_NAME_NOT_A_TYPE             9002, "name is not a type name \"%s\""
#define ERROR_S_NO_CODE_GENERATOR           9003, "missing code generator for program profile \"%s\""
#define ERROR_S_SYMBOL_NOT_FUNCTION         9004, "symbol not function \"%s\""
#define ERROR_S_SYMBOL_TYPE_NOT_FUNCTION    9005, "symbol's type category not function \"%s\""
#define ERROR___TYPE_NOT_ARRAY              9006, "type not an array"
#define ERROR_S_NO_CODE_HEADER              9007, "missing code header function for program profile \"%s\""
#define ERROR_S_MALLOC_FAILED               9008, "malloc failed in \"%s\""
#define ERROR___NO_MATRIX_DECONSTRUCTION    9009, "matrix deconstruction not supported"

// Misc. notices

#define NOTICE_SSS_CMEMBER_ALLOCATED        9501, "connector member \"%s.%s\" allocated to register \"%s\""

#endif // !defined(__ERRORS_H)
