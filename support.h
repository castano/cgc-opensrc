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
// support.h
//

#if !defined(__SUPPORT_H)
#define __SUPPORT_H 1

#define OPENSL_TAG "cgc"

// Typedefs for things defined here in "support.h":

typedef struct dtype_rec dtype;
typedef struct decl_rec decl;
typedef union expr_rec expr;
typedef struct symb_rec symb;
typedef union stmt_rec stmt;
typedef int spec;

typedef struct StmtList_Rec {
    stmt *first;
    stmt *last;
} StmtList;

typedef enum stmtkind {
    EXPR_STMT, IF_STMT, WHILE_STMT, DO_STMT, FOR_STMT,
    BLOCK_STMT, RETURN_STMT, DISCARD_STMT, COMMENT_STMT,
    LAST_STMTKIND
} stmtkind;

typedef enum nodekind {
    DECL_N=LAST_STMTKIND, SYMB_N, CONST_N, UNARY_N, BINARY_N, TRINARY_N,
    LAST_NODEKIND,
} nodekind;

typedef enum subopkind {
    SUB_NONE, SUB_S, SUB_V, SUB_VS, SUB_SV, SUB_M, SUB_VM, SUB_MV,
    SUB_Z, SUB_ZM, SUB_CS, SUB_CV, SUB_CM, SUB_KV,
} subopkind;

#define OPCODE_TABLE \
    PICK( VARIABLE_OP, "var",    IDENT_SY, SYMB_N, SUB_NONE ), \
    PICK( MEMBER_OP,   "member", IDENT_SY, SYMB_N, SUB_NONE ), \
    \
    PICK( ICONST_OP,   "iconst",  0, CONST_N, SUB_S ), \
    PICK( ICONST_V_OP, "iconstv", 0, CONST_N, SUB_V ), \
    PICK( BCONST_OP,   "bconst",  0, CONST_N, SUB_S ), \
    PICK( BCONST_V_OP, "bconstv", 0, CONST_N, SUB_V ), \
    PICK( FCONST_OP,   "fconst",  0, CONST_N, SUB_S ), \
    PICK( FCONST_V_OP, "fconstv", 0, CONST_N, SUB_V ), \
    PICK( HCONST_OP,   "hconst",  0, CONST_N, SUB_S ), \
    PICK( HCONST_V_OP, "hconstv", 0, CONST_N, SUB_V ), \
    PICK( XCONST_OP,   "xconst",  0, CONST_N, SUB_S ), \
    PICK( XCONST_V_OP, "xconstv", 0, CONST_N, SUB_V ), \
    \
    PICK( VECTOR_V_OP,  "vector",  0,   UNARY_N, SUB_V  ), \
    PICK( SWIZZLE_Z_OP, "swizzle", '.', UNARY_N, SUB_Z  ), \
    PICK( SWIZMAT_Z_OP, "swizmat", '.', UNARY_N, SUB_ZM ), \
    PICK( CAST_CS_OP,   "cast",    '(', UNARY_N, SUB_CS ), \
    PICK( CAST_CV_OP,   "castv",   '(', UNARY_N, SUB_CV ), \
    PICK( CAST_CM_OP,   "castm",   '(', UNARY_N, SUB_CM ), \
    PICK( NEG_OP,       "neg",     '-', UNARY_N, SUB_S  ), \
    PICK( NEG_V_OP,     "negv",    '-', UNARY_N, SUB_V  ), \
    PICK( POS_OP,       "pos",     '+', UNARY_N, SUB_S  ), \
    PICK( POS_V_OP,     "posv",    '+', UNARY_N, SUB_V  ), \
    PICK( NOT_OP,       "not",     '~', UNARY_N, SUB_S  ), \
    PICK( NOT_V_OP,     "notv",    '~', UNARY_N, SUB_V  ), \
    PICK( BNOT_OP,      "bnot",    '!', UNARY_N, SUB_S  ), \
    PICK( BNOT_V_OP,    "bnotv",   '!', UNARY_N, SUB_V  ), \
    \
    PICK( KILL_OP,      "kill",    DISCARD_SY, UNARY_N, SUB_S ), \
    \
    PICK( PREDEC_OP,    "predec",  MINUSMINUS_SY, UNARY_N, SUB_S ), \
    PICK( PREINC_OP,    "preinc",  PLUSPLUS_SY,   UNARY_N, SUB_S ), \
    PICK( POSTDEC_OP,   "postdec", MINUSMINUS_SY, UNARY_N, SUB_S ), \
    PICK( POSTINC_OP,   "postinc", PLUSPLUS_SY,   UNARY_N, SUB_S ), \
    \
    PICK( MEMBER_SELECTOR_OP, "mselect", '.',   BINARY_N, SUB_NONE ), \
    PICK( ARRAY_INDEX_OP,     "index",   '[',   BINARY_N, SUB_NONE ), \
    PICK( FUN_CALL_OP,        "call",    '(',   BINARY_N, SUB_NONE ), \
    PICK( FUN_BUILTIN_OP,     "builtin", 0,     BINARY_N, SUB_NONE ), \
    PICK( FUN_ARG_OP,         "arg",     0,     BINARY_N, SUB_NONE ), \
    PICK( EXPR_LIST_OP,       "list",    0,     BINARY_N, SUB_NONE ), \
    PICK( MUL_OP,             "mul",     '*',   BINARY_N, SUB_S    ), \
    PICK( MUL_V_OP,           "mulv",    '*',   BINARY_N, SUB_V    ), \
    PICK( MUL_SV_OP,          "mulsv",   '*',   BINARY_N, SUB_SV   ), \
    PICK( MUL_VS_OP,          "mulvs",   '*',   BINARY_N, SUB_VS   ), \
    PICK( DIV_OP,             "div",     '/',   BINARY_N, SUB_S    ), \
    PICK( DIV_V_OP,           "divv",    '/',   BINARY_N, SUB_V    ), \
    PICK( DIV_SV_OP,          "divsv",   '/',   BINARY_N, SUB_SV   ), \
    PICK( DIV_VS_OP,          "divvs",   '/',   BINARY_N, SUB_VS   ), \
    PICK( MOD_OP,             "mod",     '%',   BINARY_N, SUB_S    ), \
    PICK( MOD_V_OP,           "modv",    '%',   BINARY_N, SUB_V    ), \
    PICK( MOD_SV_OP,          "modsv",   '%',   BINARY_N, SUB_SV   ), \
    PICK( MOD_VS_OP,          "modvs",   '%',   BINARY_N, SUB_VS   ), \
    PICK( ADD_OP,             "add",     '+',   BINARY_N, SUB_S    ), \
    PICK( ADD_V_OP,           "addv",    '+',   BINARY_N, SUB_V    ), \
    PICK( ADD_SV_OP,          "addsv",   '+',   BINARY_N, SUB_SV   ), \
    PICK( ADD_VS_OP,          "addvs",   '+',   BINARY_N, SUB_VS   ), \
    PICK( SUB_OP,             "sub",     '-',   BINARY_N, SUB_S    ), \
    PICK( SUB_V_OP,           "subv",    '-',   BINARY_N, SUB_V    ), \
    PICK( SUB_SV_OP,          "subsv",   '-',   BINARY_N, SUB_SV   ), \
    PICK( SUB_VS_OP,          "subvs",   '-',   BINARY_N, SUB_VS   ), \
    PICK( SHL_OP,             "shl",     LL_SY, BINARY_N, SUB_S    ), \
    PICK( SHL_V_OP,           "shlv",    LL_SY, BINARY_N, SUB_V    ), \
    PICK( SHR_OP,             "shr",     GG_SY, BINARY_N, SUB_S    ), \
    PICK( SHR_V_OP,           "shrv",    GG_SY, BINARY_N, SUB_V    ), \
    PICK( LT_OP,              "lt",      '<',   BINARY_N, SUB_S    ), \
    PICK( LT_V_OP,            "ltv",     '<',   BINARY_N, SUB_V    ), \
    PICK( LT_SV_OP,           "ltsv",    '<',   BINARY_N, SUB_SV   ), \
    PICK( LT_VS_OP,           "ltvs",    '<',   BINARY_N, SUB_VS   ), \
    PICK( GT_OP,              "gt",      '>',   BINARY_N, SUB_S    ), \
    PICK( GT_V_OP,            "gtv",     '>',   BINARY_N, SUB_V    ), \
    PICK( GT_SV_OP,           "gtsv",    '>',   BINARY_N, SUB_SV   ), \
    PICK( GT_VS_OP,           "gtvs",    '>',   BINARY_N, SUB_VS   ), \
    PICK( LE_OP,              "le",      LE_SY, BINARY_N, SUB_S    ), \
    PICK( LE_V_OP,            "lev",     LE_SY, BINARY_N, SUB_V    ), \
    PICK( LE_SV_OP,           "lesv",    LE_SY, BINARY_N, SUB_SV   ), \
    PICK( LE_VS_OP,           "levs",    LE_SY, BINARY_N, SUB_VS   ), \
    PICK( GE_OP,              "ge",      GE_SY, BINARY_N, SUB_S    ), \
    PICK( GE_V_OP,            "gev",     GE_SY, BINARY_N, SUB_V    ), \
    PICK( GE_SV_OP,           "gesv",    GE_SY, BINARY_N, SUB_SV   ), \
    PICK( GE_VS_OP,           "gevs",    GE_SY, BINARY_N, SUB_VS   ), \
    PICK( EQ_OP,              "eq",      EQ_SY, BINARY_N, SUB_S    ), \
    PICK( EQ_V_OP,            "eqv",     EQ_SY, BINARY_N, SUB_V    ), \
    PICK( EQ_SV_OP,           "eqsv",    EQ_SY, BINARY_N, SUB_SV   ), \
    PICK( EQ_VS_OP,           "eqvs",    EQ_SY, BINARY_N, SUB_VS   ), \
    PICK( NE_OP,              "ne",      NE_SY, BINARY_N, SUB_S    ), \
    PICK( NE_V_OP,            "nev",     NE_SY, BINARY_N, SUB_V    ), \
    PICK( NE_SV_OP,           "nesv",    NE_SY, BINARY_N, SUB_SV   ), \
    PICK( NE_VS_OP,           "nevs",    NE_SY, BINARY_N, SUB_VS   ), \
    PICK( AND_OP,             "and",     '&',   BINARY_N, SUB_S    ), \
    PICK( AND_V_OP,           "andv",    '&',   BINARY_N, SUB_V    ), \
    PICK( AND_SV_OP,          "andsv",   '&',   BINARY_N, SUB_SV   ), \
    PICK( AND_VS_OP,          "andvs",   '&',   BINARY_N, SUB_VS   ), \
    PICK( XOR_OP,             "xor",     '^',   BINARY_N, SUB_S    ), \
    PICK( XOR_V_OP,           "xorv",    '^',   BINARY_N, SUB_V    ), \
    PICK( XOR_SV_OP,          "xorsv",   '^',   BINARY_N, SUB_SV   ), \
    PICK( XOR_VS_OP,          "xorvs",   '^',   BINARY_N, SUB_VS   ), \
    PICK( OR_OP,              "or",      '|',   BINARY_N, SUB_S    ), \
    PICK( OR_V_OP,            "orv",     '|',   BINARY_N, SUB_V    ), \
    PICK( OR_SV_OP,           "orsv",    '|',   BINARY_N, SUB_SV   ), \
    PICK( OR_VS_OP,           "orvs",    '|',   BINARY_N, SUB_VS   ), \
    PICK( BAND_OP,            "band",    AND_SY, BINARY_N, SUB_S    ), \
    PICK( BAND_V_OP,          "bandv",   AND_SY, BINARY_N, SUB_V    ), \
    PICK( BAND_SV_OP,         "bandsv",  AND_SY, BINARY_N, SUB_SV   ), \
    PICK( BAND_VS_OP,         "bandvs",  AND_SY, BINARY_N, SUB_VS   ), \
    PICK( BOR_OP,             "bor",     OR_SY, BINARY_N, SUB_S    ), \
    PICK( BOR_V_OP,           "borv",    OR_SY, BINARY_N, SUB_V    ), \
    PICK( BOR_SV_OP,          "borsv",   OR_SY, BINARY_N, SUB_SV   ), \
    PICK( BOR_VS_OP,          "borvs",   OR_SY, BINARY_N, SUB_VS   ), \
    PICK( ASSIGN_OP,          "assign",  '=',   BINARY_N, SUB_S    ), \
    PICK( ASSIGN_V_OP,        "assignv", '=',   BINARY_N, SUB_V    ), \
    PICK( ASSIGN_GEN_OP,      "assigngen", '=', BINARY_N, SUB_NONE ), \
    PICK( ASSIGN_MASKED_KV_OP, "assignm", '=',  BINARY_N, SUB_KV  ), \
    \
    PICK( ASSIGNMINUS_OP,     "assign-", ASSIGNMINUS_SY, BINARY_N, SUB_S ), \
    PICK( ASSIGNMOD_OP,       "assign%", ASSIGNMOD_SY,   BINARY_N, SUB_S ), \
    PICK( ASSIGNPLUS_OP,      "assign+", ASSIGNPLUS_SY,  BINARY_N, SUB_S ), \
    PICK( ASSIGNSLASH_OP,     "assign/", ASSIGNSLASH_SY, BINARY_N, SUB_S ), \
    PICK( ASSIGNSTAR_OP,      "assign*", ASSIGNSTAR_SY,  BINARY_N, SUB_S ), \
    PICK( COMMA_OP,           "comma",   ',', BINARY_N,  SUB_NONE ), \
    \
    PICK( COND_OP,            "cond",    '?', TRINARY_N, SUB_S    ), \
    PICK( COND_V_OP,          "condv",   '?', TRINARY_N, SUB_V    ), \
    PICK( COND_SV_OP,         "condsv",  '?', TRINARY_N, SUB_SV   ), \
    PICK( COND_GEN_OP,        "condgen", '?', TRINARY_N, SUB_NONE ), \
    PICK( ASSIGN_COND_OP,     "assc",    '@', TRINARY_N, SUB_S    ), \
    PICK( ASSIGN_COND_V_OP,   "asscv",   '@', TRINARY_N, SUB_V    ), \
    PICK( ASSIGN_COND_SV_OP,  "asscsc",  '@', TRINARY_N, SUB_VS   ), \
    PICK( ASSIGN_COND_GEN_OP, "asscgen", '@', TRINARY_N, SUB_NONE ),


// Description of opcode classes:
//
// SSSS:  Size or number of values in an operand:
//        0:      scalar or other (like struct)
//        1 to 4: vector or array dimension
// TTTT:  Base type (see type properties)
//
// _:  0000 0000 0000 0000 0000 0000 0000 TTTT
// V:  0000 0000 0000 0000 0000 0000 SSSS TTTT
// VS: 0000 0000 0000 0000 0000 0000 SSSS TTTT
// SV: 0000 0000 0000 0000 0000 0000 SSSS TTTT
// M:  --- Not used yet, reserved for matrix inner product ---
// VM: 0000 0000 0000 0000 SSS2 0000 SSS1 TTTT - Vector length SSS2, Mat size SSS2 by SSS1
// MV: 0000 0000 0000 0000 SSS2 0000 SSS1 TTTT - Vector length SSS1, Mat size SSS2 by SSS1
// Z:  0000 0000 MMMM-MMMM SSS2 0000 SSS1 TTTT - Swizzle vector/scalar size SSS2 to SSS1
//                                              CTD -- the above appears to be wrong;
//                                              should be SSS1 to SSS2
// ZM: MMMM-MMMM-MMMM-MMMM SSS2 SSSR SSS1 TTTT - Swizzle matrix size SSS2 by SSS1 to SSSR
// CS: 0000 0000 0000 0000 0000 TTT2 0000 TTT1 - Cast scalar type base1 to base2
// CV: 0000 0000 0000 0000 0000 TTT2 SSS1 TTT1 - Cast vector
// CM: 0000 0000 0000 0000 SSS2 TTT2 SSS1 TTT1 - Case matrix
// KV: 0000 0000 0000 MMMM 0000 0000 SSSS TTTT - Masked vector write

#undef PICK
#define PICK(a, b, c, d, e) a

typedef enum opcode {
    OPCODE_TABLE
    LAST_OPCODE,
    // Some useful offsets:
    OFFSET_S_OP = 0, OFFSET_V_OP = 1, OFFSET_SV_OP = 2, OFFSET_VS_OP = 3,
} opcode;

#undef PICK

extern const char *opcode_name[];
extern const int opcode_atom[];

#define SUBOP__(base)                                                   \
        ((base) & 15)

#define SUBOP_S(base)                                                   \
        ((base) & 15)

#define SUBOP_V(size, base)                                             \
        ((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_VS(size, base)                                            \
        ((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_SV(size, base)                                            \
        ((((size) & 15) << 4) | ((base) & 15))

#define SUBOP_MV(size2, size1, base)                                    \
        ((((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_VM(size2, size1, base)                                    \
        ((((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_Z(mask, size2, size1, base)                               \
        (((mask) << 16) | (((size2) & 15) << 12) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_ZM(mask, sizer, size2, size1, base)                               \
        (((mask) << 16) | (((size2) & 15) << 12) | (((sizer) & 15) << 8) | (((size1) & 15) << 4) | ((base) & 15))

#define SUBOP_CS(base2, base1)                                          \
        ((((base2) & 15) << 8) | ((base1) & 15))

#define SUBOP_CV(base2, size, base1)                                    \
        (((((base2) & 15) << 8) | ((size) & 15) << 4) | ((base1) & 15))

#define SUBOP_CM(size2, base2, size1, base1)                            \
        ((((size2) & 15) << 12) | (((base2) & 15) << 8) | (((size1) & 15) << 4) | ((base1) & 15))

#define SUBOP_SET_T(subop, base)  ((subop) = ((subop) & ~15) | ((base) & 15))

#define SUBOP_SET_MASK(subop, mask)  ((subop) = ((subop) & ~0x00ff0000) | (((mask) & 0xff) << 16))

#define SUBOP_KV(mask, size, base)                                      \
        ((((mask) & 15) << 16) | (((size) & 15) << 4) | ((base) & 15))

#define SUBOP_GET_T1(subop)     ((subop) & 15)
#define SUBOP_GET_S1(subop)     (((subop) >> 4) & 15)
#define SUBOP_GET_T2(subop)     (((subop) >> 8) & 15)
#define SUBOP_GET_S2(subop)     (((subop) >> 12) & 15)
#define SUBOP_GET_S(subop)      (((subop) >> 4) & 15)
#define SUBOP_GET_T(subop)      ((subop) & 15)
#define SUBOP_GET_MASK(subop)   (((subop) >> 16) & 0xff)
#define SUBOP_GET_MASK16(subop) (((subop) >> 16) & 0xffff)

#define PRAGMA_HASH_STR "#"
#define COMMENT_CPP_STR "//"

struct dtype_rec {
    // Possibly derived, stack-resident pointer to and copy of a type.
    Type *basetype;     // Pointer to non-derived
    int IsDerived;      // TRUE if anything has been altered
    int numNewDims;     // Number of new dimensions added for this declarator
    StorageClass storageClass;   // Aplied to variables when defined, not part of the type
    Type type;          // Local copy of type
};

struct decl_rec {
    // For declaration parsing
    nodekind kind;
    SourceLoc loc;      // Location for error reporting
    int name;           // Symbol name atom
    int semantics;
    dtype type;         // Type collected while parsing
    decl *next;
    Symbol *symb;       // Symbol table definition of actual object
    decl *params;       // Actual paramaters to function declaration
    expr *initexpr;     // Initializer
};

typedef struct exprhead_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];  // Used by backends
} exprhead;

struct symb_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];
    opcode op;
    Symbol *symbol;
};

typedef union scalar_constant_rec {
    float f;
    int i;
} scalar_constant;

typedef struct constant_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];
    opcode op;
    int subop;
    scalar_constant val[4];
} constant;

typedef struct unary_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];
    opcode op;
    int subop;
    expr *arg;
} unary;

typedef struct binary_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];
    opcode op;
    int subop;
    expr *left, *right;
} binary;

typedef struct trinary_rec {
    nodekind kind;
    Type *type;
    int IsLValue;
    int IsConst;
    int HasSideEffects;
    void *tempptr[4];
    opcode op;
    int subop;
    expr *arg1, *arg2, *arg3;
} trinary;

union expr_rec {
    exprhead common;
    symb sym;
    constant co;
    unary un;
    binary bin;
    trinary tri;
};


typedef struct common_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
} common_stmt;

typedef struct expr_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    expr *exp;
} expr_stmt;

typedef struct if_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    expr *cond;
    stmt *thenstmt;
    stmt *elsestmt;
} if_stmt;

typedef struct while_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    expr *cond;
    stmt *body;
} while_stmt;

typedef struct for_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    stmt *init;
    expr *cond;
    stmt *step;
    stmt *body;
} for_stmt;

typedef struct block_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    stmt *body;
} block_stmt;

typedef struct return_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    expr *exp;
} return_stmt;

typedef struct discard_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    expr *cond;
} discard_stmt;

typedef struct comment_stmt_rec {
    stmtkind kind;
    stmt *next;
    SourceLoc loc;
    int str;
} comment_stmt;

union stmt_rec {
    common_stmt commonst;
    expr_stmt exprst;
    if_stmt ifst;
    while_stmt whilest;
    for_stmt forst;
    block_stmt blockst;
    return_stmt returnst;
    discard_stmt discardst;
    comment_stmt commentst;
};

extern dtype CurrentDeclTypeSpecs;

decl *NewDeclNode(SourceLoc *loc, int atom, dtype *type);
symb *NewSymbNode(opcode op, Symbol *fSymb);
constant *NewIConstNode(opcode op, int fval, int base);
constant *NewBConstNode(opcode op, int fval, int base);
constant *NewFConstNode(opcode op, float fval, int base);
constant *NewFConstNodeV(opcode op, float *fval, int len, int base);
unary *NewUnopNode(opcode op, expr *arg);
unary *NewUnopSubNode(opcode op, int subop, expr *arg);
binary *NewBinopNode(opcode op, expr *left, expr *right);
binary *NewBinopSubNode(opcode op, int subop, expr *left, expr *right);
trinary *NewTriopNode(opcode op, expr *arg1, expr *arg2, expr *arg3);
trinary *NewTriopSubNode(opcode op, int subop, expr *arg1, expr *arg2, expr *arg3);

symb *DupSymbNode(const symb *fSymb);
constant *DupConstNode(const constant *fconst);
unary *DupUnaryNode(const unary *fun);
binary *DupBinaryNode(const binary *fbin);
trinary *DupTrinaryNode(const trinary *ftri);
expr *DupNode(const expr *fExpr);

expr_stmt *NewExprStmt(SourceLoc *loc, expr *fExpr);
if_stmt *NewIfStmt(SourceLoc *loc, expr *fExpr, stmt *thenStmt, stmt *elseStmt);
if_stmt *SetThenElseStmts(SourceLoc *loc, stmt *ifStmt, stmt *thenStmt, stmt *elseStmt);
while_stmt *NewWhileStmt(SourceLoc *loc, stmtkind kind, expr *fExpr, stmt *body);
for_stmt *NewForStmt(SourceLoc *loc, stmt *fExpr1, expr *fExpr2, stmt *fExpr3, stmt *body);
block_stmt *NewBlockStmt(SourceLoc *loc, stmt *fStmt);
return_stmt *NewReturnStmt(SourceLoc *loc, Scope *fScope, expr *fExpr);
discard_stmt *NewDiscardStmt(SourceLoc *loc, expr *fExpr);
comment_stmt *NewCommentStmt(SourceLoc *loc, const char *str);

/************************************* dtype functions: *************************************/

Type *GetTypePointer(SourceLoc *loc, const dtype *fDtype);
dtype *SetDType(dtype *fDtype, Type *fType);
dtype *NewDType(dtype *fDtype, Type *baseType, int category);

int SetTypeCategory(SourceLoc *loc, int atom, dtype *fType, int category, int Force);
int SetTypeQualifiers(SourceLoc *loc, dtype *fType, int qualifiers);
int SetTypeDomain(SourceLoc *loc, dtype *fType, int domain);
int SetTypeMisc(SourceLoc *loc, dtype *fType, int misc);
int SetTypePacked(SourceLoc *loc, dtype *fType);
int SetStorageClass(SourceLoc *loc, dtype *fType, int storage);

/********************************** Parser Semantic Rules: ***********************************/

expr *Initializer(SourceLoc *loc, expr *fExpr);
expr *InitializerList(SourceLoc *loc, expr *list, expr *fExpr);
expr *ArgumentList(SourceLoc *loc, expr *flist, expr *fExpr);
expr *ExpressionList(SourceLoc *loc, expr *flist, expr *fExpr);
decl *AddDecl(decl *first, decl *last);
stmt *AddStmt(stmt *first, stmt *last);
stmt *CheckStmt(stmt *fStmt);
decl *Function_Definition_Header(SourceLoc *loc, struct decl_rec *fDecl);
decl *Param_Init_Declarator(SourceLoc *loc, Scope *fScope, decl *fDecl, expr *fExpr);
stmt *Init_Declarator(SourceLoc *loc, Scope *fScope, decl *fDecl, expr *fExpr);
decl *Declarator(SourceLoc *loc, struct decl_rec *fDecl, int semantics);
decl *Array_Declarator(SourceLoc *loc, decl *fDecl, int size, int Empty);

Symbol *AddFormalParamDecls(Scope *fScope, decl *params);
decl *SetFunTypeParams(Scope *fScope, decl *func, decl *params, decl *actuals);
decl *FunctionDeclHeader(SourceLoc *loc, Scope *fScope, decl *func);

Type *StructHeader(SourceLoc *loc, Scope *fScope, int ctype, int tag);

Symbol *DefineVar(SourceLoc *loc, Scope *fScope, int atom, Type *fType);
Symbol *DefineTypedef(SourceLoc *loc, Scope *fScope, int atom, Type *fType);
Symbol *DeclareFunc(SourceLoc *loc, Scope *fScope, Symbol *fSymb, int atom, Type *fType,
                Scope *locals, Symbol *params);

void DefineFunction(SourceLoc *loc, Scope *fScope, struct decl_rec *func, union stmt_rec *body);
int GlobalInitStatements(Scope *fScope, union stmt_rec *fStmt);

expr *BasicVariable(SourceLoc *loc, int name);

int IsLValue(const expr *fExpr);
int IsConst(const expr *fExpr);
int IsArrayIndex(const expr *fExpr);
int ConvertType(expr *fExpr, Type *toType, Type *fromType, expr **result, int IgnorePacked,
                int Explicit);
expr *CastScalarVectorMatrix(expr *fExpr, int fbase, int tbase, int len, int len2);
int ConvertNumericOperands(int baseop, expr **lexpr, expr **rexpr, int lbase, int rbase,
                           int llen, int rlen, int llen2, int rlen2);
expr *CheckBooleanExpr(SourceLoc *loc, expr *fExpr, int AllowVector);

expr *NewUnaryOperator(SourceLoc *loc, int fop, int name, expr *fExpr, int IntegralOnly);
expr *NewBinaryOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rExpr,
                        int IntegralOnly);
expr *NewBinaryBooleanOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rExpr);
expr *NewBinaryComparisonOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rExpr);
expr *NewConditionalOperator(SourceLoc *loc, expr *bExpr, expr *lExpr, expr *rExpr);
expr *NewSwizzleOperator(SourceLoc *loc, expr *fExpr, int ident);
expr *NewMatrixSwizzleOperator(SourceLoc *loc, expr *fExpr, int ident);
expr *NewVectorConstructor(SourceLoc *loc, Type *fType, expr *fExpr);
expr *NewCastOperator(SourceLoc *loc, expr *fExpr, Type *toType);
expr *NewMemberSelectorOrSwizzleOrWriteMaskOperator(SourceLoc *loc, expr *fExpr, int ident);
expr *NewIndexOperator(SourceLoc *loc, expr *fExpr, expr *ixExpr);
expr *NewFunctionCallOperator(SourceLoc *loc, expr *funExpr, expr *actuals);

expr *NewSimpleAssignment(SourceLoc *loc, expr *fvar, expr *fExpr, int InInit);
stmt *NewSimpleAssignmentStmt(SourceLoc *loc, expr *fvar, expr *fExpr, int InInit);
stmt *NewCompoundAssignmentStmt(SourceLoc *loc, opcode op, expr *fvar, expr *fExpr);
/***
stmt *NewMaskedAssignment(SourceLoc *loc, int mask, expr *fExpr, stmt *fStmt);
stmt *NewConditionalAssignment(SourceLoc *loc, expr *fCond, expr *fExpr, stmt *fStmt);
***/

/********************************** Traversal Routeins: ******************************************/

expr *ApplyToNodes(expr *(*pre)(expr *, void *, int), expr *(*post)(expr *, void *, int),
                expr *fExpr, void *arg1, int arg2);
#define PreApplyToNodes(F, E, A1, A2) ApplyToNodes(F, 0, E, A1, A2)
#define PostApplyToNodes(F, E, A1, A2) ApplyToNodes(0, F, E, A1, A2)

void ApplyToExpressions(expr *(*pre)(expr *, void *, int), expr *(*post)(expr *, void *, int),
                stmt *fStmt, void *arg1, int arg2);
#define PreApplyToExpressions(F, E, A1, A2) ApplyToExpressions(F, 0, E, A1, A2)
#define PostApplyToExpressions(F, E, A1, A2) ApplyToExpressions(0, F, E, A1, A2)

void ApplyToExpressionsLocal(expr *(*pre)(expr *, void *, int), expr *(*post)(expr *, void *, int),
                stmt *fStmt, void *arg1, int arg2);
#define PreApplyToExpressionsLocal(F, E, A1, A2) ApplyToExpressionsLocal(F, 0, E, A1, A2)
#define PostApplyToExpressionsLocal(F, E, A1, A2) ApplyToExpressionsLocal(0, F, E, A1, A2)

void ApplyToTopExpressions(expr *(*fun)(expr *, void *, int), stmt *fStmt, void *arg1, int arg2);

stmt *ApplyToStatements(stmt *(*pre)(stmt *, void *, int), stmt *(*post)(stmt *, void *, int),
                stmt *fStmt, void *arg1, int arg2);
#define PreApplyToStatements(F, E, A1, A2) ApplyToStatements(F, 0, E, A1, A2)
#define PostApplyToStatements(F, E, A1, A2) ApplyToStatements(0, F, E, A1, A2)

void PostApplyToChildStatements(stmt *(*fun)(stmt *, void *, int), stmt *fStmt, void *arg1,
                int arg2);

/************************************** misc: ************************************************/
void InitTempptr(expr *e, void *arg1, int arg2);
/********************************** Error checking: ******************************************/

void RecordErrorPos(SourceLoc *loc);
void MarkErrorPosHit(SourceLoc *loc);
void CheckAllErrorsGenerated(void);

typedef struct ErrorLocRec
{
  // The source location at which the error token was encountered.
  SourceLoc loc;
  // 1 iff an error was found on this line or before this line after
  // the previous error token.
  int hit;
  struct ErrorLocRec * next;
} ErrorLoc;

// Beginning and end of list of error locations, sorted by line.
extern ErrorLoc * ErrorLocsFirst;
extern ErrorLoc * ErrorLocsLast;

#endif // !defined(__SUPPORT_H)
