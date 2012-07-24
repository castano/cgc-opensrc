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
// symbols.h
//

#if !defined(__SYMBOLS_H)
#define __SYMBOLS_H 1

#include "memory.h"

#define MAX_ARRAY_DIMENSIONS 3

// Type propertes:

#define TYPE_BASE_MASK              0x0000000f
#define TYPE_BASE_SHIFT             0
#define TYPE_BASE_BITS              4
#define TYPE_BASE_NO_TYPE           0x00000000 // e.g. struct or connector
#define TYPE_BASE_UNDEFINED_TYPE    0x00000001
#define TYPE_BASE_CFLOAT            0x00000002
#define TYPE_BASE_CINT              0x00000003
#define TYPE_BASE_VOID              0x00000004
#define TYPE_BASE_FLOAT             0x00000005
#define TYPE_BASE_INT               0x00000006
#define TYPE_BASE_BOOLEAN           0x00000007
#define TYPE_BASE_FIRST_USER        0x00000008
#define TYPE_BASE_LAST_USER         0x0000000f

#define TYPE_CATEGORY_MASK          0x000000f0
#define TYPE_CATEGORY_SHIFT         4
#define TYPE_CATEGORY_NONE          0x00000000
#define TYPE_CATEGORY_SCALAR        0x00000010
#define TYPE_CATEGORY_ARRAY         0x00000020
#define TYPE_CATEGORY_FUNCTION      0x00000030
#define TYPE_CATEGORY_STRUCT        0x00000040
#define TYPE_CATEGORY_CONNECTOR     0x00000050

#define TYPE_DOMAIN_MASK            0x00000f00
#define TYPE_DOMAIN_SHIFT           8
#define TYPE_DOMAIN_UNKNOWN         0x00000000
#define TYPE_DOMAIN_UNIFORM         0x00000100
#define TYPE_DOMAIN_VARYING         0x00000200

#define TYPE_QUALIFIER_MASK         0x0000f000
#define TYPE_QUALIFIER_NONE         0x00000000
#define TYPE_QUALIFIER_CONST        0x00001000
#define TYPE_QUALIFIER_IN           0x00002000
#define TYPE_QUALIFIER_OUT          0x00004000
#define TYPE_QUALIFIER_INOUT        0x00006000

// ??? Should these be called "declarator bits"???
#define TYPE_MISC_MASK              0x0ff00000
#define TYPE_MISC_TYPEDEF           0x00100000
#define TYPE_MISC_PROGRAM           0x00200000    // Type is program function
#define TYPE_MISC_ABSTRACT_PARAMS   0x00400000    // Type is function declared with abstract parameters
#define TYPE_MISC_VOID              0x00800000    // Type is void
#define TYPE_MISC_INLINE            0x01000000    // "inline" function attribute
#define TYPE_MISC_INTERNAL          0x02000000    // "__internal" function attribute
#define TYPE_MISC_PACKED            0x04000000    // For vector types like float3
#define TYPE_MISC_PACKED_KW         0x08000000    // Actual "packed" keyword used
#define TYPE_MISC_MARKED            0x10000000    // Temp value for printing types, etc.

typedef enum symbolkind {
    VARIABLE_S, TYPEDEF_S, FUNCTION_S, CONSTANT_S, TAG_S, MACRO_S,
} symbolkind;

typedef enum StrorageClass {
    SC_UNKNOWN, SC_AUTO, SC_STATIC, SC_EXTERN,
} StorageClass;

#define SYMB_IS_PARAMETER           0x000001    // Symbol is a formal parameter
#define SYMB_IS_DEFINED             0x000002    // Symbol is defined.  Currently only used for functions.
#define SYMB_IS_BUILTIN             0x000004    // Symbol is a built-in function.
#define SYMB_IS_INLINE_FUNCTION     0x000008    // Symbol is a function that will be inlined.
#define SYMB_IS_CONNECTOR_REGISTER  0x000010    // Symbol is a connector hw register
#define SYMB_CONNECTOR_CAN_READ     0x000020    // Symbol is a readable connector hw register
#define SYMB_CONNECTOR_CAN_WRITE    0x000040    // Symbol is a writable connector hw register
#define SYMB_NEEDS_BINDING          0x000080    // Symbol is a non-static global and has not yet been bound

// Typedefs for things defined in "support.h":

union stmt_rec;

// Typedefs for things defined here in "symbols.h":

typedef struct Scope_Rec Scope;
typedef struct FunSymbol_Rec FunSymbol;
typedef struct Symbol_Rec Symbol;
typedef union Type_Rec Type;
typedef struct TypeCommon_Rec TypeCommon;
typedef struct TypeScalar_Rec TypeScalar;
typedef struct TypeArray_Rec TypeArray;
typedef struct TypeStruct_Rec TypeStruct;
typedef struct TypeFunction_Rec TypeFunction;

typedef struct SymbolList_Rec {
    struct SymbolList_Rec *next;
    Symbol *symb;
} SymbolList;

struct Scope_Rec {
    Scope *next, *prev;     // doubly-linked list of all scopes
    Scope *parent;
    Scope *funScope;        // Points to base scope of enclosing function
    MemoryPool *pool;       // pool used for allocation in this scope
    Symbol *symbols;
    Symbol *tags;
    Symbol *params;
    Type *returnType;
    int level;              // 0 = super globals, 1 = globals, etc.
    int funindex;           // Identifies which function contains scope
    int InFormalParameters; // > 0 when parsing formal parameters.
    int HasVoidParameter;
    int HasReturnStmt;
    int IsStructScope;
    int HasSemantics;       // Struct scope has members with semantics
    int pid;                // Program type id
    // Only used at global scope (level 1):
    SymbolList *programs;   // List of programs for this compilation.
    union stmt_rec *initStmts;        // Global initialization statements.
};

typedef struct TypeList_Rec {
    struct TypeList_Rec *next;
    Type *type;
} TypeList;

struct TypeCommon_Rec {
    int properties;
    int size;
};

struct TypeScalar_Rec {
    int properties;
    int size;
};

struct TypeArray_Rec {
    int properties;
    int size;
    Type *eltype;
    int numels;
};

struct TypeStruct_Rec { // for structs and connectors
    int properties;
    int size;
    Type *unqualifiedtype;
    Scope *members;
    SourceLoc loc;
    int tag;          // struct or connector tag
    int semantics;
    int variety;      // connector variety
    int HasSemantics; // set if any members have semantics
    char *allocated;  // set if corresponding register has been bound
    int csize;
    void *tempptr;    // temp for FP30 backend connectors: dagnode* to DOP_VARYING
};

struct TypeFunction_Rec {
    int properties;
    int size;
    Type *rettype;
    TypeList *paramtypes;
};

union Type_Rec {
    int properties;
    TypeCommon co;
    TypeScalar sc;
    TypeArray arr;
    TypeStruct str;
    TypeFunction fun;
};

// Symbol table is a simple binary tree.

struct FunSymbol_Rec {
    Scope *locals;
    Symbol *params;
    union stmt_rec *statements;
    Symbol *overload;   // List of overloaded versions of this function
    int flags;          // Used when resolving overloaded reference
    short group;        // Built-in function group
    short index;        // Built-in function index
    char HasOutParams;
};

typedef struct VarSymbol_Rec {
    int addr;           // Address or member offset
    int semantics;
    Binding *bind;
    union expr_rec *init;   // For initialized non-static globals
} VarSymbol;

typedef struct ConstSymbol_Rec {
    int value;          // Constant value: 0 = false, 1 = true
} ConstSymbol;

#include "cpp.h"        // to get MacroSymbol def

struct Symbol_Rec {
    Symbol *left, *right;
    Symbol *next;
    int name;       // Name atom
    Type *type;     // Type descriptor
    SourceLoc loc;
    symbolkind kind;
    int properties; // Symbol properties
    StorageClass storageClass;
    int flags;      // Temp for various semantic uses
    void *tempptr;  // DAG rewrite temp: expr*, for SSA expr. that writes this sym
    void *tempptr2; // DAG rewrite temp: dagnode* to DOP_UNIFORM, for input var
    union {
        FunSymbol fun;
        VarSymbol var;
        ConstSymbol con;
        MacroSymbol mac;
    } details;
};

extern Scope *CurrentScope;
extern Scope *GlobalScope;
extern Scope *ScopeList;
extern int NextFunctionIndex;

extern Type *UndefinedType;
extern Type *CFloatType;
extern Type *CIntType;
extern Type *IntType;
extern Type *FloatType;
extern Type *VoidType;
extern Type *BooleanType;

extern Type *Float2Type;
extern Type *Float3Type;
extern Type *Float4Type;

extern Type *CFloat2Type;
extern Type *CFloat3Type;
extern Type *CFloat4Type;

extern Type *CInt2Type;
extern Type *CInt3Type;
extern Type *CInt4Type;

extern Type *Float2Type;
extern Type *Float3Type;
extern Type *Float4Type;

extern Type *Int2Type;
extern Type *Int3Type;
extern Type *Int4Type;

extern Type *Boolean2Type;
extern Type *Boolean3Type;
extern Type *Boolean4Type;

void SetScalarTypeName(int base, int name, Type *fType);

int InitSymbolTable(CgStruct *Cg);
int StartGlobalScope(CgStruct *Cg);
int FreeSymbolTable(CgStruct *Cg);
Scope *NewScopeInPool(MemoryPool *);
#define NewScope()      NewScopeInPool(CurrentScope->pool)
void PushScope(Scope *fScope);
Scope *PopScope(void);
Symbol *NewSymbol(SourceLoc *loc, Scope *fScope, int name, Type *fType, symbolkind kind);
Symbol *AddSymbol(SourceLoc *loc, Scope *fScope, int atom, Type *fType, symbolkind kind);
Symbol *UniqueSymbol(Scope *fScope, Type *fType, symbolkind kind);
Symbol *AddTag(SourceLoc *loc, Scope *fScope, int atom, int category);
Symbol *LookUpLocalSymbol(Scope *fScope, int atom);
Symbol *LookUpLocalSymbolBySemanticName(Scope *fScope, int atom);
Symbol *LookUpLocalSymbolByBindingName(Scope *fScope, int atom);
Symbol *LookUpLocalTag(Scope *fScope, int atom);
Symbol *LookUpSymbol(Scope *fScope, int atom);
Symbol *LookUpTag(Scope *fScope, int atom);
Type *LookUpTypeSymbol(Scope *fScope, int atom);

void InitType(Type *fType);

Type *NewType(int properties, int size);
Type *DupType(Type *fType);
Type *NewPackedArrayType(Type *elType, int numels, int properties);

int IsCategory(const Type *fType, int category);
int IsTypeBase(const Type *fType, int base);
int IsVoid(const Type *fType);
int IsBoolean(const Type *fType);
int IsArray(const Type *fType);
int IsScalar(const Type *fType);
int IsVector(const Type *fType, int *len);
int IsMatrix(const Type *fType, int *len, int *len2);
int IsUnsizedArray(const Type *fType);
int IsStruct(const Type *fType);
int IsProgram(const Type *fType);
int IsPacked(const Type *fType);
int IsSameUnqualifiedType(const Type *aType, const Type *bType);
int IsTypedef(const Symbol *fSymb);
int IsFunction(const Symbol *fSymb);
int IsInline(const Symbol *fSymb);
int GetBase(const Type *fType);
int GetCategory(const Type *fType);
int GetDomain(const Type *fType);
int GetQualifiers(const Type *fType);
int GetQuadRegSize(const Type *fType);
void ClearTypeMisc(Type *fType, int misc);

Type *GetStandardType(int tbase, int tlen, int tlen2);
Type *GetElementType(const Type *fType);

void SetStructMemberOffsets(Type *fType);
Type *SetStructMembers(SourceLoc *loc, Type *fType, Scope *members);

void AddParameter(Scope *fScope, Symbol *param);

int GetSwizzleOrWriteMask(SourceLoc *loc, int atom, int *FIsLValue, int *flen);
int GetMatrixSwizzleOrWriteMask(SourceLoc *loc, int atom, int *FIsLValue, int *flen);
const char *GetBaseTypeNameString(int base);

void ClearAllSymbolTempptr(void);
void ClearAllSymbolTempptr2(void);

#endif // !defined(__SYMBOLS_H)

