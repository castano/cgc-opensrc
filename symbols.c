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
// symbols.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Symbol Table Variables: ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

Scope *ScopeList = NULL;
Scope *CurrentScope = NULL;
Scope *GlobalScope = NULL;
int NextFunctionIndex = 0;

Type *UndefinedType = NULL;
Type *CFloatType = NULL;
Type *CIntType = NULL;
Type *VoidType = NULL;
Type *FloatType = NULL;
Type *IntType = NULL;
Type *BooleanType = NULL;

Type *CFloat1Type = NULL;
Type *CFloat2Type = NULL;
Type *CFloat3Type = NULL;
Type *CFloat4Type = NULL;

Type *CInt1Type = NULL;
Type *CInt2Type = NULL;
Type *CInt3Type = NULL;
Type *CInt4Type = NULL;

Type *Float1Type = NULL;
Type *Float2Type = NULL;
Type *Float3Type = NULL;
Type *Float4Type = NULL;

Type *Int1Type = NULL;
Type *Int2Type = NULL;
Type *Int3Type = NULL;
Type *Int4Type = NULL;

Type *Boolean1Type = NULL;
Type *Boolean2Type = NULL;
Type *Boolean3Type = NULL;
Type *Boolean4Type = NULL;

Symbol *FalseSymb = NULL;
Symbol *TrueSymb = NULL;

static int baseTypeNames[TYPE_BASE_LAST_USER + 1] = { 0 };
static Type *baseTypes[TYPE_BASE_LAST_USER + 1] = { NULL };

/************************************ Type Name Error Support ********************************/

/*
 * SetScalarTypeName() - Set a scalar type name.
 *
 */

void SetScalarTypeName(int base, int name, Type *fType)
{
    if (base >= 0 && base <= TYPE_BASE_LAST_USER) {
        baseTypeNames[base] = name;
        baseTypes[base] = fType;
    }
} // SetScalarTypeName

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Symbol Table Fuctions: ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * InitSymbolTable()
 *
 */

int InitSymbolTable(CgStruct *Cg)
{
    SourceLoc dummyLoc = { 0, 0 };
    int ii, name;

    // Create the super-global scope and add predefined types and symbols:

    PushScope(NewScopeInPool(mem_CreatePool(0, 0)));
    UndefinedType = NewType(TYPE_BASE_UNDEFINED_TYPE | TYPE_CATEGORY_SCALAR, 0);
    CFloatType = NewType(TYPE_BASE_CFLOAT | TYPE_CATEGORY_SCALAR | TYPE_QUALIFIER_CONST, 1);
    CIntType = NewType(TYPE_BASE_CINT | TYPE_CATEGORY_SCALAR | TYPE_QUALIFIER_CONST, 1);
    VoidType = NewType(TYPE_BASE_VOID | TYPE_CATEGORY_SCALAR | TYPE_MISC_VOID, 0);
    FloatType = NewType(TYPE_BASE_FLOAT | TYPE_CATEGORY_SCALAR, 1);
    IntType = NewType(TYPE_BASE_INT | TYPE_CATEGORY_SCALAR, 1);
    BooleanType = NewType(TYPE_BASE_BOOLEAN | TYPE_CATEGORY_SCALAR, 1);

    CFloat1Type = NewPackedArrayType(CFloatType, 1, TYPE_QUALIFIER_CONST);
    CFloat2Type = NewPackedArrayType(CFloatType, 2, TYPE_QUALIFIER_CONST);
    CFloat3Type = NewPackedArrayType(CFloatType, 3, TYPE_QUALIFIER_CONST);
    CFloat4Type = NewPackedArrayType(CFloatType, 4, TYPE_QUALIFIER_CONST);
    CInt1Type = NewPackedArrayType(CIntType, 1, TYPE_QUALIFIER_CONST);
    CInt2Type = NewPackedArrayType(CIntType, 2, TYPE_QUALIFIER_CONST);
    CInt3Type = NewPackedArrayType(CIntType, 3, TYPE_QUALIFIER_CONST);
    CInt4Type = NewPackedArrayType(CIntType, 4, TYPE_QUALIFIER_CONST);
    Float1Type = NewPackedArrayType(FloatType, 1, 0);
    Float2Type = NewPackedArrayType(FloatType, 2, 0);
    Float3Type = NewPackedArrayType(FloatType, 3, 0);
    Float4Type = NewPackedArrayType(FloatType, 4, 0);
    Int1Type = NewPackedArrayType(IntType, 1, 0);
    Int2Type = NewPackedArrayType(IntType, 2, 0);
    Int3Type = NewPackedArrayType(IntType, 3, 0);
    Int4Type = NewPackedArrayType(IntType, 4, 0);
    Boolean1Type = NewPackedArrayType(BooleanType, 1, 0);
    Boolean2Type = NewPackedArrayType(BooleanType, 2, 0);
    Boolean3Type = NewPackedArrayType(BooleanType, 3, 0);
    Boolean4Type = NewPackedArrayType(BooleanType, 4, 0);

    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cfloat"), CFloatType, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cint"), CIntType, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, VOID_SY, VoidType, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, FLOAT_SY, FloatType, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, INT_SY, IntType, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, BOOLEAN_SY, BooleanType, TYPEDEF_S);

    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cfloat1"), CFloat1Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cfloat2"), CFloat2Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cfloat3"), CFloat3Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cfloat4"), CFloat4Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cint1"), CInt1Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cint2"), CInt2Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cint3"), CInt3Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "cint4"), CInt4Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "float1"), Float1Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "float2"), Float2Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "float3"), Float3Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "float4"), Float4Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "int1"), Int1Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "int2"), Int2Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "int3"), Int3Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "int4"), Int4Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "bool1"), Boolean1Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "bool2"), Boolean2Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "bool3"), Boolean3Type, TYPEDEF_S);
    AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "bool4"), Boolean4Type, TYPEDEF_S);

    FalseSymb = AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "false"), BooleanType, CONSTANT_S);
    TrueSymb = AddSymbol(&dummyLoc, CurrentScope, LookUpAddString(atable, "true"), BooleanType, CONSTANT_S);
    FalseSymb->details.con.value = 0;
    TrueSymb->details.con.value = 1;

    SetScalarTypeName(TYPE_BASE_NO_TYPE, LookUpAddString(atable, "***no-base-type***"), UndefinedType);
    SetScalarTypeName(TYPE_BASE_UNDEFINED_TYPE, LookUpAddString(atable, "***undefined-base-type***"), UndefinedType);
    SetScalarTypeName(TYPE_BASE_CFLOAT, LookUpAddString(atable, "cfloat"), CFloatType);
    SetScalarTypeName(TYPE_BASE_CINT, LookUpAddString(atable, "cint"), CIntType);
    SetScalarTypeName(TYPE_BASE_VOID, LookUpAddString(atable, "void"), VoidType);
    SetScalarTypeName(TYPE_BASE_FLOAT, LookUpAddString(atable, "float"), FloatType);
    SetScalarTypeName(TYPE_BASE_INT, LookUpAddString(atable, "int"), IntType);
    SetScalarTypeName(TYPE_BASE_BOOLEAN, LookUpAddString(atable, "bool"), BooleanType);

    name = LookUpAddString(atable, "***unknown-profile-base-type***");
    for (ii = TYPE_BASE_FIRST_USER; ii <= TYPE_BASE_LAST_USER; ii++)
        SetScalarTypeName(ii, name, UndefinedType);

    // Add profile specific symbols and types:

    Cg->theHAL->RegisterNames(Cg->theHAL);
    AddAtom(atable, "<*** end hal specific atoms ***>");

    // Initialize misc. other globals:

    CurrentDeclTypeSpecs.basetype = UndefinedType;
    CurrentDeclTypeSpecs.IsDerived = 0;
    CurrentDeclTypeSpecs.type = *UndefinedType;

    return 1;
} // InitSymbolTable

int StartGlobalScope(CgStruct *Cg)
{
    // Create user's global scope:
    GlobalScope = NewScopeInPool(mem_CreatePool(0, 0));
    PushScope(GlobalScope);
    return 1;
} // StartGlobalScope

/*
 * FreeSymbolTable()
 *
 */

int FreeSymbolTable(CgStruct *Cg)
{
    Scope *lScope, *nScope;

    lScope = ScopeList;
    while (lScope) {
        nScope = lScope->next;
        // FreeEverythingOwnedByScope(pScope);
        lScope = nScope;
    }
    return 1;
} // FreeSymbolTable

static void unlinkScope(void *_scope) {
    Scope *scope = _scope;

    if (scope->next)
        scope->next->prev = scope->prev;
    if (scope->prev)
        scope->prev->next = scope->next;
    else
        ScopeList = scope->next;
}

/*
 * NewScope()
 *
 */
Scope *NewScopeInPool(MemoryPool *pool)
{
    Scope *lScope;

    lScope = mem_Alloc(pool, sizeof(Scope));
    lScope->pool = pool;
    lScope->parent = NULL;
    lScope->funScope = NULL;
    lScope->symbols = NULL;
    lScope->tags = NULL;
    lScope->params = NULL;
    lScope->returnType = NULL;
    lScope->level = 0;
    lScope->funindex = 0;
    lScope->InFormalParameters = 0;
    lScope->HasVoidParameter = 0;
    lScope->HasReturnStmt = 0;
    lScope->IsStructScope = 0;
    lScope->HasSemantics = 0;
    lScope->pid = PID_NONE_ID;
    lScope->programs = NULL;
    lScope->initStmts = NULL;
    if ((lScope->next = ScopeList))
        ScopeList->prev = lScope;
    lScope->prev = 0;
    ScopeList = lScope;
    mem_AddCleanup(pool, unlinkScope, lScope);
    return lScope;
} // NewScope

/*
 * PushScope()
 *
 */

void PushScope(Scope *fScope)
{
    Scope *lScope;

    if (CurrentScope) {
        fScope->level = CurrentScope->level + 1;
        if (fScope->level == 1) {
            if (!GlobalScope) {
                /* HACK - CTD -- if GlobalScope==NULL and level==1, we're
                 * defining a function in the superglobal scope.  Things
                 * will break if we leave the level as 1, so we arbitrarily
                 * set it to 2 */
                fScope->level = 2;
            }
        }
        if (fScope->level >= 2) {
            lScope = fScope;
            while (lScope->level > 2)
                lScope = lScope->next;
            fScope->funScope = lScope;
        }
    } else {
        fScope->level = 0;
    }
    fScope->parent = CurrentScope;
    CurrentScope = fScope;
} // PushScope

/*
 * PopScope()
 *
 */

Scope *PopScope(void)
{
    Scope *lScope;

    lScope = CurrentScope;
    if (CurrentScope)
        CurrentScope = CurrentScope->parent;
    return lScope;
} // PopScope

/*
 * NewSymbol() - Allocate a new symbol node;
 *
 */

Symbol *NewSymbol(SourceLoc *loc, Scope *fScope, int name, Type *fType, symbolkind kind)
{
    Symbol *lSymb;
    char *pch;
    int ii;

    lSymb = (Symbol *) mem_Alloc(fScope->pool, sizeof(Symbol));
    lSymb->left = NULL;
    lSymb->right = NULL;
    lSymb->next = NULL;
    lSymb->name = name;
    lSymb->storageClass = SC_UNKNOWN;
    lSymb->type = fType;
    lSymb->loc = *loc;
    lSymb->kind = kind;
    lSymb->properties = 0;
    lSymb->flags = 0;
    lSymb->tempptr = NULL;
    lSymb->tempptr2 = NULL;
    
    // Clear union area:

    pch = (char *) &lSymb->details;
    for (ii = 0; ii < sizeof(lSymb->details); ii++)
        *pch++ = 0;
    return lSymb;
} // NewSymbol

/*
 * lAddToTree() - Using a binary tree is not a good idea for basic atom values because they
 *         are generated in order.  We'll fix this later (by reversing the bit pattern).
 */

static void lAddToTree(Symbol **fSymbols, Symbol *fSymb, Type *fType)
{
    Symbol *lSymb;
    int lrev, frev;

    lSymb = *fSymbols;
    if (lSymb) {
        frev = GetReversedAtom(atable, fSymb->name);
        while (lSymb) {
            lrev = GetReversedAtom(atable, lSymb->name);
            if (lrev == frev) {
                InternalError(Cg->tokenLoc, 9999, "symbol \"%s\" already in table",
                       GetAtomString(atable, fSymb->name));
                break;
            } else {
                if (lrev > frev) {
                    if (lSymb->left) {
                        lSymb = lSymb->left;
                    } else {
                        lSymb->left = fSymb;
                        break;
                    }
                } else {
                    if (lSymb->right) {
                        lSymb = lSymb->right;
                    } else {
                        lSymb->right = fSymb;
                        break;
                    }
                }
            }
        }
    } else {
        *fSymbols = fSymb;
    }
} // lAddToTree


/*
 * AddSymbol() - Add a variable, type, or function name to a scope.
 *
 */

Symbol *AddSymbol(SourceLoc *loc, Scope *fScope, int atom, Type *fType, symbolkind kind)
{
    Symbol *lSymb;

    if (!fScope)
        fScope = CurrentScope;
    lSymb = NewSymbol(loc, fScope, atom, fType, kind);
    lAddToTree(&fScope->symbols, lSymb, fType);
    return lSymb;
} // AddSymbol

/*
 * UniqueSymbol() - Add a symbol to fScope that is different from
 * every other symbol.  Useful for compiler generated temporaries.
 *
 */

Symbol *UniqueSymbol(Scope *fScope, Type *fType, symbolkind kind)
{
    static int nextTmp = 0;
    static SourceLoc tmpLoc = { 0, 0 };
    char buf[256];
    int atom;
  
    sprintf(buf, "@TMP%d", nextTmp++);
    atom = AddAtom(atable, buf);
    return AddSymbol(&tmpLoc, fScope, atom, fType, kind);
} // UniqueSymbol


/*
 * AddTag() - Add a tag name to a scope.
 *
 */

Symbol *AddTag(SourceLoc *loc, Scope *fScope, int atom, int category)
{
    Symbol *lSymb;
    Type *pType;

    if (!fScope)
        fScope = CurrentScope;
    pType = NewType(category, 0);
    pType->str.unqualifiedtype = pType;
    lSymb = NewSymbol(loc, fScope, atom, pType, TAG_S);
    lAddToTree(&fScope->tags, lSymb, pType);
    return lSymb;
} // AddTag

/*********************************************************************************************/
/***************************************** Type Functions ************************************/
/*********************************************************************************************/

/*
 * InitType() - Initialize a type struct.
 *
 */

void InitType(Type *fType)
{
    char *c = (char *) fType;
    int ii;

    for (ii = 0; ii < sizeof(Type); ii++)
        *c++ = 0;
} // InitType

/*
 * NewType() - Allocate a new type struct.
 *
 */

Type *NewType(int properties, int size)
{
    Type *lType;

    lType = (Type *) malloc(sizeof(Type));
    InitType(lType);
    lType->properties = properties;
    lType->co.size = size;
    return lType;
} // NewType

/*
 * DupType() - Duplicate a type struct.
 *
 */

Type *DupType(Type *fType)
{
    Type *lType;

    lType = (Type *) malloc(sizeof(Type));
    *lType = *fType;
    return lType;
} // DupType

/*
 * NewPackedArrayType() - Define a new packed (vector) array type.
 *
 */

Type *NewPackedArrayType(Type *elType, int numels, int properties)
{
    Type *lType;

    lType = NewType(TYPE_CATEGORY_ARRAY | TYPE_MISC_PACKED | properties | GetBase(elType), 0);
    lType->arr.eltype = elType;
    lType->arr.numels = numels;
    lType->arr.size = Cg->theHAL->GetSizeof(lType);
    return lType;
} // NewPakedArrayType

/*************************************** Category Functions **********************************/

/*
 * IsCategory() - See if a type is of the given category.
 *
 */

int IsCategory(const Type *fType, int category)
{
    if (fType && (fType->properties & TYPE_CATEGORY_MASK) == category) {
        return 1;
    } else {
        return 0;
    }
} // IsCategory

/*
 * IsTypeBase() - See if a type is of the given base.
 *
 */

int IsTypeBase(const Type *fType, int base)
{
    if (fType && (fType->properties & TYPE_BASE_MASK) == base) {
        return 1;
    } else {
        return 0;
    }
} // IsTypeBase

/*
 * IsVoid() - Returns TRUE if a void type.
 *
 */

int IsVoid(const Type *fType)
{
    if (fType && (fType->properties & TYPE_MISC_VOID)) {
        return 1;
    } else {
        return 0;
    }
} // IsVoid

/*
 * IsBoolean() - Returns TRUE if a Boolean type.
 *
 */

int IsBoolean(const Type *fType)
{
    if (fType && (fType->properties & TYPE_BASE_MASK) == TYPE_BASE_BOOLEAN) {
        return 1;
    } else {
        return 0;
    }
} // IsBoolean

/*
 * IsScalar() - Returns TRUE if a scalar type.
 *
 */

int IsScalar(const Type *fType)
{
    if (fType && (fType->properties & TYPE_CATEGORY_MASK) == TYPE_CATEGORY_SCALAR) {
        return 1;
    } else {
        return 0;
    }
} // IsScalar

/*
 * IsArray() - Returns TRUE if a packed or unpacked array type.
 *
 */

int IsArray(const Type *fType)
{
    if (fType && (fType->properties & TYPE_CATEGORY_MASK) == TYPE_CATEGORY_ARRAY) {
        return 1;
    } else {
        return 0;
    }
} // IsScalar

/*
 * IsVector() - Returns TRUE if a vector type.
 *
 */

int IsVector(const Type *fType, int *len)
{
    if (fType &&
        (fType->properties & TYPE_CATEGORY_MASK) == TYPE_CATEGORY_ARRAY &&
        (fType->properties & TYPE_MISC_PACKED) &&
        !IsArray(fType->arr.eltype))
    {
        if (len)
            *len = fType->arr.numels;
        return 1;
    } else {
        return 0;
    }
} // IsVector

/*
 * IsMatrix() - Returns TRUE if a matrix type.
 *
 */

int IsMatrix(const Type *fType, int *len, int *len2)
{
    if (fType &&
        (fType->properties & TYPE_CATEGORY_MASK) == TYPE_CATEGORY_ARRAY &&
        (fType->properties & TYPE_MISC_PACKED) &&
        IsVector(fType->arr.eltype, len))
    {
        if (len2)
            *len2 = fType->arr.numels;
        return 1;
    } else {
        return 0;
    }
} // IsMatrix

/*
 * IsUnsizedArray() - Returns TRUE if an array with an unspecified number of elements.
 *
 */

int IsUnsizedArray(const Type *fType)
{
    if (GetCategory(fType) == TYPE_CATEGORY_ARRAY &&
        fType->arr.numels == 0)
    {
        return 1;
    } else {
        return 0;
    }
} // IsUnsizedArray

/*
 * IsStruct() - Returns TRUE if a struct.
 *
 */

int IsStruct(const Type *fType)
{
    if (fType &&
        GetCategory(fType) == TYPE_CATEGORY_STRUCT)
    {
        return 1;
    } else {
        return 0;
    }
} // IsStruct

/*
 * IsProgram() - See if a type is a program.
 *
 */

int IsProgram(const Type *fType)
{
    if (fType && (fType->properties & TYPE_MISC_PROGRAM)) {
        return 1;
    } else {
        return 0;
    }
} // IsProgram

/*
 * IsPacked()
 *
 */

int IsPacked(const Type *fType)
{
    if (fType && (fType->properties & TYPE_MISC_PACKED)) {
        return 1;
    } else {
        return 0;
    }
} // IsPacked

/*
 * IsSameUnqualifiedType() - Returns TRUE if the unqualified types aType and bType are the same.
 *
 */

int IsSameUnqualifiedType(const Type *aType, const Type *bType)
{
    const int UnQMask = TYPE_BASE_MASK | TYPE_CATEGORY_MASK ; // 020122 // | TYPE_DOMAIN_MASK;

    if (aType == bType) {
        return 1;
    } else {
        if ((aType->properties & UnQMask) == (bType->properties & UnQMask)) {
            switch (aType->properties & TYPE_CATEGORY_MASK) {
            case TYPE_CATEGORY_SCALAR:
                return 1;
            case TYPE_CATEGORY_ARRAY:
                if (aType->arr.numels == bType->arr.numels) {
                    // Should we check for Packed here??? I think so!
                    return IsSameUnqualifiedType(aType->arr.eltype, bType->arr.eltype);
                }
                break;
            case TYPE_CATEGORY_FUNCTION:
                break;
            case TYPE_CATEGORY_STRUCT:
                if (aType->str.unqualifiedtype == bType->str.unqualifiedtype)
                    return 1;
            default:
                break;
            }
        }
    }
    return 0;
} // IsSameUnqualifiedType

/*
 * IsTypedef() - See if a symbol is a typedef.
 *
 */

int IsTypedef(const Symbol *fSymb)
{
    if (fSymb && fSymb->kind == TYPEDEF_S) {
        return 1;
    } else {
        return 0;
    }
} // IsTypedef

/*
 * IsFunction() - See if a symbol is a function.
 *
 */

int IsFunction(const Symbol *fSymb)
{
    if (fSymb && fSymb->kind == FUNCTION_S) {
        return 1;
    } else {
        return 0;
    }
} // IsFunction

/*
 * IsInline() - See if a symbol is an inline function.
 *
 */

int IsInline(const Symbol *fSymb)
{
    if (fSymb && fSymb->kind == FUNCTION_S && (fSymb->properties & SYMB_IS_INLINE_FUNCTION)) {
        return 1;
    } else {
        return 0;
    }
} // IsInline

/*
 * GetBase() - Return the base attributes of a type.
 *
 */

int GetBase(const Type *fType)
{
    if (fType) {
        return fType->properties & TYPE_BASE_MASK;
    } else {
        return TYPE_BASE_NO_TYPE;
    }
} // GetBase

/*
 * GetCategory() - Return the categpry of a type.
 *
 */

int GetCategory(const Type *fType)
{
    if (fType) {
        return fType->properties & TYPE_CATEGORY_MASK;
    } else {
        return TYPE_CATEGORY_NONE;
    }
} // GetCategory

/*
 * GetDomain() - Return the domain of a type.
 *
 */

int GetDomain(const Type *fType)
{
    if (fType) {
        return fType->properties & TYPE_DOMAIN_MASK;
    } else {
        return TYPE_DOMAIN_UNKNOWN;
    }
} // GetDomain

/*
 * GetQualifiers() - Return a type's qualifiers.
 *
 */

int GetQualifiers(const Type *fType)
{
    if (fType) {
        return fType->properties & TYPE_QUALIFIER_MASK;
    } else {
        return TYPE_QUALIFIER_NONE;
    }
} // GetQualifiers

/*
 * GetQuadRegSize() - Return the number of quad registers required to hold an object
 *         of this type.  Minimum size is 1.
 */

int GetQuadRegSize(const Type *fType)
{
    if (fType) {
        return (fType->co.size + 3) >> 2;
    } else {
        return 1;
    }
} // GetQuadRegSize

/*
 * ClearTypeMisc() - Clear bits in the properties field a type.
 *
 */

void ClearTypeMisc(Type *fType, int misc)
{
    if (fType)
        fType->properties &= ~misc;
} // ClearTypeMisc

/*********************************************************************************************/
/************************************ Symbol Semantic Functions ******************************/
/*********************************************************************************************/

/*
 * LookUpLocalSymbol()
 *
 */

Symbol *LookUpLocalSymbol(Scope *fScope, int atom)
{
    Symbol *lSymb;
    int rname, ratom;

    ratom = GetReversedAtom(atable, atom);
    if (!fScope)
        fScope = CurrentScope;
    lSymb = fScope->symbols;
    while (lSymb) {
        rname = GetReversedAtom(atable, lSymb->name);
        if (rname == ratom) {
            return lSymb;
        } else {
            if (rname > ratom) {
                lSymb = lSymb->left;
            } else {
                lSymb = lSymb->right;
            }
        }
    }
    return NULL;
} // LookUpLocalSymbol

/*
 * LookUpLocalSymbolBySemanticName() - Lookup a symbol in a local tree by the : semantic name.
 *
 * Note:  The tree is not ordered for this lookup so the next field is used.  This only works
 * for structs and other scopes that maintain this list.
 *
 * Note: There can be multiple matches.  Returns the first.
 *
 */

Symbol *LookUpLocalSymbolBySemanticName(Scope *fScope, int atom)
{
    Symbol *lSymb;

    if (!fScope)
        return NULL;
    lSymb = fScope->symbols;
    while (lSymb) {
        if (lSymb->kind == VARIABLE_S) {
            if (lSymb->details.var.semantics == atom)
                return lSymb;
        }
        lSymb = lSymb->next;
    }
    return NULL;
} // LookUpLocalSymbolBySemanticName

/*
 * LookUpLocalSymbolByBindingName() - Lookup a symbol in a local tree by the lname in the
 *         semantic binding structure.
 *
 * Note:  The tree is not ordered for this lookup so the next field is used.  This only works
 * for structs and other scopes that maintain this list.
 *
 * Note: There can be multiple matches.  Returns the first.
 *
 */

Symbol *LookUpLocalSymbolByBindingName(Scope *fScope, int atom)
{
    Symbol *lSymb;

    if (!fScope)
        return NULL;
    lSymb = fScope->symbols;
    while (lSymb) {
        if (lSymb->kind == VARIABLE_S && lSymb->details.var.bind) {
            if (lSymb->details.var.bind->none.lname == atom)
                return lSymb;
        }
        lSymb = lSymb->next;
    }
    return NULL;
} // LookUpLocalSymbolByBindingName

/*
 * LookUpLocalTag()
 *
 */

Symbol *LookUpLocalTag(Scope *fScope, int atom)
{
    Symbol *lSymb;
    int rname, ratom;

    ratom = GetReversedAtom(atable, atom);
    if (!fScope)
        fScope = CurrentScope;
    lSymb = fScope->tags;
    while (lSymb) {
        rname = GetReversedAtom(atable, lSymb->name);
        if (rname == ratom) {
            return lSymb;
        } else {
            if (rname > ratom) {
                lSymb = lSymb->left;
            } else {
                lSymb = lSymb->right;
            }
        }
    }
    return NULL;
} // LookUpLocalTag

/*
 * LookUpSymbol()
 *
 */

Symbol *LookUpSymbol(Scope *fScope, int atom)
{
    Symbol *lSymb;

    if (!fScope)
        fScope = CurrentScope;
    while (fScope) {
        lSymb = LookUpLocalSymbol(fScope, atom);
        if (lSymb)
            return lSymb;
        fScope = fScope->parent;
    }
    return NULL;
} // LookUpSymbol

/*
 * LookUpTag()
 *
 */

Symbol *LookUpTag(Scope *fScope, int atom)
{
    Symbol *lSymb;

    if (!fScope)
        fScope = CurrentScope;
    while (fScope) {
        lSymb = LookUpLocalTag(fScope, atom);
        if (lSymb)
            return lSymb;
        fScope = fScope->parent;
    }
    return NULL;
} // LookUpTag

/*
 * LookUpTypeSymbol()
 *
 */

Type *LookUpTypeSymbol(Scope *fScope, int atom)
{
    Symbol *lSymb;
    Type *lType;

    lSymb = LookUpSymbol(fScope, atom);
    if (lSymb) {
        if (!IsTypedef(lSymb)) {
            InternalError(Cg->tokenLoc, ERROR_S_NAME_NOT_A_TYPE,
                          GetAtomString(atable, atom));
            return UndefinedType;
        }
        lType = lSymb->type;
        if (lType) {
            return lType;
        } else {
            return UndefinedType;
        }
    } else {
        InternalError(Cg->tokenLoc, ERROR_S_TYPE_NAME_NOT_FOUND,
                      GetAtomString(atable, atom));
        return UndefinedType;
    }
} // LookUpTypeSymbol

/*
 * GetStandardType()
 *
 * Scalar: len = 0.
 * Vector: len >= 1 and len2 = 0
 * Matrix: len >= 1 and len2 >= 1
 *
 * len = 1 means "float f[1]" not "float f"
 * Vector and matrix types are PACKED.
 *
 */

Type *GetStandardType(int tbase, int tlen, int tlen2)
{
    Type *lType, *nType;

    if (tbase >= 0 && tbase <= TYPE_BASE_LAST_USER) {
        lType = baseTypes[tbase];
        if (tlen > 0) {
            // Put these in a table, too!!! XYZZY !!!
            nType = NewType(TYPE_CATEGORY_ARRAY | TYPE_MISC_PACKED | tbase, 0);
            nType->arr.eltype = lType;
            nType->arr.numels = tlen;
            nType->arr.size = Cg->theHAL->GetSizeof(nType);
            lType = nType;
            if (tlen2 > 0) {
                // Put these in a table, too!!! XYZZY !!!
                nType = NewType(TYPE_CATEGORY_ARRAY | TYPE_MISC_PACKED | tbase, 0);
                nType->arr.eltype = lType;
                nType->arr.numels = tlen2;
                nType->arr.size = Cg->theHAL->GetSizeof(nType);
                lType = nType;
            }
        }
    } else {
        lType = UndefinedType;
    }
    return lType;
} // GetStandardType

/*
 * GetElementType() - Return a pointer to the type of elements stored in this array.
 *
 */

Type *GetElementType(const Type *fType)
{
    Type *lType;

    if (GetCategory(fType) == TYPE_CATEGORY_ARRAY) {
        lType = fType->arr.eltype;
#if 0000
        if ((fType->properties & TYPE_QUALIFIER_CONST) &&
            !(lType->properties & TYPE_QUALIFIER_CONST))
        {
            lType = DupType(lType);
            lType->properties |= TYPE_QUALIFIER_CONST;
        }
#endif
    } else {
        InternalError(Cg->tokenLoc, ERROR___TYPE_NOT_ARRAY);
        lType = UndefinedType;
    }
    return lType;
} // GetElementType

/*
 * SetMemberOffsets() - Assign offsets to members for use by code generators.
 *
 */

void SetStructMemberOffsets(Type *fType)
{
    int addr, size, alignment;
    Symbol *lSymb;

    addr = 0;
    lSymb = fType->str.members->symbols;
    while (lSymb) {
        alignment = Cg->theHAL->GetAlignment(lSymb->type);
        size = Cg->theHAL->GetSizeof(lSymb->type);
        addr = ((addr + alignment - 1)/alignment)*alignment;
        lSymb->details.var.addr = addr;
        addr += size;
        lSymb = lSymb->next;
    }
    fType->co.size = ((addr + 3)/4)*4;
} // SetStructMemberOffsets

/*
 * SetStructMembers() - Set the member tree of a structure.
 *
 */

Type *SetStructMembers(SourceLoc *loc, Type *fType, Scope *members)
{
    Symbol *lSymb;
    const char *tagname;

    if (fType) {
        if (fType->str.members) {
            SemanticError(loc, ERROR_SSD_STRUCT_ALREADY_DEFINED,
                            GetAtomString(atable, fType->str.tag),
                            GetAtomString(atable, fType->str.loc.file),
                            fType->str.loc.line);
        } else {
            if (fType->str.tag) {
                tagname = GetAtomString(atable, fType->str.tag);
            } else {
                tagname = "<no-name>";
            }
            fType->str.members = members;
            fType->str.loc = *loc;
            fType->str.HasSemantics = members->HasSemantics;
            SetStructMemberOffsets(fType);
            if (fType->str.tag) {
                lSymb = LookUpLocalSymbol(CurrentScope, fType->str.tag);
                if (!lSymb) {
                    lSymb = DefineTypedef(loc, CurrentScope, fType->str.tag, fType);
                } else {
                    if (IsCategory(fType, TYPE_CATEGORY_STRUCT)) {
                        if (!IsCategory(lSymb->type, TYPE_CATEGORY_STRUCT)) {
                            SemanticError(loc, ERROR_S_NAME_ALREADY_DEFINED, tagname);
                        }
                    }
                }
            }
        }
    }
    return fType;
} // SetStructMembers

/*
 * AddParameter() - Add a parameter to a function's formal parameter list, or a member to a
 *         struct or connector's member list.
 */

void AddParameter(Scope *fScope, Symbol *param)
{
    Symbol *lSymb = fScope->params;

    if (lSymb) {
        while (lSymb->next)
            lSymb = lSymb->next;
        lSymb->next = param;
    } else {
        fScope->params = param;
    }
} // AddParameter

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Various Support Functions: /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GetSwizzleOrWriteMask() - Build a swizzle mask out of the letters in an identifier.
 *
 */
 
int GetSwizzleOrWriteMask(SourceLoc *loc, int atom, int *FIsLValue, int *flen)
{
    const char *s, *t;
    int len, bit, mask, bits;
    int groups, group;
    int LIsLValue;
    char ch;

    s = t = GetAtomString(atable, atom);
    len = mask = bits = groups = 0;
    LIsLValue = 1;
    while (*s) {
        ch = *s++;
        switch (ch) {
        case 'x':
            bit = 0;
            group = 1;
            break;
        case 'y':
            bit = 1;
            group = 1;
            break;
        case 'z':
            bit = 2;
            group = 1;
            break;
        case 'w':
            bit = 3;
            group = 1;
            break;
        case 'r':
            bit = 0;
            group = 2;
            break;
        case 'g':
            bit = 1;
            group = 2;
            break;
        case 'b':
            bit = 2;
            group = 2;
            break;
        case 'a':
            bit = 3;
            group = 2;
            break;
        default:
            SemanticError(loc, ERROR_CS_INVALID_SWIZZLE_CHAR, ch, t);
            return mask;
            break;
        }
        mask |= bit << len*2;
        bit = 1 << bit;
        if (bits & bit)
            LIsLValue = 0;
        bits |= bit;
        if (groups && groups != group) {
            SemanticError(loc, ERROR_CS_INVALID_SWIZZLE_CHAR, ch, t);
            return mask;
        }
        groups |= group;
        len++;
    }
    if (len > 4)
        SemanticError(loc, ERROR_S_SWIZZLE_TOO_LONG, t);
    if (FIsLValue)
        *FIsLValue = LIsLValue;
    if (flen)
        *flen = len;
    return mask;
} // GetSwizzleOrWriteMask

/*
 * GetMatrixSwizzleOrWriteMask() - Build a matrix swizzle mask out of the letters in an identifier.
 *
 */
 
int GetMatrixSwizzleOrWriteMask(SourceLoc *loc, int atom, int *FIsLValue, int *flen)
{
    const char *s, *t;
    int len, bit, mask, bits, base;
    int LIsLValue, Error;
    char lch, ch;

    s = t = GetAtomString(atable, atom);
    len = mask = bits = 0;
    LIsLValue = 1;
    if (s[0] == '_' && s[1] != '\0') {
        Error = 0;
        if (s[1] == 'm') {
            base = 0;
        } else {
            base = 1;
        }
        while (*s) {
            ch = lch = *s++;
            if (ch == '_') {
                if (base == 0) {
                    if (*s++ != 'm') {
                        Error = 1;
                        break;
                    }
                }
                lch = *s++;
                ch = lch - base;
                if (ch >= '0' && ch <= '3') {
                    bit = (ch - '0') << 2;
                    lch = *s++;
                    ch = lch - base;
                    if (ch >= '0' && ch <= '3') {
                        bit = bit | (ch - '0');
                        mask |= bit << len*4;
                        bit = 1 << bit;
                        if (bit & bits)
                            LIsLValue = 0;
                        bits |= bit;
                        len++;
                    } else {
                        Error = 1;
                        break;
                    }
                } else {
                    Error = 1;
                    break;
                }
            } else {
                Error = 1;
                break;
            }
        }
    } else {
        lch = *s;
        Error = 1;
    }
    if (Error) {
        SemanticError(loc, ERROR_CS_INVALID_SWIZZLE_CHAR, lch, t);
    }
    if (len > 4)
        SemanticError(loc, ERROR_S_SWIZZLE_TOO_LONG, t);
    if (FIsLValue)
        *FIsLValue = LIsLValue;
    if (flen)
        *flen = len;
    return mask;
} // GetMatrixSwizzleOrWriteMask

/*
 * GetBaseTypeNameString() - Return a pointer to a string representation of a base type name.
 *
 */

const char *GetBaseTypeNameString(int base)
{
    if (base >= 0 && base <= TYPE_BASE_LAST_USER) {
        return GetAtomString(atable, baseTypeNames[base]);
    } else {
        return "*** bad base value ***";
    }
} // GetBaseTypeNameString

/*
 * ClearSymbolTempptr() - Clear the tempptr for all symbols in this tree.
 *
 */

static void ClearSymbolTempptr(Symbol *fSymb)
{
    if (fSymb) {
        fSymb->tempptr = NULL;
        ClearSymbolTempptr(fSymb->left);
        ClearSymbolTempptr(fSymb->right);
    }
} // ClearSymbolTempptr

/*
 * ClearSymbolTempptrList() - Walk a list of scopes and
 *                            clear tempptr field for all symbols.
 *
 */

static void ClearSymbolTempptrList(Scope *fScope)
{
    while (fScope) {
        ClearSymbolTempptr(fScope->symbols);
        fScope = fScope->next;
    }
} // ClearSymbolTempptrList

/*
 * ClearAllSymbolTempptr
 *
 */

void ClearAllSymbolTempptr(void)
{
    ClearSymbolTempptrList(ScopeList);
} // ClearSymbolTempptr


/*
 * ClearSymbolTempptr2() - Clear the tempptr2 for all symbols in this tree.
 *
 */

static void ClearSymbolTempptr2(Symbol *fSymb)
{
    if (fSymb) {
        fSymb->tempptr2 = NULL;
        ClearSymbolTempptr2(fSymb->left);
        ClearSymbolTempptr2(fSymb->right);
    }
} // ClearSymbolTempptr2

/*
 * ClearSymbolTempptr2List() - Walk a list of scopes and
 *                            clear tempptr field for all symbols.
 *
 */

static void ClearSymbolTempptr2List(Scope *fScope)
{
    while (fScope) {
        ClearSymbolTempptr2(fScope->symbols);
        fScope = fScope->next;
    }
} // ClearSymbolTempptr2List

/*
 * ClearAllSymbolTempptr2
 *
 */

void ClearAllSymbolTempptr2(void)
{
    ClearSymbolTempptr2List(ScopeList);
} // ClearSymbolTempptr2

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of symbols.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
