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
// hal.c
//

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

static void InitHAL_HAL(slHAL *);
static int GetCapsBit_HAL(int bitNumber);
static int GetConnectorUses_HAL(int cid, int pid);
static int GetConnectorRegister_HAL(int cid, int ByIndex, int ratom, Binding *fBind);
static int GetFloatSuffixBase_HAL(SourceLoc *loc, int suffix);
static int GetSizeof_HAL(Type *fType);
static int GetAlignment_HAL(Type *fType);
static int CheckDeclarators_HAL(SourceLoc *loc, const dtype *fDtype);
static int CheckDefinition_HAL(SourceLoc *loc, int name, const Type *fType);
static int CheckStatement_HAL(SourceLoc *loc, stmt *fstmt);
static int CheckInternalFunction_HAL(Symbol *fSymb, int *group);
static int IsNumericBase_HAL(int fBase);
static int IsIntegralBase_HAL(int fBase);
static int IsTexobjBase_HAL(int fBase);
static int IsValidRuntimeBase_HAL(int fBase);
static int IsValidScalarCast_HAL(int toBase, int fromBase, int Explicit);
static int IsValidOperator_HAL(SourceLoc *loc, int name, int op, int suobp);
static int GetBinOpBase_HAL(int lop, int lbase, int rbase, int llen, int rlen);
static int ConvertConstant_HAL(const scalar_constant *fval, int fbase, int tbase,
                    expr **fexpr);
static int BindUniformUnbound_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind);
static int BindUniformPragma_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                    const Binding *fBind);
static int BindVaryingSemantic_HAL(SourceLoc *loc, Symbol *fSymb, int semantic,
                    Binding *fBind, int IsOutVal);
static int BindVaryingPragma_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                    const Binding *fBind, int IsOutVal);
static int BindVaryingUnbound_HAL(SourceLoc *loc, Symbol *fSymb, int name, int connector,
                    Binding *fBind, int IsOutVal);
static int PrintCodeHeader_HAL(FILE *out);
static int GenerateCode_HAL(SourceLoc *loc, Scope *fScope, Symbol *program);

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// Profile Manager: //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * RegisterProfile() - Add a profile to the list of available profiles.
 *
 */

slProfile *RegisterProfile(int (*InitHAL)(slHAL *), const char *name, int id)
{
    slProfile *lProfile;

    lProfile = (slProfile *) malloc(sizeof(slProfile));
    lProfile->next = Cg->allProfiles;
    lProfile->InitHAL = InitHAL;
    lProfile->name = AddAtom(atable, name);
    lProfile->id = id;
    Cg->allProfiles = lProfile;
    return lProfile;
} // RegisterProfile

/*
 * EnumerateProfiles()
 *
 */

slProfile *EnumerateProfiles(int index)
{
    slProfile *lProfile;
    
    lProfile = Cg->allProfiles;
    while (index-- > 0 && lProfile)
        lProfile = lProfile->next;
    return lProfile;
} // EnumerateProfiles

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Default Language HAL ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * InitHAL()
 *
 */

int InitHAL(const char *profileName, const char *entryName)
{
    slProfile *lProfile;
    int result;

    Cg->theHAL = (slHAL *) malloc(sizeof(slHAL));
    InitHAL_HAL(Cg->theHAL);
    Cg->theHAL->profileName = AddAtom(atable, profileName);
    Cg->theHAL->entryName = AddAtom(atable, entryName);
    lProfile = Cg->allProfiles;
    while (lProfile) {
        if (Cg->theHAL->profileName == lProfile->name) {
            Cg->theHAL->InitHAL = lProfile->InitHAL;
            Cg->theHAL->pid = lProfile->id;
            result = Cg->theHAL->InitHAL(Cg->theHAL);
            return result;
        }
        lProfile = lProfile->next;
    }
    printf(OPENSL_TAG ": unknown profile \"%s\".\n", profileName);
    return 0;
} // InitHAL

/*
 * InitHAL_HAL();
 *
 */

static void InitHAL_HAL(slHAL *fHAL)
{
    // Initialize default function members:

    fHAL->InitHAL = NULL;
    fHAL->FreeHAL = NULL;
    fHAL->RegisterNames = NULL;
    fHAL->GetCapsBit = GetCapsBit_HAL;
    fHAL->GetConnectorID = NULL;
    fHAL->GetConnectorAtom = NULL;
    fHAL->GetConnectorUses = GetConnectorUses_HAL;
    fHAL->GetConnectorRegister = GetConnectorRegister_HAL;
    fHAL->GetFloatSuffixBase = GetFloatSuffixBase_HAL;
    fHAL->GetSizeof = GetSizeof_HAL;
    fHAL->GetAlignment = GetAlignment_HAL;
    fHAL->CheckDeclarators = CheckDeclarators_HAL;
    fHAL->CheckDefinition = CheckDefinition_HAL;
    fHAL->CheckStatement = CheckStatement_HAL;
    fHAL->CheckInternalFunction = CheckInternalFunction_HAL;
    fHAL->IsNumericBase = IsNumericBase_HAL;
    fHAL->IsTexobjBase = IsTexobjBase_HAL;
    fHAL->IsIntegralBase = IsIntegralBase_HAL;
    fHAL->IsValidRuntimeBase = IsValidRuntimeBase_HAL;
    fHAL->IsValidScalarCast = IsValidScalarCast_HAL;
    fHAL->IsValidOperator = IsValidOperator_HAL;
    fHAL->GetBinOpBase = GetBinOpBase_HAL;
    fHAL->ConvertConstant = ConvertConstant_HAL;
    fHAL->BindUniformUnbound = BindUniformUnbound_HAL;
    fHAL->BindUniformPragma = BindUniformPragma_HAL;
    fHAL->BindVaryingSemantic = BindVaryingSemantic_HAL;
    fHAL->BindVaryingPragma = BindVaryingPragma_HAL;
    fHAL->BindVaryingUnbound = BindVaryingUnbound_HAL;
    fHAL->PrintCodeHeader = PrintCodeHeader_HAL;
    fHAL->GenerateCode = GenerateCode_HAL;

    // Initialize default data members:

    // Defined when profile is registered:

    fHAL->vendor = "None";
    fHAL->version = "0.0";

    fHAL->profileName = 0;
    fHAL->pid = 0;
    fHAL->entryName = 0;

    fHAL->semantics = NULL;
    fHAL->numSemantics = 0;

    fHAL->incid = CID_NONE_ID;
    fHAL->inputCRegs = NULL;
    fHAL->numInputCRegs = 0;

    fHAL->nextUnboundVinReg = 0;
    fHAL->lastUnboundVinReg = -1;

    fHAL->outcid = CID_NONE_ID;
    fHAL->outputCRegs = NULL;
    fHAL->numOutputCRegs = 0;

    fHAL->nextUnboundVoutReg = 0;
    fHAL->lastUnboundVoutReg = -1;

    // Defined by compiler front end:

    fHAL->globalScope = NULL;
    fHAL->varyingIn = NULL;
    fHAL->varyingOut = NULL;
    fHAL->uniformParam = NULL;
    fHAL->uniformGlobal = NULL;
    fHAL->uniforms = NULL;

    fHAL->constantBindings = NULL;
    fHAL->defaultBindings = NULL;

    // Define default comment start string:

    fHAL->comment = "#";

    // Pointer to profile specific struct:

    fHAL->localData = NULL;

} // InitHAL_HAL

/*
 * AddConstantBinding() - Add a constant binding to the program.
 *
 */

void AddConstantBinding(Binding *fBind)
{
    BindingList *lBindList, *nBindList;

    nBindList = (BindingList *) malloc(sizeof(BindingList));
    nBindList->next = NULL;
    nBindList->binding = fBind;
    lBindList = Cg->theHAL->constantBindings;
    if (lBindList) {
        while (lBindList->next)
            lBindList = lBindList->next;
        lBindList->next = nBindList;
    } else {
        Cg->theHAL->constantBindings = nBindList;
    }
} // AddConstantBinding

/*
 * AddDefaultBinding() - Add a default binding to the program.
 *
 */

void AddDefaultBinding(Binding *fBind)
{
    BindingList *lBindList, *nBindList;

    nBindList = (BindingList *) malloc(sizeof(BindingList));
    nBindList->next = NULL;
    nBindList->binding = fBind;
    lBindList = Cg->theHAL->defaultBindings;
    if (lBindList) {
        while (lBindList->next)
            lBindList = lBindList->next;
        lBindList->next = nBindList;
    } else {
        Cg->theHAL->defaultBindings = nBindList;
    }
} // AddDefaultBinding

/*
 * LookupConnectorHAL() - Lookup a connector descriptor by cid.
 *
 */

ConnectorDescriptor *LookupConnectorHAL(ConnectorDescriptor *connectors, int cid, int num)
{
    int ii;

    for (ii = 0; ii < num; ii++) {
        if (cid == connectors[ii].cid)
            return &connectors[ii];
    }
    return NULL;
} // LookupConnectorHAL

/*
 * SetSymbolConnectorBindingHAL()
 *
 */

void SetSymbolConnectorBindingHAL(Binding *fBind, ConnectorRegisters *fConn)
{
    BindingConnector *tBind;

    tBind = &fBind->conn;
    tBind->properties = BIND_IS_BOUND;
    tBind->kind = BK_CONNECTOR;
    if (fConn->properties & REG_WRITE_REQUIRED)
        tBind->properties |= BIND_WRITE_REQUIRED;
    // tBind->gname set elsewhere
    // tBind->lname set elsewhere
    tBind->base = fConn->base;
    tBind->size = fConn->size;
    tBind->rname = fConn->name;
    tBind->regno = fConn->regno;
} // SetSymbolConnectorBindingHAL

/*
 * GetCapsBit_HAL() - Return an integer value representing the capabilities of this profile.
 *
 */

static int GetCapsBit_HAL(int bitNumber)
{
    return 0;
} // GetCapsBit_HAL

/*
 * GetConnectorUses_HAL() - Return a connectors capabilities for this profile.
 *
 */

static int GetConnectorUses_HAL(int cid, int pid)
{
    return CONNECTOR_IS_USELESS;
} // GetConnectorUses_HAL

/*
 * GetConnectorRegister_HAL() - Return the hw register number for "ratom" in connector "cid".
 *
 */

static int GetConnectorRegister_HAL(int cid, int ByIndex, int ratom, Binding *fBind)
{
    return 0;
} // GetConnectorRegister_HAL

/*
 * GetFloatSuffixBase_HAL() - Check for profile-specific limitations of floating point
 *         suffixes and return the base for this suffix.
 *
 */

static int GetFloatSuffixBase_HAL(SourceLoc *loc, int suffix)
{
    switch (suffix) {
    case ' ':
        return TYPE_BASE_CFLOAT;
    case 'f':
        return TYPE_BASE_FLOAT;
    default:
        SemanticError(loc, ERROR_C_UNSUPPORTED_FP_SUFFIX, suffix);
        return TYPE_BASE_UNDEFINED_TYPE;
    }
} // GetFloatSuffixBase_HAL

/*
 * GetSizeof_HAL() - Return a profile specific size for this scalar, vector, or matrix type.
 *         Used for defining struct member offsets for use by code generator.
 */

static int GetSizeof_HAL(Type *fType)
{
    int category, size, alignment, len, len2;

    if (fType) {
        category = GetCategory(fType);
        switch (category) {
        case TYPE_CATEGORY_SCALAR:
        case TYPE_CATEGORY_STRUCT:
        case TYPE_CATEGORY_CONNECTOR:
            size = fType->co.size;
            break;
        case TYPE_CATEGORY_ARRAY:
            if (IsVector(fType, &len)) {
                size = len;
            } else if (IsMatrix(fType, &len, &len2)) {
                if (len2 > len) {
                    size = len*4;
                } else {
                    size = len2*4;
                }
            } else {
                size = Cg->theHAL->GetSizeof(fType->arr.eltype);
                alignment = Cg->theHAL->GetAlignment(fType->arr.eltype);
                size = ((size + alignment - 1)/alignment)*alignment*fType->arr.numels;
            }
            break;
        case TYPE_CATEGORY_FUNCTION:
        default:
            size = 0;
            break;
        }
    } else {
        size = 0;
    }
    return size;
} // GetSizeof_HAL

/*
 * GetAlignment_HAL() - Return a profile specific alignment for this type.
 *         Used for defining struct member offsets for use by code generator.
 */

static int GetAlignment_HAL(Type *fType)
{
    int category, alignment;

    if (fType) {
        if (Cg->theHAL->IsTexobjBase(GetBase(fType))) {
            alignment = 4;
        } else {
            category = GetCategory(fType);
            switch (category) {
            case TYPE_CATEGORY_SCALAR:
                alignment = 4;
                break;
            case TYPE_CATEGORY_STRUCT:
            case TYPE_CATEGORY_ARRAY:
                alignment = 4;
                break;
            case TYPE_CATEGORY_FUNCTION:
            default:
                alignment = 1;
                break;
            }
        }
    } else {
        alignment = 1;
    }
    return alignment;
} // GetAlignment_HAL

/*
 * CheckDeclarators_HAL() - Check for profile-specific limitations of declarators.
 *
 */

static int CheckDeclarators_HAL(SourceLoc *loc, const dtype *fDtype)
{
    int numdims = 0;
    const Type *lType;

    lType = &fDtype->type;
    while (GetCategory(lType) == TYPE_CATEGORY_ARRAY) {
        if (lType->arr.numels == 0 && numdims > 0) {
            SemanticError(loc, ERROR___LOW_DIM_UNSPECIFIED);
            return 0;
        }
        if (lType->arr.numels > 4 && IsPacked(lType)) {
            SemanticError(loc, ERROR___PACKED_DIM_EXCEEDS_4);
            return 0;
        }
        lType = lType->arr.eltype;
        numdims++;
    }
    if (numdims > 3) {
        SemanticError(loc, ERROR___NUM_DIMS_EXCEEDS_3);
        return 0;
    }
    return 1;
} // CheckDeclarators_HAL

/*
 * CheckDefinition_HAL() - Check for profile-specific statement limitations.
 *
 */

static int CheckDefinition_HAL(SourceLoc *loc, int name, const Type *fType)
{
    return 1;
} // CheckDefinition_HAL

/*
 * CheckStatement_HAL() - Check for profile-specific limitations of statements.
 *
 */

static int CheckStatement_HAL(SourceLoc *loc, stmt *fstmt)
{
    return 1;
} // CheckStatement_HAL

/*
 * CheckInternalFunction_HAL() - Check for profile-specific internally implemented function.
 *
 */

static int CheckInternalFunction_HAL(Symbol *fSymb, int *group)
{
    return 0;
} // CheckInternalFunction_HAL

/*
 * IsValidScalarCast_HAL() - Is it valid to typecast a scalar from fromBase to toBase?.
 *
 */

static int IsValidScalarCast_HAL(int toBase, int fromBase, int Explicit)
{
    int answer;

    switch (toBase) {
    case TYPE_BASE_BOOLEAN:
    case TYPE_BASE_FLOAT:
    case TYPE_BASE_INT:
        switch (fromBase) {
        case TYPE_BASE_CFLOAT:
        case TYPE_BASE_CINT:
        case TYPE_BASE_FLOAT:
        case TYPE_BASE_INT:
            answer = 1;
            break;
        case TYPE_BASE_BOOLEAN:
            answer = (toBase == TYPE_BASE_BOOLEAN) || Explicit;
            break;
        default:
            answer = 0;
            break;
        }
        break;
    case TYPE_BASE_CFLOAT:
    case TYPE_BASE_CINT:
    default:
        answer = 0;
        break;
    }
    return answer;
} // IsValidScalarCast_HAL

/*
 * IsValidOperator_HAL() - Is this operator supported in this profile?  Print an error is not.
 *
 */

static int IsValidOperator_HAL(SourceLoc *loc, int name, int op, int suobp)
{
    switch (op) {
    default:
        return 1;
    }
} // IsValidOperator_HAL

/*
 * IsNumericBase_HAL() - Is fBase a numeric type?
 *
 */

static int IsNumericBase_HAL(int fBase)
{
    int answer;

    switch (fBase) {
    case TYPE_BASE_CFLOAT:
    case TYPE_BASE_CINT:
    case TYPE_BASE_FLOAT:
    case TYPE_BASE_INT:
        answer = 1;
        break;
    default:
        answer = 0;
        break;
    }
    return answer;
} // IsNumericBase_HAL

/*
 * IsIntegralBase_HAL() - Is fBase an integral type?
 *
 */

static int IsIntegralBase_HAL(int fBase)
{
    int answer;

    switch (fBase) {
    case TYPE_BASE_CINT:
    case TYPE_BASE_INT:
        answer = 1;
        break;
    default:
        answer = 0;
        break;
    }
    return answer;
} // IsIntegralBase_HAL

/*
 * IsTexobjBase_HAL() - Is fBase a texture object type?
 *
 */

static int IsTexobjBase_HAL(int fBase)
{
    return 0;
} // IsTexobjBase_HAL

/*
 * IsValidRuntimeBase_HAL() - Are runtime variables with a base of fBase supported?
 *         In other words, can a non-const variable of this base be declared in this profile?
 *
 */

static int IsValidRuntimeBase_HAL(int fBase)
{
    int answer;

    switch (fBase) {
    case TYPE_BASE_FLOAT:
        answer = 1;
        break;
    default:
        answer = 0;
        break;
    }
    return answer;
} // IsIntegralBase_HAL

/*
 * GetBinOpBase_HAL() - Return the base type for this binary operation.
 *
 */

static int GetBinOpBase_HAL(int lop, int lbase, int rbase, int llen, int rlen)
{
    int result;

    switch (lop) {
    case VECTOR_V_OP:
    case MUL_OP:
    case DIV_OP:
    case MOD_OP:
    case ADD_OP:
    case SUB_OP:
    case SHL_OP:
    case SHR_OP:
    case LT_OP:
    case GT_OP:
    case LE_OP:
    case GE_OP:
    case EQ_OP:
    case NE_OP:
    case AND_OP:
    case XOR_OP:
    case OR_OP:
    case COND_OP:
        if (lbase == rbase) {
            result = lbase;
        } else if (lbase == TYPE_BASE_FLOAT || rbase == TYPE_BASE_FLOAT) {
            result = TYPE_BASE_FLOAT;
        } else if (lbase == TYPE_BASE_CFLOAT || rbase == TYPE_BASE_CFLOAT) {
            if (lbase == TYPE_BASE_INT || rbase == TYPE_BASE_INT) {
                result = TYPE_BASE_FLOAT;
            } else {
                result = TYPE_BASE_CFLOAT;
            }
        } else {
            result = TYPE_BASE_INT;
        }
        break;
    default:
        result = TYPE_BASE_NO_TYPE;
        break;
    };
    return result;
} // GetBinOpBase_HAL

/*
 * ConvertConstant()_HAL - Convert a numeric scalar constant from one base type to another.
 *
 */

static int ConvertConstant_HAL(const scalar_constant *fval, int fbase, int tbase, expr **fexpr)
{
    expr *lexpr = NULL;

    switch (fbase) {
    case TYPE_BASE_CFLOAT:
    case TYPE_BASE_FLOAT:
        switch (tbase) {
        case TYPE_BASE_CFLOAT:
        case TYPE_BASE_FLOAT:
            lexpr = (expr *) NewFConstNode(FCONST_OP, fval->f, tbase);
            *fexpr = lexpr;
            break;
        case TYPE_BASE_CINT:
        case TYPE_BASE_INT:
            lexpr = (expr *) NewIConstNode(ICONST_OP, (int) fval->f, tbase);
            *fexpr = lexpr;
            break;
        default:
            return 0;
        }
        break;
    case TYPE_BASE_CINT:
    case TYPE_BASE_INT:
        switch (tbase) {
        case TYPE_BASE_CFLOAT:
        case TYPE_BASE_FLOAT:
            lexpr = (expr *) NewFConstNode(FCONST_OP, (float) fval->i, tbase);
            *fexpr = lexpr;
            break;
        case TYPE_BASE_CINT:
        case TYPE_BASE_INT:
            lexpr = (expr *) NewIConstNode(ICONST_OP, fval->i, tbase);
            *fexpr = lexpr;
            break;
        default:
            return 0;
        }
        break;
    default:
        return 0;
    }
    return 1;
} // ConvertConstant_HAL

/*
 * BindUniformUnbound_HAL() - Bind an unbound variable to a free uniform resource.
 *
 */

static int BindUniformUnbound_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind)
{
    return 0;
} // BindUniformUnbound_HAL

/*
 * BindUniformPragma_HAL() - Bind a variable to a uniform resource based on a #pragma bind.
 *
 */

static int BindUniformPragma_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                                 const Binding *fBind)
{
    return 0;
} // BindUniformPragma_HAL

/*
 * BindVaryingSemantic_HAL() - Bind a variable to a specific varying semantic.
 *
 */

static int BindVaryingSemantic_HAL(SourceLoc *loc, Symbol *fSymb, int semantic,
                                              Binding *fBind, int IsOutVal)
{
    return 0;
} // BindVaryingSemantic_HAL

/*
 * BindVaryingPragma_HAL() - Bind a variable to a varying resource based on a #pragma bind.
 *
 */

static int BindVaryingPragma_HAL(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                                 const Binding *fBind, int IsOutVal)
{
    return 0;
} // BindVaryingPragma_HAL

/*
 * BindVaryingUnbound_HAL() - Bind a variable with no binding information to a varying resource.
 *
 */

static int BindVaryingUnbound_HAL(SourceLoc *loc, Symbol *fSymb, int name, int connector,
                                  Binding *fBind, int IsOutVal)
{
    return 0;
} // BindVaryingUnbound_HAL

/*
 * PrintCodeHeader_HAL() - Dummy header output.
 *
 */

static int PrintCodeHeader_HAL(FILE *out)

{
    InternalError(Cg->tokenLoc, ERROR_S_NO_CODE_HEADER, GetAtomString(atable, Cg->theHAL->profileName));
    return 1;
} // PrintCodeHeader_HAL

/*
 * GenerateCode_HAL() - Dummy code generator.
 *
 */

static int GenerateCode_HAL(SourceLoc *loc, Scope *fScope, Symbol *program)
{
    InternalError(Cg->tokenLoc, ERROR_S_NO_CODE_GENERATOR, GetAtomString(atable, Cg->theHAL->profileName));
    return 1;
} // GenerateCode_HAL

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// End of hal.c ////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
