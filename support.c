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
// support.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

dtype CurrentDeclTypeSpecs = { 0, };

#undef PICK
#define PICK(a, b, c, d, e) d

const nodekind NodeKind[] = {
    OPCODE_TABLE
};

#undef PICK

#define PICK(a, b, c, d, e) c

int const opcode_atom[] = {
    OPCODE_TABLE
};

#undef PICK

/*
 * NewDeclNode() - Create a new declaration node.
 *
 */
 
decl *NewDeclNode(SourceLoc *loc, int atom, dtype *type)
{
    decl *pdecl;

    pdecl = (decl *) malloc(sizeof(decl));
    pdecl->kind = DECL_N;
    pdecl->loc = *loc;
    pdecl->name = atom;
    pdecl->semantics = 0;
    pdecl->type = *type;
    pdecl->next = NULL;
    pdecl->symb = NULL;
    pdecl->params = NULL;
    pdecl->initexpr = NULL;
    return pdecl;
} // NewDeclNode

/*
 * NewSymbNode() - Create a new symbol node.
 *
 */
 
symb *NewSymbNode(opcode op, Symbol *fSymb)
{
    symb *psymb;

    assert(NodeKind[op] == SYMB_N);
    psymb = (symb *) malloc(sizeof(symb));
    psymb->kind = SYMB_N;
    psymb->type = fSymb->type;
    psymb->IsLValue = 1;
    if (GetQualifiers(psymb->type) & TYPE_QUALIFIER_CONST) {
        psymb->IsConst = 1;
    } else {
        psymb->IsConst = 0;
    }
    psymb->HasSideEffects = 0;
    psymb->op = op;
    psymb->symbol = fSymb;
    return psymb;
} // NewSymbNode

/*
 * NewIConstNode() - Create a new integer constant node.
 *
 */
 
constant *NewIConstNode(opcode op, int fval, int base)
{
    constant *pconst;

    assert(NodeKind[op] == CONST_N);
    pconst = (constant *) malloc(sizeof(constant));
    pconst->kind = CONST_N;
    pconst->type = GetStandardType(base, 0, 0);
    pconst->IsLValue = 0;
    pconst->IsConst = 0;
    pconst->HasSideEffects = 0;
    pconst->op = op;
    pconst->subop = SUBOP__(base);
    pconst->val[0].i = fval;
    pconst->tempptr[0] = 0;
    return pconst;
} // NewIConstNode

/*
 * NewBConstNode() - Create a new Boolean constant node.
 *
 */
 
constant *NewBConstNode(opcode op, int fval, int base)
{
    constant *pconst;

    assert(NodeKind[op] == CONST_N);
    pconst = (constant *) malloc(sizeof(constant));
    pconst->kind = CONST_N;
    pconst->type = GetStandardType(base, 0, 0);
    pconst->IsLValue = 0;
    pconst->IsConst = 0;
    pconst->HasSideEffects = 0;
    pconst->op = op;
    pconst->subop = SUBOP__(base);
    pconst->val[0].i = fval;
    pconst->tempptr[0] = 0;
    return pconst;
} // NewBConstNode

/*
 * NewFConstNode() - Create a new floating point constant node.
 *
 */
 
constant *NewFConstNode(opcode op, float fval, int base)
{
    constant *pconst;

    assert(NodeKind[op] == CONST_N);
    pconst = (constant *) malloc(sizeof(constant));
    pconst->kind = CONST_N;
    pconst->type = GetStandardType(base, 0, 0);
    pconst->IsLValue = 0;
    pconst->IsConst = 0;
    pconst->HasSideEffects = 0;
    pconst->op = op;
    pconst->subop = SUBOP__(base);
    pconst->val[0].f = fval;
    pconst->tempptr[0] = 0;
    return pconst;
} // NewFConstNode

/*
 * NewFConstNodeV() - Create a new floating point constant vector node.
 *
 */
 
constant *NewFConstNodeV(opcode op, float *fval, int len, int base)
{
    constant *pconst;
    int ii;

    assert(NodeKind[op] == CONST_N);
    pconst = (constant *) malloc(sizeof(constant));
    pconst->kind = CONST_N;
    pconst->type = GetStandardType(base, len, 0);
    pconst->IsLValue = 0;
    pconst->IsConst = 0;
    pconst->HasSideEffects = 0;
    pconst->op = op;
    pconst->subop = SUBOP_V(len, base);
    for (ii = 0; ii < len; ii++)
        pconst->val[ii].f = fval[ii];
    pconst->tempptr[0] = 0;
    return pconst;
} // NewFConstNodeV

/*
 * NewUnopNode() - Create a unary op node.
 *
 */
 
unary *NewUnopNode(opcode op, expr *arg)
{
    unary *pun;

    assert(NodeKind[op] == UNARY_N);
    pun = (unary *) malloc(sizeof(unary));
    pun->kind = UNARY_N;
    pun->type = UndefinedType;
    pun->IsLValue = 0;
    pun->IsConst = 0;
    pun->HasSideEffects = 0;
    if (arg)
        pun->HasSideEffects = arg->common.HasSideEffects;
    pun->op = op;
    pun->subop = 0;
    pun->arg = arg;
    pun->tempptr[0] = 0;
    return pun;
} // NewUnopNode

/*
 * NewUnopSubNode() - Create a unary op node.
 *
 */
 
unary *NewUnopSubNode(opcode op, int subop, expr *arg)
{
    unary *pun;

    assert(NodeKind[op] == UNARY_N);
    pun = (unary *) malloc(sizeof(unary));
    pun->kind = UNARY_N;
    pun->type = UndefinedType;
    pun->IsLValue = 0;
    pun->IsConst = 0;
    pun->HasSideEffects = 0;
    if (arg)
        pun->HasSideEffects = arg->common.HasSideEffects;
    pun->op = op;
    pun->subop = subop;
    pun->arg = arg;
    pun->tempptr[0] = 0;
    return pun;
} // NewUnopSubNode

/*
 * NewBinopNode() - Create a binary op node.
 *
 */
 
binary *NewBinopNode(opcode op, expr *left, expr *right)
{
    binary *pbin;

    assert(NodeKind[op] == BINARY_N);
    pbin = (binary *) malloc(sizeof(binary));
    pbin->kind = BINARY_N;
    pbin->type = UndefinedType;
    pbin->IsLValue = 0;
    pbin->IsConst = 0;
    pbin->HasSideEffects = 0;
    if (left)
        pbin->HasSideEffects = left->common.HasSideEffects;
    if (right)
        pbin->HasSideEffects |= right->common.HasSideEffects;
    pbin->op = op;
    pbin->subop = 0;
    pbin->left = left;
    pbin->right = right;
    pbin->tempptr[0] = 0;
    pbin->tempptr[1] = 0;
    return pbin;
} // NewBinopNode

/*
 * NewBinopSubNode() - Create a binary op node.
 *
 */
 
binary *NewBinopSubNode(opcode op, int subop, expr *left, expr *right)
{
    binary *pbin;

    assert(NodeKind[op] == BINARY_N);
    pbin = (binary *) malloc(sizeof(binary));
    pbin->kind = BINARY_N;
    pbin->type = UndefinedType;
    pbin->IsLValue = 0;
    pbin->IsConst = 0;
    pbin->HasSideEffects = 0;
    if (left)
        pbin->HasSideEffects = left->common.HasSideEffects;
    if (right)
        pbin->HasSideEffects |= right->common.HasSideEffects;
    pbin->op = op;
    pbin->subop = subop;
    pbin->left = left;
    pbin->right = right;
    pbin->tempptr[0] = 0;
    pbin->tempptr[1] = 0;
    return pbin;
} // NewBinopSubNode

/*
 * NewTriopNode() - Create a trinary op node.
 *
 */
 
trinary *NewTriopNode(opcode op, expr *arg1, expr *arg2, expr *arg3)
{
    trinary *ptri;

    assert(NodeKind[op] == TRINARY_N);
    ptri = (trinary *) malloc(sizeof(trinary));
    ptri->kind = TRINARY_N;
    ptri->type = UndefinedType;
    ptri->IsLValue = 0;
    ptri->IsConst = 0;
    ptri->HasSideEffects = 0;
    if (arg1)
        ptri->HasSideEffects = arg1->common.HasSideEffects;
    if (arg2)
        ptri->HasSideEffects |= arg2->common.HasSideEffects;
    if (arg3)
        ptri->HasSideEffects |= arg3->common.HasSideEffects;
    ptri->op = op;
    ptri->subop = 0;
    ptri->arg1 = arg1;
    ptri->arg2 = arg2;
    ptri->arg3 = arg3;
    ptri->tempptr[0] = 0;
    ptri->tempptr[1] = 0;
    ptri->tempptr[2] = 0;
    return ptri;
} // NewTriopNode

/*
 * NewTriopSubNode() - Create a trinary op node.
 *
 */
 
trinary *NewTriopSubNode(opcode op, int subop, expr *arg1, expr *arg2, expr *arg3)
{
    trinary *ptri;

    assert(NodeKind[op] == TRINARY_N);
    ptri = (trinary *) malloc(sizeof(trinary));
    ptri->kind = TRINARY_N;
    ptri->type = UndefinedType;
    ptri->IsLValue = 0;
    ptri->IsConst = 0;
    ptri->HasSideEffects = 0;
    if (arg1)
        ptri->HasSideEffects = arg1->common.HasSideEffects;
    if (arg2)
        ptri->HasSideEffects |= arg2->common.HasSideEffects;
    if (arg3)
        ptri->HasSideEffects |= arg3->common.HasSideEffects;
    ptri->op = op;
    ptri->subop = subop;
    ptri->arg1 = arg1;
    ptri->arg2 = arg2;
    ptri->arg3 = arg3;
    ptri->tempptr[0] = 0;
    ptri->tempptr[1] = 0;
    ptri->tempptr[2] = 0;
    return ptri;
} // NewTriopSubNode

/*
 * DupSymbNode() - Duplicate a symb op node.
 *
 */
 
symb *DupSymbNode(const symb *fsymb)
{
    symb *lsymb;

    lsymb = (symb *) malloc(sizeof(symb));
    *lsymb = *fsymb;
    return lsymb;
} // DupSymbNode

/*
 * DupConstNode() - Duplicate a constant op node.
 *
 */
 
constant *DupConstNode(const constant *fconst)
{
    constant *lconst;

    lconst = (constant *) malloc(sizeof(constant));
    *lconst = *fconst;
    return lconst;
} // DupConstNode

/*
 * DupUnaryNode() - Duplicate a unary op node.
 *
 */
 
unary *DupUnaryNode(const unary *fun)
{
    unary *lun;

    lun = (unary *) malloc(sizeof(unary));
    *lun = *fun;
    return lun;
} // DupUnaryNode

/*
 * DupBinaryNode() - Duplicate a binary op node.
 *
 */
 
binary *DupBinaryNode(const binary *fbin)
{
    binary *lbin;

    lbin = (binary *) malloc(sizeof(binary));
    *lbin = *fbin;
    return lbin;
} // DupBinaryNode

/*
 * DupTrinaryNode() - Duplicate a trinary op node.
 *
 */
 
trinary *DupTrinaryNode(const trinary *ftri)
{
    trinary *ltri;

    ltri = (trinary *) malloc(sizeof(trinary));
    *ltri = *ftri;
    return ltri;
} // DupTrinaryNode


/*
 * DupNode() - Duplicate a expression node.
 *
 */
 
expr *DupNode(const expr *fExpr)
{
    switch (fExpr->common.kind) {
    case SYMB_N: return (expr *) DupSymbNode(&fExpr->sym);
    case CONST_N: return (expr *) DupConstNode(&fExpr->co);
    case UNARY_N: return (expr *) DupUnaryNode(&fExpr->un);
    case BINARY_N: return (expr *) DupBinaryNode(&fExpr->bin);
    case TRINARY_N: return (expr *) DupTrinaryNode(&fExpr->tri);
    }

    FatalError("unsupported node type in DupNode");
    return NULL;
} // DupNode


/*
 * NewExprStmt() - Create an expression statement.
 *
 */
 
expr_stmt *NewExprStmt(SourceLoc *loc, expr *fExpr)
{
    expr_stmt *lStmt;

    lStmt = (expr_stmt *) malloc(sizeof(expr_stmt));
    lStmt->kind = EXPR_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->exp = fExpr;
    return lStmt;
} // NewExprStmt

/*
 * NewIfStmt() - Create an expression statement.
 *
 */
 
if_stmt *NewIfStmt(SourceLoc *loc, expr *fExpr, stmt *thenstmt, stmt *elsestmt)
{
    if_stmt *lStmt;

    lStmt = (if_stmt *) malloc(sizeof(if_stmt));
    lStmt->kind = IF_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->cond = fExpr;
    lStmt->thenstmt = thenstmt;
    lStmt->elsestmt = elsestmt;
    return lStmt;
} // NewIfStmt

/*
 * NewIfStmt() - Create an expression statement.
 *
 */
 
if_stmt *SetThenElseStmts(SourceLoc *loc, stmt *ifstmt, stmt *thenstmt, stmt *elsestmt)
{
    if_stmt *lStmt;

    lStmt = (if_stmt *) ifstmt;
    assert(lStmt->kind == IF_STMT);
    lStmt->thenstmt = thenstmt;
    lStmt->elsestmt = elsestmt;
    return lStmt;
} // NewIfStmt

/*
 * NewWhileStmt() - Create a while statement.
 *
 */
 
while_stmt *NewWhileStmt(SourceLoc *loc, stmtkind kind, expr *fExpr, stmt *body)
{
    while_stmt *lStmt;

    lStmt = (while_stmt *) malloc(sizeof(while_stmt));
    lStmt->kind = kind;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->cond = fExpr;
    lStmt->body = body;
    return lStmt;
} // NewWhileStmt

/*
 * NewForStmt() - Create a for statement.
 *
 */
 
for_stmt *NewForStmt(SourceLoc *loc, stmt *fexpr1, expr *fexpr2, stmt *fexpr3, stmt *body)
{
    for_stmt *lStmt;

    lStmt = (for_stmt *) malloc(sizeof(for_stmt));
    lStmt->kind = FOR_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->init = fexpr1;
    lStmt->cond = fexpr2;
    lStmt->step = fexpr3;
    lStmt->body = body;
    return lStmt;
} // NewForStmt

/*
 * NewBlockStmt() - Create a block statement.
 *
 */
 
block_stmt *NewBlockStmt(SourceLoc *loc, stmt *fStmt)
{
    block_stmt *lStmt;

    lStmt = (block_stmt *) malloc(sizeof(block_stmt));
    lStmt->kind = BLOCK_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->body = fStmt;
    return lStmt;
} // NewBlockStmt

/*
 * NewReturnStmt() - Create an expression statement.
 *
 */
 
return_stmt *NewReturnStmt(SourceLoc *loc, Scope *fScope, expr *fExpr)
{
    return_stmt *lStmt;
    expr *lExpr;

    if (fScope) {
        while (fScope->level > 2)
            fScope = fScope->next;
        fScope->HasReturnStmt = 1;
        if (fScope->returnType) {
            if (fScope->returnType == VoidType) {
                if (fExpr) {
                    SemanticError(loc, ERROR___VOID_FUN_RETURNS_VALUE);
                }
            } else if (fScope->returnType != UndefinedType) {
                if (ConvertType(fExpr, fScope->returnType, fExpr->common.type, &lExpr, 0, 0)) {
                    fExpr = lExpr;
                } else {
                    SemanticError(loc, ERROR___RETURN_EXPR_INCOMPAT);
                }
            }
        }
    }
    lStmt = (return_stmt *) malloc(sizeof(return_stmt));
    lStmt->kind = RETURN_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->exp = fExpr;
    return lStmt;
} // NewReturnStmt

/*
 * NewDiscardStmt() - Create a discard statement.
 *
 */
 
discard_stmt *NewDiscardStmt(SourceLoc *loc, expr *fExpr)
{
    discard_stmt *lStmt;
    int len;

    lStmt = (discard_stmt *) malloc(sizeof(discard_stmt));
    lStmt->kind = DISCARD_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    if (fExpr && IsVector(fExpr->common.type, &len)) {
        /* empty */ ;
    } else {
        len = 0;
    }
    fExpr = (expr *) NewUnopSubNode(KILL_OP, SUBOP_V(len, TYPE_BASE_BOOLEAN), fExpr);
    lStmt->cond = fExpr;
    return lStmt;
} // NewDiscardStmt

/*
 * NewCommentStmt() - Create a comment statement.
 *
 */
 
comment_stmt *NewCommentStmt(SourceLoc *loc, const char *str)
{
    comment_stmt *lStmt;

    lStmt = (comment_stmt *) malloc(sizeof(comment_stmt));
    lStmt->kind = COMMENT_STMT;
    lStmt->next = NULL;
    lStmt->loc = *loc;
    lStmt->str = AddAtom(atable, str);
    return lStmt;
} // NewCommentStmt

/************************************* dtype functions: *************************************/

/*
 * GetTypePointer() - Strange function that returns a pointer to the type defined by it's
 *         argument.  There are 2 cases:
 *
 * A) IsDerived is TRUE:  This type is a stack-frame resident copy of another type.
 *          It has been modified by a qualifier, etc., and does not have a copy in the heap.
 *          Copy the contents into a freshly malloc'ed type and return it's address.
 * B) IsDerived is FALSE: This type is the same as that pointed to by "base".  Return "base".
 */

Type *GetTypePointer(SourceLoc *loc, const dtype *fDtype)
{
    Type *pType;

    if (fDtype) {
        if (fDtype->IsDerived) {
            if (Cg->theHAL->CheckDeclarators(loc, fDtype))
                ; /* empty statement */
            pType = NewType(0, 0);
            *pType = fDtype->type;
            pType->properties &= ~(TYPE_MISC_TYPEDEF | TYPE_MISC_PACKED_KW);
            pType->co.size = Cg->theHAL->GetSizeof(pType);
        } else {
            pType = fDtype->basetype;
        }
    } else {
        pType = UndefinedType;
    }
    return pType;
} // GetTypePointer

/*
 * SetDType() - Set the fields of a dtype to match a type.
 *
 */

dtype *SetDType(dtype *fDtype, Type *fType)
{
    fDtype->basetype = fType;
    fDtype->IsDerived = 0;
    fDtype->numNewDims = 0;
    fDtype->storageClass = SC_UNKNOWN;
    fDtype->type = *fType;
    return fDtype;
} // SetDType

/*
 * NewDType() - Initialize the fields of a dtype.
 *
 */

dtype *NewDType(dtype *fDtype, Type *baseType, int category)
{
    fDtype->basetype = baseType;
    fDtype->IsDerived = 1;
    fDtype->numNewDims = 0;
    fDtype->storageClass = SC_UNKNOWN;
    InitType(&fDtype->type);
    fDtype->type.properties = category;
    return fDtype;
} // NewDType

/*
 * SetTypeCategory() - Set the category of a type.  Issue an error if it's already set to a
 *         conflicting category.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypeCategory(SourceLoc *loc, int atom, dtype *fType, int category, int Force)
{
    int lcategory;

    lcategory = fType->type.properties & TYPE_CATEGORY_MASK;
    if (Force || lcategory == TYPE_CATEGORY_NONE) {
        fType->type.properties &= ~TYPE_CATEGORY_MASK;
        fType->type.properties |= category;
        fType->IsDerived = 1;
    } else {
        if (lcategory != category) {
            SemanticError(loc, ERROR_S_CONFLICTING_DECLARATION, GetAtomString(atable, atom));
            return 0;
        }
    }
    return 1;
} // SetTypeCategory

/*
 * SetTypeQualifiers() - Set a type's qualifier bits.  Issue an error if any bit is already set.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypeQualifiers(SourceLoc *loc, dtype *fType, int qualifiers)
{
    int lqualifiers;

    qualifiers &= TYPE_QUALIFIER_MASK;
    lqualifiers = fType->type.properties & TYPE_QUALIFIER_MASK;
    if (lqualifiers & qualifiers) {
        SemanticWarning(loc, WARNING___QUALIFIER_SPECIFIED_TWICE);
    }
    if (lqualifiers != qualifiers) {
        fType->type.properties |= qualifiers & TYPE_QUALIFIER_MASK;
        fType->IsDerived = 1;
        if ((fType->type.properties & (TYPE_QUALIFIER_CONST | TYPE_QUALIFIER_OUT)) ==
            (TYPE_QUALIFIER_CONST | TYPE_QUALIFIER_OUT))
        {
            SemanticError(loc, ERROR___CONST_OUT_INVALID);
        }
    }
    return 1;
} // SetTypeCategory

/*
 * SetTypeDomain() - Set the domain of a type.  Issue an error if it's already set to a
 *         conflicting domain.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypeDomain(SourceLoc *loc, dtype *fType, int domain)
{
    int ldomain;

    ldomain = fType->type.properties & TYPE_DOMAIN_MASK;
    if (ldomain == TYPE_DOMAIN_UNKNOWN) {
        fType->type.properties &= ~TYPE_DOMAIN_MASK;
        fType->type.properties |= domain;
        fType->IsDerived = 1;
    } else {
        if (ldomain == domain) {
            SemanticWarning(loc, WARNING___DOMAIN_SPECIFIED_TWICE);
        } else {
            SemanticError(loc, ERROR___CONFLICTING_DOMAIN);
            return 0;
        }
    }
    return 1;
} // SetTypeDomain

/*
 * SetTypeMisc() - Set a bit in the misc field a type.  Issue an error if it's already set.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypeMisc(SourceLoc *loc, dtype *fType, int misc)
{
    if (fType) {
        if (fType->type.properties & misc) {
            SemanticError(loc, ERROR___REPEATED_TYPE_ATTRIB);
            return 0;
        }
        if (misc & ~TYPE_MISC_TYPEDEF)
            fType->IsDerived = 1;
        fType->type.properties |= misc;
        return 1;
    }
    return 0;
} // SetTypeMisc

/*
 * SetTypePacked() - Add the PAKED attribute to a type specifier.  Issue an error if it's already set.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetTypePacked(SourceLoc *loc, dtype *fType)
{
    if (fType) {
		if (fType->type.properties & TYPE_MISC_PACKED_KW) {
			SemanticError(loc, ERROR___REPEATED_TYPE_ATTRIB);
			return 0;
        }
        fType->type.properties |= TYPE_MISC_PACKED | TYPE_MISC_PACKED_KW;
        return 1;
    }
    return 0;
} // SetTypePacked

/*
 * SetStorageClass() - Set the storage class of a type.  Issue an error if it's already set to
 *         a conflicting value.
 *
 * Returns: TRUE if O.K.
 *
 */

int SetStorageClass(SourceLoc *loc, dtype *fType, int storage)
{
    fType->type.properties;
    if (fType->storageClass == SC_UNKNOWN) {
        fType->storageClass = (StorageClass) storage;
    } else {
        if (fType->storageClass == (StorageClass) storage) {
            SemanticError(loc, ERROR___STORAGE_SPECIFIED_TWICE);
            return 0;
        } else {
            SemanticError(loc, ERROR___CONFLICTING_STORAGE);
            return 0;
        }
    }
    return 1;
} // SetStorageClass

/********************************** Parser Semantic Rules: ***********************************/

/*
 * Initializer() - Create an EXPR_LIST_OP node with an expression argument.
 *
 */
 
expr *Initializer(SourceLoc *loc, expr *fExpr)
{
    expr *lExpr;

    lExpr = (expr *) NewBinopNode(EXPR_LIST_OP, fExpr, NULL);
    return lExpr;
} // Initilaizer

/*
 * InitializerList() - Add an expression to a list of expressions.  Either can be NULL.
 *
 * Assumes that the nodes on list are EXPR_LIST_OP binary nodes.
 *
 */
 
expr *InitializerList(SourceLoc *loc, expr *list, expr *last)
{
    expr *lExpr;

    if (list) {
        if (last) {
            lExpr = list;
            while (lExpr->bin.right)
                lExpr = lExpr->bin.right;
            lExpr->bin.right = last;
        }
        return list;
    } else {
        return last;
    }
} // InitializerList

/*
 * ArgumentList() - Add an actual argument to a list of parameters.
 *
 */

expr *ArgumentList(SourceLoc *loc, expr *flist, expr *fExpr)
{
    expr *lExpr, *nExpr;

    nExpr = (expr *) NewBinopNode(FUN_ARG_OP, fExpr, NULL);
    nExpr->common.type = fExpr->common.type;
    nExpr->common.IsLValue = IsLValue(fExpr);
    if (GetQualifiers(nExpr->common.type) & TYPE_QUALIFIER_CONST) {
        nExpr->common.IsConst = 1;
    } else {
        nExpr->common.IsConst = 0;
    }
    if (flist) {
        lExpr = flist;
        while (lExpr->bin.right)
            lExpr = lExpr->bin.right;
        lExpr->bin.right = nExpr;
        return flist;
    } else {
        return nExpr;
    }
} // ArgumentList

/*
 * ExpressionList() - Add an expression to the end of a list of expressions..
 *
 */

expr *ExpressionList(SourceLoc *loc, expr *fList, expr *fExpr)
{
    expr *lExpr, *nExpr;

    nExpr = (expr *) NewBinopNode(EXPR_LIST_OP, fExpr, NULL);
    nExpr->common.type = fExpr->common.type;
    if (fList) {
        lExpr = fList;
        while (lExpr->bin.right)
            lExpr = lExpr->bin.right;
        lExpr->bin.right = nExpr;
        return fList;
    } else {
        return nExpr;
    }
} // ExpressionList

/*
 * AddDecl() - Add a declaration to a list of declarations.  Either can be NULL.
 *
 */
 
decl *AddDecl(decl *first, decl *last)
{
    decl *lDecl;

    if (first) {
        if (last) {
            lDecl = first;
            while (lDecl->next)
                lDecl = lDecl->next;
            lDecl->next = last;
        }
        return first;
    } else {
        return last;
    }
} // AddDecl

/*
 * AddStmt() - Add a list of statements to then end of another list.  Either can be NULL.
 *
 */
 
stmt *AddStmt(stmt *first, stmt *last)
{
    stmt *lStmt;

    if (first) {
        if (last) {
            lStmt = first;
            while (lStmt->exprst.next)
                lStmt = lStmt->exprst.next;
            lStmt->exprst.next = last;
        }
        return first;
    } else {
        return last;
    }
} // AddStmt

/*
 * CheckStatement() - See if this statement is supported by the target profile.
 *
 */

stmt *CheckStmt(stmt *fStmt)
{
    // Can't do it here.  Must wait until we know which functions are being used.
    //if (fStmt)
    //    theHAL->CheckStatement(&fStmt->commonst.loc, fStmt);
    return fStmt;
} // CheckStmt

/*
 * Function_Definition_Header() - Combine function <declaration_specifiers> and <declarator>.
 *
 */

decl *Function_Definition_Header(SourceLoc *loc, decl *fDecl)
{
    Symbol *lSymb = fDecl->symb;
    Symbol *formals;
    int ccount;
    int InProgram;
    int category, domain, qualifiers;
    Type *retType;

    if (IsFunction(lSymb)) {
        if (fDecl->type.type.properties & TYPE_MISC_ABSTRACT_PARAMS) {
            SemanticError(loc, ERROR_S_ABSTRACT_NOT_ALLOWED,
                          GetAtomString(atable, fDecl->name));
        }
        if (lSymb->details.fun.statements) {
            SemanticError(loc, ERROR_S_FUN_ALREADY_DEFINED,
                          GetAtomString(atable, fDecl->name));
        }
        if (Cg->theHAL->entryName == fDecl->name) {
            InProgram = 1;
            fDecl->type.type.properties |= TYPE_MISC_PROGRAM;
            lSymb->details.fun.locals->pid = Cg->theHAL->pid;
        } else {
            InProgram = 0;
        }
        retType = lSymb->type->fun.rettype;
        ccount = 0;
        formals = lSymb->details.fun.params;
        while (formals) {
            category = GetCategory(formals->type);
            domain = GetDomain(formals->type);
            qualifiers = GetQualifiers(formals->type);
            if (qualifiers & TYPE_QUALIFIER_OUT)
                lSymb->details.fun.HasOutParams = 1;
            formals = formals->next;
        }
#if 000 // Can't warn anymore -- could be writing to a global variable
        if (!lSymb->details.fun.HasOutParams && IsVoid(retType)) {
            SemanticWarning(loc, WARNING_S_VOID_FUN_HAS_NO_OUT_ARGS,
                          GetAtomString(atable, fDecl->name));
        }
#endif
        PushScope(lSymb->details.fun.locals); 
    } else {
        SemanticError(loc, ERROR_S_NOT_A_FUN,
                      GetAtomString(atable, fDecl->name));
        PushScope(NewScope());
    }
    CurrentScope->funindex = ++NextFunctionIndex;
    return fDecl;
} // Function_Definition_Header

/*
 * lCheckInitializationData() - Check data in an init_declarator for compatibility
 *         with variable.
 */

static int lCheckInitializationData(SourceLoc *loc, Type *vType, expr *dExpr, int IsGlobal)
{
    int category, base, ii, vlen, subop;
    expr *lExpr, *tExpr;

    if (!dExpr || !vType)
        return 0;
    base = GetBase(vType);
    category = GetCategory(vType);
    switch (category) {
    default:
    case TYPE_CATEGORY_NONE:
        return 0;
    case TYPE_CATEGORY_SCALAR:
        if (dExpr->common.kind == BINARY_N && dExpr->bin.op == EXPR_LIST_OP) {
            lExpr = FoldConstants(dExpr->bin.left);
            if (lExpr->common.kind == CONST_N) {
                if (ConvertType(lExpr, vType, lExpr->co.type, &tExpr, 1, 0)) {
                    dExpr->bin.left = tExpr;
                    return 1;
                } else {
                    SemanticError(loc, ERROR___INVALID_INITIALIZATION);
                    return 0;
                }
            } else {
#if 000 // RSG
                if (IsGlobal) {
                    SemanticError(loc, ERROR___NON_CONST_INITIALIZATION);
                    return 0;
                } else {
#endif // RSG
                    return 1;
#if 000 // RSG
                }
#endif // RSG
            }
        } else {
            SemanticError(loc, ERROR___INVALID_INITIALIZATION);
            return 0;
        }
    case TYPE_CATEGORY_ARRAY:
        vlen = vType->arr.numels;
        if (dExpr->common.kind == BINARY_N && dExpr->bin.op == EXPR_LIST_OP) {
            lExpr = dExpr->bin.left;
            if (!lExpr) {
                SemanticError(loc, ERROR___TOO_LITTLE_DATA);
                return 0;
            }
            if (lExpr->common.kind == BINARY_N && lExpr->bin.op == EXPR_LIST_OP) {
                for (ii = 0; ii < vlen; ii++) {
                    if (lExpr) {
                        if (lExpr->common.kind == BINARY_N && lExpr->bin.op == EXPR_LIST_OP) {
                            if (lCheckInitializationData(loc, vType->arr.eltype, lExpr, IsGlobal)) {
                                /* O.K. */
                            } else {
                                return 0;
                            }
                        } else {
                            SemanticError(loc, ERROR___INVALID_INITIALIZATION);
                            return 0;
                        }
                    } else {
                        SemanticError(loc, ERROR___TOO_LITTLE_DATA);
                        return 0;
                    }
                    lExpr = lExpr->bin.right;
                }
                if (lExpr) {
                    SemanticError(loc, ERROR___TOO_MUCH_DATA);
                } else {
                    subop = SUBOP_V(vlen, GetBase(vType));
                    dExpr->bin.left = (expr *) NewUnopSubNode(VECTOR_V_OP, subop, dExpr->bin.left);
                    dExpr->bin.left->un.type = GetStandardType(GetBase(vType), vlen, 0);
                    return 1;
                }
            } else {
                if (ConvertType(lExpr, vType, lExpr->common.type, &tExpr, 0, 0)) {
                    dExpr->bin.left = tExpr;
                    return 1;
                } else {
                    SemanticError(loc, ERROR___INCOMPAT_TYPE_INIT);
                    return 0;
                }
            }
        } else {
            SemanticError(loc, ERROR___INVALID_INITIALIZATION);
        }
        return 0;
    case TYPE_CATEGORY_FUNCTION:
    case TYPE_CATEGORY_STRUCT:
    case TYPE_CATEGORY_CONNECTOR:
        SemanticError(loc, ERROR___INVALID_INITIALIZATION);
        return 0;
    }
} // lCheckInitializationData

/*
 * Param_Init_Declarator() - Process a parameter initialization declarator.
 *
 */

decl *Param_Init_Declarator(SourceLoc *loc, Scope *fScope, decl *fDecl, expr *fExpr)
{
    Type *lType;

    if (fDecl) {
        lType = &fDecl->type.type;
        if (IsVoid(lType)) {
            SemanticError(loc, ERROR_S_VOID_TYPE_INVALID,
                          GetAtomString(atable, fDecl->name));
        }
        if (GetCategory(lType) == TYPE_CATEGORY_FUNCTION) {
            SemanticError(loc, ERROR_S_FUN_TYPE_INVALID,
                          GetAtomString(atable, fDecl->name));
        }
        if (fExpr) {
            if (GetDomain(lType) != TYPE_DOMAIN_UNIFORM) {
                SemanticError(loc, ERROR_S_NON_UNIFORM_PARAM_INIT,
                              GetAtomString(atable, fDecl->name));
            }
            if (lCheckInitializationData(loc, lType, fExpr, 0)) {
                fDecl->initexpr = fExpr;
            }
        }
    }
    return fDecl;
} // Param_Init_Declarator

/*
 * Init_Declarator() - Set initial value and/or semantics for this declarator.
 *
 */

stmt *Init_Declarator(SourceLoc *loc, Scope *fScope, decl *fDecl, expr *fExpr)
{
    int category, len, len2, base;
    stmt *lStmt = NULL;
    int IsGlobal, IsStatic, IsUniform, IsParam, DontAssign;
    Symbol *lSymb;
    expr *lExpr;
    Type *lType;

    if (fDecl) {
        lSymb = fDecl->symb;
        if (fExpr) {
            if (lSymb->kind != VARIABLE_S) {
                SemanticError(loc, ERROR_S_INIT_NON_VARIABLE,
                            GetAtomString(atable, lSymb->name));
            } else if (fScope->IsStructScope) {
                SemanticError(loc, ERROR_S_INIT_STRUCT_MEMBER,
                            GetAtomString(atable, lSymb->name));
            } else if (lSymb->storageClass == SC_EXTERN) {
                SemanticError(loc, ERROR_S_INIT_EXTERN,
                            GetAtomString(atable, lSymb->name));
            } else {
                lType = lSymb->type;
                IsGlobal = fScope->level <= 1;
                IsStatic = lSymb->storageClass == SC_STATIC;
                IsUniform = GetDomain(lType) == TYPE_DOMAIN_UNIFORM;
                IsParam = lSymb->properties & SYMB_IS_PARAMETER;
                if (IsGlobal && !IsStatic) {
                    DontAssign = 1;
                } else if (IsParam) {
                    DontAssign = 1;
                } else {
                    DontAssign = 0;
                }
                if (lCheckInitializationData(loc, lType, fExpr, IsGlobal)) {
                    category = GetCategory(lType);
                    base = GetBase(lType);
                    switch (category) {
                    default:
                    case TYPE_CATEGORY_NONE:
                        SemanticError(loc, ERROR___INVALID_INITIALIZATION);
                        break;
                    case TYPE_CATEGORY_SCALAR:
                        assert(fExpr->common.kind == BINARY_N && fExpr->bin.op == EXPR_LIST_OP);
                        if (DontAssign) {
                            lSymb->details.var.init = fExpr;
                        } else {
                            lExpr = (expr *) NewSymbNode(VARIABLE_OP, lSymb);
                            lStmt = NewSimpleAssignmentStmt(loc, lExpr, fExpr->bin.left, 1);
                        }
                        break;
                    case TYPE_CATEGORY_ARRAY:
                        if (IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
                            assert(fExpr->common.kind == BINARY_N && fExpr->bin.op == EXPR_LIST_OP);
                            if (DontAssign) {
                                lSymb->details.var.init = fExpr;
                            } else {
                                lExpr = (expr *) NewSymbNode(VARIABLE_OP, lSymb);
                                lStmt = NewSimpleAssignmentStmt(loc, lExpr, fExpr->bin.left, 1);
                            }
                        } else {
                            SemanticError(loc, ERROR___ARRAY2_INIT_NOT_DONE);
                        }
                        break;
                    }
                }
            }
        }
        if (lSymb->kind == FUNCTION_S) {
            lSymb = lSymb->details.fun.params;
            while (lSymb) {
                if (lSymb->kind == VARIABLE_S) {
                    if (lSymb->details.var.semantics)
                        SemanticWarning(loc, WARNING_S_FORWARD_SEMANTICS_IGNORED,
                                        GetAtomString(atable, lSymb->name));
                }
                lSymb = lSymb->next;
            }
        }
    }
    return lStmt;
} // Init_Declarator

/*
 * Declarator() - Process a declarator.
 *
 */

decl *Declarator(SourceLoc *loc, decl *fDecl, int semantics)
{
    Symbol *lSymb, *params;
    Scope *lScope;
    Type *lType;

    if (CurrentScope->InFormalParameters) {
        /*
         * Don't add formal parameters to the symbol table until we're
         * sure that we're in a function declaration.
         *
         */
        if (fDecl->type.storageClass != SC_UNKNOWN)
            SemanticError(&fDecl->loc, ERROR_S_STORAGE_NOT_ALLOWED,
                          GetAtomString(atable, fDecl->name));
        fDecl->semantics = semantics;
    } else {
        lSymb = LookUpLocalSymbol(CurrentScope, fDecl->name);
        if (!lSymb) {
            lType = GetTypePointer(&fDecl->loc, &fDecl->type);
            if (IsVoid(lType)) {
                SemanticError(&fDecl->loc, ERROR_S_VOID_TYPE_INVALID,
                              GetAtomString(atable, fDecl->name));
            }
            if (fDecl->type.type.properties & TYPE_MISC_TYPEDEF) {
                lSymb = DefineTypedef(loc, CurrentScope, fDecl->name, lType);
                if (semantics)
                    SemanticError(loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                                GetAtomString(atable, fDecl->name));
                if (fDecl->type.storageClass != SC_UNKNOWN)
                    SemanticError(&fDecl->loc, ERROR_S_STORAGE_NOT_ALLOWED_TYPEDEF,
                         GetAtomString(atable, fDecl->name));
            } else {
                if (GetQualifiers(&fDecl->type.type) & TYPE_QUALIFIER_INOUT) {
                    SemanticError(&fDecl->loc, ERROR_S_IN_OUT_PARAMS_ONLY,
                                  GetAtomString(atable, fDecl->name));
                }
                if (GetCategory(&fDecl->type.type) == TYPE_CATEGORY_FUNCTION) {
                    lScope = NewScope();
                    params = AddFormalParamDecls(lScope, fDecl->params);
                    lSymb = DeclareFunc(&fDecl->loc, CurrentScope, NULL, fDecl->name, lType, lScope, params);
                    if (semantics)
                        SemanticError(loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                                    GetAtomString(atable, fDecl->name));
                } else {
                    if (fDecl->type.type.properties & TYPE_MISC_INTERNAL) {
                        SemanticError(&fDecl->loc, ERROR_S_INTERNAL_FOR_FUN,
                                      GetAtomString(atable, fDecl->name));
                    }
                    if (fDecl->type.type.properties & TYPE_MISC_INLINE) {
                        SemanticError(&fDecl->loc, ERROR_S_INLINE_FOR_FUN,
                                      GetAtomString(atable, fDecl->name));
                    }
                    if (IsUnsizedArray(lType)) {
                        SemanticError(&fDecl->loc, ERROR_S_UNSIZED_ARRAY,
                                      GetAtomString(atable, fDecl->name));
                    }
                    if (IsCategory(lType, TYPE_CATEGORY_ARRAY) && !IsPacked(lType)) {
                        if (!Cg->theHAL->GetCapsBit(CAPS_INDEXED_ARRAYS)) {
                            // XYZZY - This test needs to be moved to later to support multiple profiles
                            SemanticError(&fDecl->loc, ERROR_S_UNPACKED_ARRAY,
                                          GetAtomString(atable, fDecl->name));
                        }
                    }
                    lSymb = DefineVar(loc, CurrentScope, fDecl->name, lType);
                    lSymb->storageClass = fDecl->type.storageClass;
                    if (semantics) {
                        if (CurrentScope->IsStructScope) {
                            CurrentScope->HasSemantics = 1;
                        } else {
                            if (CurrentScope->level > 1) {
                                SemanticError(&fDecl->loc, ERROR_S_NO_LOCAL_SEMANTICS,
                                              GetAtomString(atable, fDecl->name));
                            } else if (fDecl->type.storageClass == SC_STATIC) {
                                SemanticError(&fDecl->loc, ERROR_S_STATIC_CANT_HAVE_SEMANTICS,
                                              GetAtomString(atable, fDecl->name));
#if 000 // RSG -- Not sure if this is true.  Do non-static global variables with semantics have to be declared "uniform"?
                            } else if (GetDomain(&fDecl->type.type) != TYPE_DOMAIN_UNIFORM) {
                                SemanticError(&fDecl->loc, ERROR_S_NON_STATIC_SEM_NOT_UNIFORM,
                                              GetAtomString(atable, fDecl->name));
#endif // RSG
                            }
                        }
                        lSymb->details.var.semantics = semantics;
                    }
                    if (CurrentScope->level == 1) {
                        if (fDecl->type.storageClass != SC_STATIC &&
                            GetDomain(&fDecl->type.type) != TYPE_DOMAIN_VARYING)
                        {
                            lSymb->properties |= SYMB_NEEDS_BINDING;
                        }
                    }
                    if (CurrentScope->IsStructScope)
                        AddParameter(CurrentScope, lSymb);
                }
            }
        } else {
            if (GetCategory(&fDecl->type.type) == TYPE_CATEGORY_FUNCTION) {
                lType = GetTypePointer(&fDecl->loc, &fDecl->type);
                lScope = NewScope();
                params = AddFormalParamDecls(lScope, fDecl->params);
                lSymb = DeclareFunc(&fDecl->loc, CurrentScope, lSymb, fDecl->name, lType, lScope, params);
                lSymb->storageClass = fDecl->type.storageClass;
                if (semantics)
                    SemanticError(loc, ERROR_S_SEMANTICS_NON_VARIABLE,
                                GetAtomString(atable, fDecl->name));
            } else {
                if (!IsTypeBase(&fDecl->type.type, TYPE_BASE_UNDEFINED_TYPE)) {
                    SemanticError(&fDecl->loc, ERROR_S_NAME_ALREADY_DEFINED,
                                  GetAtomString(atable, fDecl->name));
                }
            }
        }
        fDecl->symb = lSymb;
    }
    return fDecl;
} // Declarator

/*
 * lInsertDimension() - Insert a dimension below dims levels.
 *
 */

 static int lInsertDimension(SourceLoc *loc, dtype *fDtype, int dims, int fnumels, int Packed)
 {
    int lnumels, lproperties;
    Type *lType, *elType;

    if (dims == 0) {
        fDtype->IsDerived = 0;
        lnumels = fnumels;
    } else {
        lType = &fDtype->type;
        if (IsArray(lType)) {
            lnumels = lType->arr.numels;
            lproperties = fDtype->type.arr.properties & TYPE_MISC_MASK;
            //lsize = lType->arr.size;
            elType = lType->arr.eltype;
            fDtype->type = *elType;
            if (!lInsertDimension(loc, fDtype, dims - 1, fnumels, Packed))
                return 0;  // error encountered below
            fDtype->type.arr.properties |= lproperties;
        } else {
            return 0;
        }
    }
    lType = GetTypePointer(loc, fDtype);
    SetTypeCategory(loc, 0, fDtype, TYPE_CATEGORY_ARRAY, 1);
    fDtype->type.arr.eltype = lType;
    fDtype->type.arr.numels = lnumels;
    fDtype->numNewDims = dims + 1;
	if (Packed) {
		fDtype->type.properties |= TYPE_MISC_PACKED;
	} else {
		fDtype->type.properties &= ~TYPE_MISC_PACKED;
	}
    fDtype->IsDerived = 1;
    return 1;
 } // lInsertDimension

/*
 * Array_Declarator() - Declare an array of this type.
 *
 */

decl *Array_Declarator(SourceLoc *loc, decl *fDecl, int size, int Empty)
{
    dtype *lDtype;
    Type *lType;
    int dims;

    lDtype = &fDecl->type;
    if (size <= 0 && !Empty) {
        SemanticError(loc, ERROR___DIMENSION_LT_1);
        size = 1;
    }
    if (IsVoid(&lDtype->type))
        SemanticError(loc, ERROR___ARRAY_OF_VOID);
    switch (GetCategory(&lDtype->type)) {
    case TYPE_CATEGORY_SCALAR:
        lType = lDtype->basetype;
        SetTypeCategory(loc, 0, lDtype, TYPE_CATEGORY_ARRAY, 1);
        lDtype->type.arr.eltype = lType;
        lDtype->type.arr.numels = size;
        lDtype->numNewDims = 1;
        break;
    case TYPE_CATEGORY_ARRAY:
        dims = lDtype->numNewDims;
        lInsertDimension(loc, lDtype, dims, size, lDtype->type.properties & TYPE_MISC_PACKED_KW);
        // if (TotalNumberDimensions > MAX_ARRAY_DIMENSIONS)
        //    SemanticError(loc, ERROR_D_EXCEEDS_MAX_DIMS, MAX_ARRAY_DIMENSIONS);
        break;
    case TYPE_CATEGORY_FUNCTION:
        SemanticError(loc, ERROR___ARRAY_OF_FUNS);
        break;
    case TYPE_CATEGORY_STRUCT:
        lType = GetTypePointer(loc, lDtype);
        NewDType(lDtype, lType, TYPE_CATEGORY_ARRAY);
        lDtype->type.co.properties |= lType->co.properties & (TYPE_DOMAIN_MASK | TYPE_QUALIFIER_MASK);
        lDtype->type.arr.eltype = lType;
        lDtype->type.arr.numels = size;
        lDtype->numNewDims = 1;
        break;
    default:
        InternalError(loc, 999, "ArrayDeclarator(): unknown category");
        break;
    }
    lDtype->IsDerived = 1;
    return fDecl;
} // Array_Declarator

/*
 * AddFormalParamDecls() - Add a list of formal parameter declarations to a function
 *         definition's scope.
 */

Symbol *AddFormalParamDecls(Scope *fScope, decl *params)
{
    Symbol *lSymb, *first = NULL, *last;
    Type *lType;

    while (params) {
        lSymb = LookUpLocalSymbol(fScope, params->name);
        if (lSymb) {
            SemanticError(&params->loc, ERROR_S_PARAM_NAME_TWICE,
                          GetAtomString(atable, params->name));
        } else {
            lSymb = AddSymbol(&params->loc, fScope, params->name,
                              GetTypePointer(&params->loc, &params->type), VARIABLE_S);
            lSymb->properties |= SYMB_IS_PARAMETER;
            lSymb->details.var.semantics = params->semantics;
            lSymb->details.var.init = params->initexpr;
            if (first) {
                last->next = lSymb;
            } else {
                first = lSymb;
            }
            last = lSymb;
            lType = lSymb->type;
            if (IsCategory(lType, TYPE_CATEGORY_ARRAY) && !IsPacked(lType)) {
                if (!Cg->theHAL->GetCapsBit(CAPS_INDEXED_ARRAYS)) {
                    SemanticError(&params->loc, ERROR_S_UNPACKED_ARRAY,
                                  GetAtomString(atable, params->name));
                }
            }
        }
        params = params->next;
    }
    return first;
} // AddFormalParamDecls

/*
 * SetFunTypeParams() - Build a list of types and set this function type's abstract parameter types.
 *
 */

decl *SetFunTypeParams(Scope *fScope, decl *func, decl *params, decl *actuals)
{
    TypeList *formals, *prev, *lType;

    fScope->InFormalParameters--;
    formals = prev = NULL;
    while (params) {
        lType = (TypeList *) malloc(sizeof(TypeList));
        lType->next = NULL;
        lType->type = GetTypePointer(&params->loc, &params->type);
        if (formals) {
            prev->next = lType;
        } else {
            formals = lType;
        }
        prev = lType;
        params = params->next;
    }
    if (func && IsCategory(&func->type.type, TYPE_CATEGORY_FUNCTION)) {
        func->type.type.fun.paramtypes = formals;
        func->type.IsDerived = 1;
    }
    if (actuals) {
        func->params = actuals;
    } else {
        if (!CurrentScope->HasVoidParameter)
            func->type.type.properties |= TYPE_MISC_ABSTRACT_PARAMS;
    }
    return func;
} // SetFunTypeParams


/*
 * FunctionDeclHeader()
 *
 */

decl *FunctionDeclHeader(SourceLoc *loc, Scope *fScope, decl *func)
{
    Type *rtnType = GetTypePointer(Cg->tokenLoc, &func->type);

    if (IsUnsizedArray(rtnType))
        SemanticError(loc, ERROR_S_UNSIZED_ARRAY, GetAtomString(atable, func->name));
    NewDType(&func->type, NULL, TYPE_CATEGORY_FUNCTION);
    CurrentScope->InFormalParameters++;
    func->type.type.properties |= rtnType->properties & (TYPE_MISC_INLINE | TYPE_MISC_INTERNAL);
    rtnType->properties &= ~(TYPE_MISC_INLINE | TYPE_MISC_INTERNAL);
    func->type.type.fun.paramtypes = NULL;
    func->type.type.fun.rettype = rtnType;
    func->type.IsDerived = 1;
    return func;
} // FunctionDeclHeader

/*
 * StructHeader() - Process a struct header.
 *
 */

Type *StructHeader(SourceLoc *loc, Scope *fScope, int cType, int tag)
{
    Symbol *lSymb;
    Type *lType;

    if (tag) {
        lSymb = LookUpTag(fScope, tag);
        if (!lSymb) {
            lSymb = AddTag(loc, fScope, tag, TYPE_CATEGORY_STRUCT);
            lSymb->type->str.tag = tag;
            lSymb->type->str.semantics = cType;
            lSymb->type->str.variety = CID_NONE_ID;
        }
        lType = lSymb->type;
        if (!IsCategory(lType, TYPE_CATEGORY_STRUCT)) {
            SemanticError(loc, ERROR_S_TAG_IS_NOT_A_STRUCT, GetAtomString(atable, tag));
            lType = UndefinedType;
        }
    } else {
        lType = NewType(TYPE_CATEGORY_STRUCT, 0);
    }
    return lType;
} // StructOrConnectorHeader

/*
 * DefineVar() - Define a new variable in the current scope.
 *
 */

Symbol *DefineVar(SourceLoc *loc, Scope *fScope, int atom, Type *fType)
{
    Symbol *lSymb;

    lSymb = AddSymbol(loc, fScope, atom, fType, VARIABLE_S);
    return lSymb;
} // DefineVar

/*
 * DefineTypedef() - Define a new type name in the current scope.
 *
 */

Symbol *DefineTypedef(SourceLoc *loc, Scope *fScope, int atom, Type *fType)
{
    return AddSymbol(loc, fScope, atom, fType, TYPEDEF_S);
} // DefineTypedef

/*
 * DeclareFunc() - Declare an identifier as a function in the scope fScope.  If it's already
 *         in the symbol table check the overloading rules to make sure that it either
 *         A) matches a previous declaration exactly, or B) is unambiguously resolvable.
 */

Symbol *DeclareFunc(SourceLoc *loc, Scope *fScope, Symbol *fSymb, int atom, Type *fType,
                    Scope *locals, Symbol *params)
{
    int DiffParamTypes, DiffParamQualifiers, DiffParamCount, DiffReturnType;
    TypeList *oldArgType, *newArgType;
    Symbol *lSymb;
    int index, group, OK;

    if (fSymb) {
        if (GetCategory(fSymb->type) != TYPE_CATEGORY_FUNCTION) {
            SemanticError(loc, ERROR_S_NAME_ALREADY_DEFINED, GetAtomString(atable, atom));
            lSymb = fSymb;
        } else {
            OK = 1;
            lSymb = fSymb;
            while (lSymb) {
                if (GetCategory(fSymb->type) != TYPE_CATEGORY_FUNCTION) {
                    InternalError(loc, ERROR_S_SYMBOL_TYPE_NOT_FUNCTION,
                                  GetAtomString(atable, lSymb->name));
                    return fSymb;
                }
                DiffParamTypes = DiffParamQualifiers = DiffParamCount = DiffReturnType = 0;
                if (!IsSameUnqualifiedType(lSymb->type->fun.rettype, fType->fun.rettype))
                    DiffReturnType = 1;
                oldArgType = lSymb->type->fun.paramtypes;
                newArgType = fType->fun.paramtypes;
                while (newArgType && oldArgType) {
                    if (!IsSameUnqualifiedType(oldArgType->type, newArgType->type)) {
                        DiffParamTypes = 1;
                    } else if (GetQualifiers(oldArgType->type) != GetQualifiers(newArgType->type)) {
                        DiffParamQualifiers = 1;
                    }
                    oldArgType = oldArgType->next;
                    newArgType = newArgType->next;
                }
                if (newArgType || oldArgType)
                    DiffParamCount = 1;
                if (!DiffParamCount && !DiffParamTypes) {
                    if (DiffParamQualifiers) {
                        SemanticError(loc, ERROR_S_OVERLOAD_DIFF_ONLY_QUALS,
                                      GetAtomString(atable, atom));
                        OK = 0;
                        break;
                    }
                    if (DiffReturnType) {
                        SemanticError(loc, ERROR_S_OVERLOAD_DIFF_ONLY_RETURN,
                                      GetAtomString(atable, atom));
                        OK = 0;
                        break;
                    }
                    break; // Found the matching function
                }
                lSymb = lSymb->details.fun.overload;
            }
            if (OK) {
                if (DiffParamCount || DiffParamTypes) {
                    lSymb = NewSymbol(loc, fScope, atom, fType, FUNCTION_S);
                    lSymb->details.fun.params = params;
                    lSymb->details.fun.locals = locals;
                    lSymb->details.fun.overload = fSymb->details.fun.overload;
                    fSymb->details.fun.overload = lSymb;
                    if (GetCategory(fType) == TYPE_CATEGORY_FUNCTION) {
                        locals->returnType = fType->fun.rettype;
                    } else {
                        locals->returnType = UndefinedType;
                    }
                } else {
                    if (!(lSymb->properties & SYMB_IS_DEFINED)) {
                        // Overwrite previous definitions if this function is not yet defined.
                        // Prototype parameter names are ignored.
                        lSymb->details.fun.params = params;
                        lSymb->details.fun.locals = locals;
                    } else {
                        // Declarator for a function that's already been defined.  Not an error.
                    }
                }
            } else {
                // Found a function that differs only by qualifiers or return type.  Error arleady issued.
                // lSymb = fSymb;
            }
        }
    } else {
        lSymb = AddSymbol(loc, fScope, atom, fType, FUNCTION_S);
        lSymb->details.fun.params = params;
        lSymb->details.fun.locals = locals;
        if (GetCategory(fType) == TYPE_CATEGORY_FUNCTION) {
            locals->returnType = fType->fun.rettype;
        } else {
            locals->returnType = UndefinedType;
        }
    }
    if (lSymb->type->properties & TYPE_MISC_INTERNAL) {
        index = Cg->theHAL->CheckInternalFunction(lSymb, &group);
        if (index) {
            //
            // lSymb->InternalIndex = index; etc.
            //
            lSymb->properties |= SYMB_IS_DEFINED | SYMB_IS_BUILTIN;
            lSymb->details.fun.group = group;
            lSymb->details.fun.index = index;
        } else {
            SemanticError(loc, ERROR_S_INVALID_INTERNAL_FUNCTION, GetAtomString(atable, atom));
        }
    }

    return lSymb;
} // DeclareFunc

/*
 * DefineFunction() - Set the body of the function "func" to the statements in "body".
 *
 */

void DefineFunction(SourceLoc *loc, Scope *fScope, decl *func, stmt *body)
{
    Symbol *lSymb = func->symb;
    SymbolList *lSymbList;
    Scope *globals;
    Type *lType;

    if (IsFunction(lSymb)) {
        if (body) {
            if (lSymb->properties & SYMB_IS_DEFINED) {
                SemanticError(loc, ERROR_S_FUN_ALREADY_DEFINED,
                              GetAtomString(atable, lSymb->name));
            } else {
                lSymb->properties |= SYMB_IS_DEFINED;
                lSymb->details.fun.statements = body;
                lType = lSymb->type;
                if ((lType->properties & TYPE_MISC_INLINE) ||
                    Cg->theHAL->GetCapsBit(CAPS_INLINE_ALL_FUNCTIONS))
                {
                    lSymb->properties |= SYMB_IS_INLINE_FUNCTION;
                }
            }
            if (!fScope->HasReturnStmt && !IsVoid(fScope->returnType)) {
                SemanticError(loc, ERROR_S_FUNCTION_HAS_NO_RETURN,
                              GetAtomString(atable, lSymb->name));
            }
            if (func->type.type.properties & TYPE_MISC_PROGRAM) {
                globals = fScope->parent;
                if (!globals->programs) {
                    lSymbList = (SymbolList *) malloc(sizeof(SymbolList));
                    lSymbList->next = globals->programs;
                    lSymbList->symb = lSymb;
                    globals->programs = lSymbList;
                } else {
                    SemanticError(loc, ERROR_S_ONE_PROGRAM,
                                  GetAtomString(atable, globals->programs->symb->name));
                }
            }
        } else {
            SemanticError(loc, ERROR_S_NO_STATEMENTS, GetAtomString(atable, func->name));
        }
        if (Cg->options.DumpParseTree) {
            PrintScopeDeclarations();
            PrintFunction(lSymb);
        }
    }
} // DefineFunction

/*
 * GlobalInitStatements()
 *
 */

int GlobalInitStatements(Scope *fScope, stmt *fStmt)
{
    stmt *lStmt;

    if (fStmt) {
        if (fScope->initStmts) {
            lStmt = fScope->initStmts;
            while (lStmt->commonst.next)
                lStmt = lStmt->commonst.next;
            lStmt->commonst.next = fStmt;
        } else {
            fScope->initStmts = fStmt;
        }
    }
    return 1;
} // GlobalInitStatements

/*
 * BasicVariable() - A variable identifier has been encountered.
 *
 */

expr *BasicVariable(SourceLoc *loc, int name)
{
    Symbol *lSymb;
    
    lSymb = LookUpSymbol(CurrentScope, name);
    if (!lSymb) {
        SemanticError(loc, ERROR_S_UNDEFINED_VAR, GetAtomString(atable, name));
        lSymb = DefineVar(loc, CurrentScope, name, UndefinedType);
    }
    return (expr *) NewSymbNode(VARIABLE_OP, lSymb);
} // BasicVariable

/*
 * IsLValue() - Is this expression an l-value?
 *
 */

int IsLValue(const expr *fExpr)
{
    if (fExpr) {
        return fExpr->common.IsLValue;
    } else {
        return 0;
    }
} // IsLValue

/*
 * IsConst() - Is this expression an l-value?
 *
 */

int IsConst(const expr *fExpr)
{
    if (fExpr) {
        return fExpr->common.IsConst;
    } else {
        return 0;
    }
} // IsConst

/*
 * IsArrayIndex() - Is this expression an array index expression?
 *
 */

int IsArrayIndex(const expr *fExpr)
{
    if (fExpr) {
        return fExpr->common.kind == BINARY_N && fExpr->bin.op == ARRAY_INDEX_OP;
    } else {
        return 0;
    }
} // IsArrayIndex

/*
 * lIsBaseCastValid() - Is it O.K. to cast the base type fromBase to toBase?
 *
 */

static int lIsBaseCastValid(int toBase, int fromBase, int Explicit)
{
    if (toBase == TYPE_BASE_NO_TYPE || fromBase == TYPE_BASE_NO_TYPE)
        return 0;
    if (toBase == TYPE_BASE_VOID || fromBase == TYPE_BASE_VOID)
        return 0;
    if (toBase == fromBase)
        return 1;
    if (Cg->theHAL->IsValidScalarCast(toBase, fromBase, Explicit)) {
        return 1;
    } else {
        return 0;
    }
} // lIsBaseCastValid

/*
 * ConvertType() - Type cast fExpr from fromType to toType if needed.  Ignore qualifiers.
 *
 * If "result" is NULL just check validity of cast; don't allocate cast operator node.
 *
 */

int ConvertType(expr *fExpr, Type *toType, Type *fromType, expr **result, int IgnorePacked, int Explicit)
{
    int fcategory, tcategory;
    int fbase, tbase;
    Type *feltype, *teltype;
    unary *unnode;
    int ToPacked, FromPacked;

    ToPacked = (toType->properties & TYPE_MISC_PACKED) != 0;
    FromPacked = (fromType->properties & TYPE_MISC_PACKED) != 0;
    if (IsSameUnqualifiedType(toType, fromType) &&
        ((ToPacked == FromPacked) || IgnorePacked))
    {
        if (result)
            *result = fExpr;
        return 1;
    } else {
        fcategory = GetCategory(fromType);
        tcategory = GetCategory(toType);
        if (fcategory == tcategory) {
            switch (fcategory) {
            case TYPE_CATEGORY_SCALAR:
                fbase = GetBase(fromType);
                tbase = GetBase(toType);
                if (lIsBaseCastValid(tbase, fbase, Explicit)) {
                    if (result) {
                        unnode = NewUnopSubNode(CAST_CS_OP, SUBOP_CS(tbase, fbase), fExpr);
                        unnode->type = GetStandardType(tbase, 0, 0);
                        unnode->HasSideEffects = fExpr->common.HasSideEffects;
                        *result = (expr *) unnode;
                    }
                    return 1;
                } else {
                    return 0;
                }
                break;
            case TYPE_CATEGORY_ARRAY:
                if (toType->arr.numels != fromType->arr.numels)
                    return 0;
                if (toType->arr.numels > 4)
                    return 0;
                if (!IgnorePacked && (ToPacked != FromPacked))
                    return 0;
                feltype = fromType->arr.eltype;
                teltype = toType->arr.eltype;
                fcategory = GetCategory(feltype);
                tcategory = GetCategory(teltype);
                if (tcategory != TYPE_CATEGORY_SCALAR || fcategory != TYPE_CATEGORY_SCALAR)
                    return 0;
                fbase = GetBase(feltype);
                tbase = GetBase(teltype);
                if (lIsBaseCastValid(tbase, fbase, Explicit)) {
                    if (result) {
                        unnode = NewUnopSubNode(CAST_CV_OP, SUBOP_CV(tbase, toType->arr.numels, fbase), fExpr);
                        unnode->type = GetStandardType(tbase, toType->arr.numels, 0);
                        unnode->HasSideEffects = fExpr->common.HasSideEffects;
                        *result = (expr *) unnode;
                    }
                    return 1;
                } else {
                    return 0;
                }
                break;
            default:
                return 0;
            }
        } else {
            return 0;
        }
    }
} // ConvertType

/*
 * CastScalarVectorMatrix() - Cast a scalar, vector, or matrix expression.
 *
 * Scalar: len = 0.
 * Vector: len >= 1 and len2 = 0
 * Matrix: len >= 1 and len2 >= 1
 *
 * len = 1 means "float f[1]" not "float f"
 *
 */

expr *CastScalarVectorMatrix(expr *fExpr, int fbase, int tbase, int len, int len2)
{
    int op, subop;
    expr *lExpr;

    if (len == 0) {
        op = CAST_CS_OP;
        subop = SUBOP_CS(tbase, fbase);
    } else if (len2 == 0) {
        op = CAST_CV_OP;
        subop = SUBOP_CV(tbase, len, fbase);
    } else {
        op = CAST_CM_OP;
        subop = SUBOP_CM(len2, tbase, len, fbase);
    }
    lExpr = (expr *) NewUnopSubNode(op, subop, fExpr);
    lExpr->common.type = GetStandardType(tbase, len, len2);
    return lExpr;
} // CastScalarVectorMatrix

/*
 * ConvertNumericOperands() - Convert two scalar, vector, or matrix expressions to the same type
 *         for use in an expression.  Number of dimensions and lengths may differ.
 *
 * Returns: base type of resulting values.
 *
 */

int ConvertNumericOperands(int baseop, expr **lExpr, expr **rexpr, int lbase, int rbase,
                           int llen, int rlen, int llen2, int rlen2)
{
    int nbase;

    nbase = Cg->theHAL->GetBinOpBase(baseop, lbase, rbase, llen, rlen);
    if (nbase != lbase)
        *lExpr = CastScalarVectorMatrix(*lExpr, lbase, nbase, llen, llen2);
    if (nbase != rbase)
        *rexpr = CastScalarVectorMatrix(*rexpr, rbase, nbase, rlen, rlen2);
    return nbase;
} // ConvertNumericOperands

/*
 * CheckBooleanExpr()
 *
 */

expr *CheckBooleanExpr(SourceLoc *loc, expr *fExpr, int AllowVector)
{
    int len = 0, HasError = 0;
    Type *lType, *leltype;

    lType = leltype = fExpr->common.type;
    if (IsScalar(lType)) {
        if (!IsBoolean(lType)) {
            SemanticError(loc, ERROR___BOOL_EXPR_EXPECTED);
            HasError = 1;
        }
    } else if (IsVector(lType, &len)) {
        leltype = lType->arr.eltype;
        if (AllowVector) {
            if (len > 4) {
                SemanticError(loc, ERROR___VECTOR_EXPR_LEN_GR_4);
                HasError = 1;
                len = 4;
            }
            if (!IsBoolean(lType)) {
                SemanticError(loc, ERROR___BOOL_EXPR_EXPECTED);
                HasError = 1;
            }
        } else {
            SemanticError(loc, ERROR___SCALAR_BOOL_EXPR_EXPECTED);
            HasError = 1;
        }
    } else {
        SemanticError(loc, ERROR___BOOL_EXPR_EXPECTED);
        HasError = 1;
    }
    if (HasError)
        fExpr->common.type = GetStandardType(TYPE_BASE_BOOLEAN, len, 0);
    return fExpr;
} // CheckBooleanExpr

/*
 * NewUnaryOperator() - See if this is a valid unary operation.  Return a new node with the
 *         proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   NEG      scalar  scalar  scalar
 *   NEG_V    vector  vector  vector
 *
 */

expr *NewUnaryOperator(SourceLoc *loc, int fop, int name, expr *fExpr, int IntegralOnly)
{
    int lop, subop = 0, HasError = 0, len = 0;
    int lbase;
    Type *lType, *eltype;
    unary *result = NULL;
    int MustBeBoolean, OK = 0;

    lop = fop;
    MustBeBoolean = fop == BNOT_OP ? 1 : 0;
    lType = eltype = fExpr->common.type;
    if (IsScalar(lType)) {
        subop = 0;
    } else if (IsVector(lType, &len)) {
        eltype = lType->arr.eltype;
        lop = fop + OFFSET_V_OP;
        subop = SUBOP_V(len, 0);
    } else {
        SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
        HasError = 1;
    }
    if (!HasError) {
        if (len > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, GetAtomString(atable, name));
        } else {
            lbase = GetBase(lType);
            SUBOP_SET_T(subop, lbase);
            if (MustBeBoolean) {
                if (lbase == TYPE_BASE_BOOLEAN) {
                    OK = 1;
                } else {
                    SemanticError(loc, ERROR___BOOL_EXPR_EXPECTED);
                }
            } else {
                if (Cg->theHAL->IsNumericBase(lbase)) {
                    if (IntegralOnly) {
                        if (Cg->theHAL->IsIntegralBase(lbase)) {
                            OK = 1;
                        } else {
                            SemanticError(loc, ERROR_S_OPERANDS_NOT_INTEGRAL, GetAtomString(atable, name));
                        }
                    } else {
                        OK = 1;
                    }
                } else {
                    SemanticError(loc, ERROR_S_OPERANDS_NOT_NUMERIC, GetAtomString(atable, name));
                }
            }
            if (OK) {
                result = NewUnopSubNode(lop, subop, fExpr);
                result->type = GetStandardType(lbase, len, 0);
            }
        }
    }
    if (!result) {
        result = NewUnopSubNode(lop, 0, fExpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewUnaryOperator

/*
 * NewBinaryOperator() - See if this is a valid binary operation.  Return a new node with the
 *         proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   MUL      scalar  scalar  scalar
 *   MUL_V    vector  vector  vector
 *   MUL_SV*  scalar  vector  vector
 *   MUL_VS*  vector  scalar  vector
 *
 *    *only allowed for smearing operators MUL, DIV, ADD, SUB.
 */

expr *NewBinaryOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rexpr, int IntegralOnly)
{
    int lop, subop = 0, HasError = 0, llen = 0, rlen = 0, nlen;
    int lbase, rbase, nbase;
    Type *lType, *rtype, *leltype, *reltype;
    binary *result = NULL;
    int CanSmear;

    lop = fop;
    CanSmear = fop == MUL_OP || fop == DIV_OP || fop == ADD_OP || fop == SUB_OP ? 1 : 0;
    lType = leltype = lExpr->common.type;
    rtype = reltype = rexpr->common.type;
    if (IsScalar(lType)) {
        if (IsScalar(rtype)) {
            subop = 0;
        } else if (IsVector(rtype, &rlen)) {
            if (CanSmear) {
                reltype = rtype->arr.eltype;
                lop = fop + OFFSET_SV_OP;
                subop = SUBOP_SV(rlen, 0);
            } else {
                SemanticError(loc, ERROR_S_SCALAR_OP_VECTOR_INVALID, GetAtomString(atable, name));
                HasError = 1;
            }
        } else {
            SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
            HasError = 1;
        }
    } else if (IsVector(lType, &llen)) {
        leltype = lType->arr.eltype;
        if (IsScalar(rtype)) {
            if (CanSmear) {
                lop = fop + OFFSET_VS_OP;
                subop = SUBOP_VS(llen, 0);
            } else {
                SemanticError(loc, ERROR_S_VECTOR_OP_SCALAR_INVALID, GetAtomString(atable, name));
                HasError = 1;
            }
        } else {
            if (IsVector(rtype, &rlen)) {
                reltype = rtype->arr.eltype;
                lop = fop + OFFSET_V_OP;
                subop = SUBOP_VS(llen, 0);
                if (llen != rlen) {
                    SemanticError(loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, GetAtomString(atable, name));
                    HasError = 1;
                }
            } else {
                SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
                HasError = 1;
            }
        }
    } else {
        SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
        HasError = 1;
    }
    if (!HasError) {
        if (llen > 4 || rlen > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, GetAtomString(atable, name));
        } else {
            lbase = GetBase(lType);
            rbase = GetBase(rtype);
            if (Cg->theHAL->IsNumericBase(lbase) && Cg->theHAL->IsNumericBase(rbase)) {
                nbase = ConvertNumericOperands(fop, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
                SUBOP_SET_T(subop, nbase);
                nlen = llen > rlen ? llen : rlen;
                result = NewBinopSubNode(lop, subop, lExpr, rexpr);
                result->type = GetStandardType(nbase, nlen, 0);
                if (IntegralOnly && !Cg->theHAL->IsIntegralBase(nbase)) {
                    SemanticError(loc, ERROR_S_OPERANDS_NOT_INTEGRAL, GetAtomString(atable, name));
                }
            } else {
                SemanticError(loc, ERROR_S_OPERANDS_NOT_NUMERIC, GetAtomString(atable, name));
            }
        }
    }
    if (!result) {
        result = NewBinopSubNode(lop, 0, lExpr, rexpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewBinaryOperator

/*
 * NewBinaryBooleanOperator() - See if this is a valid binary Boolean operator.  Return a new
 *         node with the proper operator description.
 *
 * Valid operators are:
 *
 *     op      arg1    arg2   result
 *   ------   ------  ------  ------
 *   BAND     scalar  scalar  scalar
 *   BAND_V   vector  vector  vector
 *
 */

expr *NewBinaryBooleanOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rexpr)
{
    int lop, subop = 0, HasError = 0, llen = 0, rlen = 0;
    int lbase, rbase;
    Type *lType, *rtype, *leltype, *reltype;
    binary *result = NULL;

    lop = fop;
    lType = leltype = lExpr->common.type;
    rtype = reltype = rexpr->common.type;
    if (IsScalar(lType)) {
        if (IsScalar(rtype)) {
            subop = SUBOP__(TYPE_BASE_BOOLEAN);
        } else {
            SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
            HasError = 1;
        }
    } else if (IsVector(lType, &llen)) {
        leltype = lType->arr.eltype;
        if (IsVector(rtype, &rlen)) {
            reltype = rtype->arr.eltype;
            lop = fop + OFFSET_V_OP;
            subop = SUBOP_V(llen, TYPE_BASE_BOOLEAN);
            if (llen != rlen) {
                SemanticError(loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, GetAtomString(atable, name));
                HasError = 1;
            }
        } else {
            SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
            HasError = 1;
        }
    } else {
        SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
        HasError = 1;
    }
    if (!HasError) {
        if (llen > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, GetAtomString(atable, name));
        } else {
            lbase = GetBase(lType);
            rbase = GetBase(rtype);
            if (lbase == TYPE_BASE_BOOLEAN && rbase == TYPE_BASE_BOOLEAN) {
                result = NewBinopSubNode(lop, subop, lExpr, rexpr);
                result->type = GetStandardType(TYPE_BASE_BOOLEAN, llen, 0);
            } else {
                SemanticError(loc, ERROR_S_OPERANDS_NOT_BOOLEAN, GetAtomString(atable, name));
            }
            if (lExpr->common.HasSideEffects || rexpr->common.HasSideEffects) {
                SemanticError(loc, ERROR_S_OPERANDS_HAVE_SIDE_EFFECTS, GetAtomString(atable, name));
            }
        }
    }
    if (!result) {
        result = NewBinopSubNode(lop, 0, lExpr, rexpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewBinaryBooleanOperator

/*
 * NewBinaryComparisonOperator() - See if this is a valid binary comparison.  Return a new node
 *         with the proper operator description.
 *
 * Valid operators are:
 *
 *    op     arg1    arg2   result
 *   ----   ------  ------  ------
 *   LT     scalar  scalar  scalar
 *   LT_V   vector  vector  vector
 *
 */

expr *NewBinaryComparisonOperator(SourceLoc *loc, int fop, int name, expr *lExpr, expr *rexpr)
{
    int lop, subop = 0, HasError = 0, llen = 0, rlen = 0, nlen = 0;
    int lbase, rbase, nbase;
    Type *lType, *rtype, *leltype, *reltype;
    binary *result = NULL;

    lop = fop;
    lType = leltype = lExpr->common.type;
    rtype = reltype = rexpr->common.type;
    if (IsScalar(lType)) {
        if (IsScalar(rtype)) {
            subop = 0;
        } else if (IsVector(rtype, &rlen)) {
            reltype = rtype->arr.eltype;
            lop = fop + OFFSET_SV_OP;
            subop = SUBOP_SV(rlen, 0);
        } else {
            SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
            HasError = 1;
        }
    } else if (IsVector(lType, &llen)) {
        leltype = lType->arr.eltype;
        if (IsScalar(rtype)) {
            lop = fop + OFFSET_VS_OP;
            subop = SUBOP_VS(llen, 0);
        } else if (IsVector(rtype, &rlen)) {
            reltype = rtype->arr.eltype;
            lop = fop + OFFSET_V_OP;
            subop = SUBOP_V(llen, 0);
            if (llen != rlen) {
                SemanticError(loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, GetAtomString(atable, name));
                HasError = 1;
            }
            nlen = llen;
        } else {
            SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
            HasError = 1;
        }
    } else {
        SemanticError(loc, ERROR_S_INVALID_OPERANDS, GetAtomString(atable, name));
        HasError = 1;
    }
    if (!HasError) {
        if (nlen > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, GetAtomString(atable, name));
        } else {
            lbase = GetBase(lType);
            rbase = GetBase(rtype);
            if (Cg->theHAL->IsNumericBase(lbase) && Cg->theHAL->IsNumericBase(rbase)) {
                nbase = ConvertNumericOperands(fop, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
                SUBOP_SET_T(subop, nbase);
                nlen = llen > rlen ? llen : rlen;
                result = NewBinopSubNode(lop, subop, lExpr, rexpr);
                result->type = GetStandardType(TYPE_BASE_BOOLEAN, nlen, 0);
            } else if (lbase == TYPE_BASE_BOOLEAN && rbase == TYPE_BASE_BOOLEAN) {
                subop = SUBOP_V(nlen, TYPE_BASE_BOOLEAN);
                result = NewBinopSubNode(lop, subop, lExpr, rexpr);
                result->type = GetStandardType(TYPE_BASE_BOOLEAN, nlen, 0);
            } else {
                SemanticError(loc, ERROR_S_OPERANDS_NOT_NUMERIC, GetAtomString(atable, name));
            }
        }
    }
    if (!result) {
        result = NewBinopSubNode(lop, 0, lExpr, rexpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewBinaryComparisonOperator

/*
 * NewConditionalOperator() - Check the types of the components of a conditional expression.
 *         Return a new node with the proper operator description.
 *
 * Valid forma are:
 *
 *     op       cond    exp1    exp2   result
 *   -------   ------  ------  ------  ------
 *   COND      scalar  scalar  scalar  scalar
 *   COND_SV   scalar  vector  vector  vector
 *   COND_V    vector  vector  vector  vector
 *   COND_GEN  scalar   type    type    type
 */

expr *NewConditionalOperator(SourceLoc *loc, expr *bexpr, expr *lExpr, expr *rexpr)
{
    int lop, subop, blen = 0, llen = 0, rlen = 0, nlen = 0;
    int HasError = 0, LIsNumeric, LIsBoolean, LIsSimple;
    int lbase, rbase, nbase, category;
    Type *btype, *lType, *rtype, *beltype, *leltype, *reltype;
    Type *resulttype = UndefinedType;
    trinary *result = NULL;

    // Type of conditional expression is checked elsewhere.

    lop = COND_OP;
    subop = 0;
    btype = beltype = bexpr->common.type;
    lType = leltype = lExpr->common.type;
    rtype = reltype = rexpr->common.type;
    lbase = GetBase(leltype);
    rbase = GetBase(reltype);
    LIsNumeric = Cg->theHAL->IsNumericBase(lbase) & Cg->theHAL->IsNumericBase(rbase);
    LIsBoolean = (lbase == TYPE_BASE_BOOLEAN) & (rbase == TYPE_BASE_BOOLEAN);
    LIsSimple = LIsNumeric | LIsBoolean;
    if (LIsSimple) {

        // 1) Numeric

        if (IsScalar(btype)) {

            // 1A) Scalar ? Scalar : Scalar

            if (IsScalar(lType)) {
                if (IsScalar(rtype)) {
                    // O.K.
                } else {
                    SemanticError(loc, ERROR___QSTN_SCALAR_3RD_OPND_EXPECTED);
                    HasError = 1;
                }

            // 1B) Scalar ? Vector : Vector

            } else if (IsVector(lType, &llen)) {
                leltype = lType->arr.eltype;
                if (IsVector(rtype, &rlen)) {
                    reltype = rtype->arr.eltype;
                    lbase = GetBase(leltype);
                    rbase = GetBase(reltype);
                    lop = COND_SV_OP;
                    subop = SUBOP_SV(llen, 0);
                } else {
                    SemanticError(loc, ERROR___QSTN_VECTOR_3RD_OPND_EXPECTED);
                    HasError = 1;
                }

            // 1C) Scalar ? Array : Array >>--->> Treat as non-numeric case

            } else {
                LIsSimple = 0; // Check type compatibility later
            }
        } else if (IsVector(btype, &blen)) {

            // 1D) Vector ? Vector : Vector

            if (IsVector(lType, &llen) && IsVector(rtype, &rlen)) {
                lop = COND_V_OP;
                subop = SUBOP_SV(llen, 0);
                leltype = lType->arr.eltype;
                reltype = rtype->arr.eltype;
                lbase = GetBase(leltype);
                rbase = GetBase(reltype);
            } else {
                SemanticError(loc, ERROR___QSTN_VECTOR_23_OPNDS_EXPECTED);
                HasError = 1;
            }
        } else {
            SemanticError(loc, ERROR___QSTN_INVALID_1ST_OPERAND);
            HasError = 1;
        }
    }
    if (!LIsSimple) {

        // 2) Not numeric - must be same type.  Requires scalar condition.

        if (IsScalar(btype)) {
            if (IsSameUnqualifiedType(lType, rtype)) {
                lop = COND_GEN_OP;
                resulttype = lType;
            } else {
                SemanticError(loc, ERROR___QSTN_23_OPNDS_INCOMPAT);
                HasError = 1;
            }
        } else {
            SemanticError(loc, ERROR___QSTN_1ST_OPERAND_NOT_SCALAR);
            HasError = 1;
        }
    }
    if (!HasError) {
        if (lExpr->common.HasSideEffects || rexpr->common.HasSideEffects) {
            SemanticError(loc, ERROR_S_OPERANDS_HAVE_SIDE_EFFECTS, "?:");
        }
        if (LIsSimple) {
            nbase = ConvertNumericOperands(COND_OP, &lExpr, &rexpr, lbase, rbase, llen, rlen, 0, 0);
            if (llen == rlen && (blen == 0 || blen == llen)) {
                SUBOP_SET_T(subop, nbase);
                result = NewTriopSubNode(lop, subop, bexpr, lExpr, rexpr);
                result->type = GetStandardType(nbase, llen, 0);
            } else {
                SemanticError(loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, "\"? :\"");
                HasError = 1;
            }
        } else {
            category = GetCategory(lType);
            if ((category == TYPE_CATEGORY_SCALAR ||
                 category == TYPE_CATEGORY_ARRAY ||
                 category == TYPE_CATEGORY_STRUCT) &&
                !IsVoid(lType))
            {
                result = NewTriopSubNode(lop, 0, bexpr, lExpr, rexpr);
                result->type = lType;
            } else {
                SemanticError(loc, ERROR___QSTN_23_OPNDS_INVALID);
                HasError = 1;
            }
        }
    }
    if (!result) {
        result = NewTriopSubNode(lop, 0, bexpr, lExpr, rexpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewConditionalOperator

/*
 * NewSwizzleOperator() - See if this is a valid swizzle operation.  Return a new node with the
 *         proper operator description.
 *
 */

expr *NewSwizzleOperator(SourceLoc *loc, expr *fExpr, int ident)
{
    int HasError = 0, len = 0, ii, maxi, base, tmask, mask = 0, mlen = 0, LIsLValue;
    Type *ftype, *feltype;
    unary *result = NULL;

    mask = GetSwizzleOrWriteMask(loc, ident, &LIsLValue, &mlen);
    ftype = fExpr->common.type;
    if (IsScalar(ftype)) {
        feltype = ftype;
        maxi = 0;
    } else if (IsVector(ftype, &len)) {
        feltype = ftype->arr.eltype;
        maxi = len - 1;
        if (len > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, ".");
            HasError = 1;
        }
    } else {
        SemanticError(loc, ERROR_S_OPERANDS_NOT_SCALAR_VECTOR, ".");
        HasError = 1;
    }
    if (!HasError) {
        base = GetBase(feltype);
        tmask = mask;
        for (ii = 0; ii < mlen; ii++) {
            if ((tmask & 0x3) > maxi) {
                SemanticError(loc, ERROR_S_SWIZZLE_MASK_EL_MISSING,
                              GetAtomString(atable, ident));
                HasError = 1;
                break;
            }
            tmask >>= 2;
        }
        if (!HasError) {
            if (mlen == 1)
                mlen = 0; // I.e. scalar, not array[1]
            result = NewUnopSubNode(SWIZZLE_Z_OP, SUBOP_Z(mask, mlen, len, base), fExpr);
            result->type = GetStandardType(base, mlen, 0);
            result->IsLValue = LIsLValue & fExpr->common.IsLValue;
            result->IsConst = result->IsLValue & fExpr->common.IsConst;
        }
    }
    if (!result) {
        result = NewUnopSubNode(SWIZZLE_Z_OP, 0, fExpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewSwizzleOperator

/*
 * NewMatrixSwizzleOperator() - See if this is a valid matrix swizzle operation.  Return a new
 *         node with the proper operator description.
 */

expr *NewMatrixSwizzleOperator(SourceLoc *loc, expr *fExpr, int ident)
{
    int HasError = 0, len = 0, len2 = 0, ii, maxi, base, tmask, mask = 0, mlen = 0, LIsLValue;
    Type *ftype, *feltype;
    unary *result = NULL;

    mask = GetMatrixSwizzleOrWriteMask(loc, ident, &LIsLValue, &mlen);
    ftype = fExpr->common.type;
    if (IsMatrix(ftype, &len, &len2)) {
        feltype = ftype->arr.eltype;
        maxi = len - 1;
        if (len > 4 || len2 > 4) {
            SemanticError(loc, ERROR_S_MATRIX_OPERAND_GR_4, ".");
            HasError = 1;
        }
    } else {
        SemanticError(loc, ERROR_S_OPERANDS_NOT_MATRIX, ".");
        HasError = 1;
    }
    if (!HasError) {
        base = GetBase(feltype);
        tmask = mask;
        for (ii = 0; ii < mlen; ii++) {
            if ((tmask & 0x3) >= len || ((tmask >> 2) & 0x3) >= len2) {
                SemanticError(loc, ERROR_S_SWIZZLE_MASK_EL_MISSING,
                              GetAtomString(atable, ident));
                HasError = 1;
                break;
            }
            tmask >>= 4;
        }
        if (!HasError) {
            if (mlen == 1)
                mlen = 0; // I.e. scalar, not array[1]
            result = NewUnopSubNode(SWIZMAT_Z_OP, SUBOP_ZM(mask, mlen, len2, len, base), fExpr);
            result->type = GetStandardType(base, mlen, 0);
            result->IsLValue = LIsLValue & fExpr->common.IsLValue;
            result->IsConst = result->IsLValue & fExpr->common.IsConst;
        }
    }
    if (!result) {
        result = NewUnopSubNode(SWIZMAT_Z_OP, 0, fExpr);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewMatrixSwizzleOperator

/*
 * NewVectorConstructor() - Construct a vector of length 1 to 4 from the expressions in fExpr.
 *
 */

expr *NewVectorConstructor(SourceLoc *loc, Type *fType, expr *fExpr)
{
    int len = 0, HasError = 0, size = 0, lbase, nbase, lNumeric, nNumeric, vlen, vlen2;
    unary *result = NULL;
    expr *lExpr;
    Type *lType, *rType;

    if (fType) {
        rType = fType;
        if (IsScalar(fType)) {
            size = 1;
        } else if (IsVector(fType, &vlen)) {
            size = vlen;
        } else if (IsMatrix(fType, &vlen, &vlen2)) {
            size = vlen*vlen2;
        } else {
            SemanticError(loc, ERROR___INVALID_TYPE_FUNCTION);
            rType = UndefinedType;
        }
    } else {
        rType = UndefinedType;
    }
    lExpr = fExpr;
    while (lExpr) {
        vlen = 0;
        lType = lExpr->common.type;
        lbase = GetBase(lType);
        lNumeric = Cg->theHAL->IsNumericBase(lbase);
        if (!lNumeric && lbase != TYPE_BASE_BOOLEAN) {
            SemanticError(loc, ERROR___VECTOR_CONSTR_NOT_NUM_BOOL);
            HasError = 1;
            break;
        }
        if (IsScalar(lType)) {
            vlen = 1;
#if 000 // Unifdefout this to allow things like: "{ vec3, float }"
        } else if (IsVector(lType, &vlen)) {
            /* Nothing to do. */
#endif
        } else {
            SemanticError(loc, ERROR___VECTOR_CONSTR_NOT_SCALAR);
            HasError = 1;
            break;
        }
        if (len == 0) {
            nbase = lbase;
            nNumeric = lNumeric;
        } else if (len + vlen <= 4) {
            if (lNumeric == nNumeric) {
                if (nNumeric) {
                    nbase = Cg->theHAL->GetBinOpBase(VECTOR_V_OP, nbase, lbase, 0, 0);
                }
            } else {
                SemanticError(loc, ERROR___MIXED_NUM_NONNUM_VECT_CNSTR);
                HasError = 1;
                break;
            }
        } else {
            SemanticError(loc, ERROR___CONSTRUCTER_VECTOR_LEN_GR_4);
            HasError = 1;
            break;
        }
        len += vlen;
        lExpr = lExpr->bin.right;
    }
    if (size && !HasError) {
        if (size > len) {
            SemanticError(loc, ERROR___TOO_LITTLE_DATA_TYPE_FUN);
            HasError = 1;
        } else if (size < len) {
            SemanticError(loc, ERROR___TOO_MUCH_DATA_TYPE_FUN);
            HasError = 1;
        }
    }
    if (!HasError) {
        lExpr = fExpr;
        while (lExpr) {
            lType = lExpr->common.type;
            lbase = GetBase(lType);
            if (lbase != nbase)
                lExpr->bin.left = CastScalarVectorMatrix(lExpr->bin.left, lbase, nbase, 0, 0);
            lExpr = lExpr->bin.right;
        }
        result = NewUnopSubNode(VECTOR_V_OP, SUBOP_V(len, nbase), fExpr);
        result->type = GetStandardType(nbase, len, 0);
    }
    if (!result) {
        result = NewUnopSubNode(VECTOR_V_OP, 0, fExpr);
        result->type = rType;
    }
    return (expr *) result;
} // NewVectorConstructor

/*
 * NewCastOperator() - Type cast "fExpr" to "ftype" if possible.
 *
 */

expr *NewCastOperator(SourceLoc *loc, expr *fExpr, Type *toType)
{
    expr *lExpr;

    if (ConvertType(fExpr, toType, fExpr->common.type, &lExpr, 0, 1)) {
        lExpr->common.type = toType;
        return lExpr;
    } else {
        SemanticError(loc, ERROR___INVALID_CAST);
        return fExpr;
    }
} // NewCastOperator

/*
 * NewMemberSelectorOrSwizzleOrWriteMaskOperator() - Construct either a struct member
 *         operator,or a swizzle operator, or a writemask operator, depending upon the
 *         type of the expression "fExpr".   I think I'm gonna barf.
 */

expr *NewMemberSelectorOrSwizzleOrWriteMaskOperator(SourceLoc *loc, expr *fExpr, int ident)
{
    Type *lType = fExpr->common.type;
    int len, len2;
    expr *lExpr, *mExpr;
    Symbol *lSymb;

    if (IsCategory(lType, TYPE_CATEGORY_STRUCT)) {
        lSymb = LookUpLocalSymbol(lType->str.members, ident);
        if (lSymb) {
            mExpr = (expr *) NewSymbNode(MEMBER_OP, lSymb);
            lExpr = (expr *) NewBinopNode(MEMBER_SELECTOR_OP, fExpr, mExpr);
            lExpr->common.IsLValue = fExpr->common.IsLValue;
            lExpr->common.IsConst = fExpr->common.IsConst;
            lExpr->common.type = lSymb->type;
        } else {
            SemanticError(loc, ERROR_SS_NOT_A_MEMBER,
                          GetAtomString(atable, ident), GetAtomString(atable, lType->str.tag));
            lExpr = fExpr;
        }
    } else if (IsScalar(lType) || IsVector(lType, &len)) {
        lExpr = NewSwizzleOperator(loc, fExpr, ident);
    } else if (IsMatrix(lType, &len, &len2)) {
        lExpr = NewMatrixSwizzleOperator(loc, fExpr, ident);
    } else {
        SemanticError(loc, ERROR_S_LEFT_EXPR_NOT_STRUCT_ARRAY, GetAtomString(atable, ident));
        lExpr = fExpr;
    }
    return lExpr;
} // NewMemberSelectorOrSwizzleOrWriteMaskOperator

/*
 * NewIndexOperator() - Construct an array index operator.
 *
 */

expr *NewIndexOperator(SourceLoc *loc, expr *fExpr, expr *ixexpr)
{
    expr *lExpr;

    if (IsCategory(fExpr->common.type, TYPE_CATEGORY_ARRAY)) {
        lExpr = (expr *) NewBinopNode(ARRAY_INDEX_OP, fExpr, ixexpr);
        lExpr->common.IsLValue = fExpr->common.IsLValue;
        lExpr->common.IsConst = fExpr->common.IsConst;
        lExpr->common.type = GetElementType(fExpr->common.type);
    } else {
        SemanticError(loc, ERROR___INDEX_OF_NON_ARRAY);
        lExpr = fExpr;
    }
    return lExpr;
} // NewIndexOperator

/*
 * lResolveOverloadedFunction() - Resolve an overloaded function call.
 *
 */

Symbol *lResolveOverloadedFunction(SourceLoc *loc, Symbol *fSymb, expr *actuals)
{
    const int NO_MATCH = 0;
    const int EXACT_MATCH = 1;
    const int VALID_MATCH = 2;
    int paramno, numexact, numvalid, ii;
    Symbol *lSymb, *lExact, *lValid;
    TypeList *lFormals;

    lSymb = fSymb;
    while (lSymb) {
        lSymb->details.fun.flags = EXACT_MATCH;
        lSymb = lSymb->details.fun.overload;
    }
    paramno = 0;
    while (actuals) {
        numexact = numvalid = 0;
        lExact = lValid = fSymb;
        lSymb = fSymb;
        while (lSymb) {
            if (lSymb->details.fun.flags) {
                lFormals = lSymb->type->fun.paramtypes;
                for (ii = 0; ii < paramno; ii++) {
                    if (lFormals) {
                        lFormals = lFormals->next;
                    } else {
                        // Ran out of formals -- kick it out.
                        lSymb->details.fun.flags = NO_MATCH;
                    }
                }
                if (lFormals) {
                    if (IsSameUnqualifiedType(lFormals->type, actuals->common.type)) {
                        lSymb->details.fun.flags = EXACT_MATCH;
                        lExact = lSymb;
                        numexact++;
                    } else {
                        if (ConvertType(NULL, lFormals->type, actuals->common.type, NULL, 0, 0)) {
                            lSymb->details.fun.flags = VALID_MATCH;
                            lValid = lSymb;
                            numvalid++;
                        } else {
                            lSymb->details.fun.flags = NO_MATCH;
                        }
                    }
                } else {
                    lSymb->details.fun.flags = NO_MATCH;
                }
            }
            lSymb = lSymb->details.fun.overload;
        }
        if (numexact == 1)
            return lExact;
        if (numvalid == 1)
            return lValid;
        if (numexact > 0) {
            if (numvalid > 0) {
                // Disqualify non-exact matches:
                lSymb = fSymb;
                while (lSymb) {
                    if (lSymb->details.fun.flags == VALID_MATCH)
                        lSymb->details.fun.flags = NO_MATCH;
                    lSymb = lSymb->details.fun.overload;
                }
            }
        } else {
            if (numvalid == 0) {
                // Nothing matches.
                break;
            }
        }
        actuals = actuals->bin.right;
        paramno++;
    }
    // If multiple matches still present check number of args:
    if (numexact > 0 || numvalid > 0) {
        numvalid = 0;
        lSymb = lValid = fSymb;
        while (lSymb) {
            if (lSymb->details.fun.flags) {
                lFormals = lSymb->type->fun.paramtypes;
                for (ii = 0; ii < paramno; ii++) {
                    if (lFormals) {
                        lFormals = lFormals->next;
                    } else {
                        // Ran out of formals -- shouldn't happen.
                        assert(0);
                    }
                }
                if (lFormals) {
                    lSymb->details.fun.flags = NO_MATCH;
                } else {
                    numvalid++;
                    lValid = lSymb;
                }
            }
            lSymb = lSymb->details.fun.overload;
        }
        if (numvalid == 1)
            return lValid;
    }
    if (numvalid > 0) {
        SemanticError(loc, ERROR_S_AMBIGUOUS_FUN_REFERENCE, GetAtomString(atable, fSymb->name));
    } else {
        SemanticError(loc, ERROR_S_NO_COMPAT_OVERLOADED_FUN, GetAtomString(atable, fSymb->name));
    }
#if 1 // Detailed error messages - requires printing of types
    lSymb = fSymb;
    numvalid = 0;
    while (lSymb) {
        if (lSymb->details.fun.flags) {
            printf("    #%d: ", ++numvalid);
            PrintType(lSymb->type->fun.rettype, 0);
            printf(" %s", GetAtomString(atable, lSymb->name));
            PrintType(lSymb->type, 0);
            printf("\n");
        }
        lSymb = lSymb->details.fun.overload;
    }
#endif
    return fSymb;
} // lResolveOverloadedFunction

/*
 * NewFunctionCallOperator() - Construct a function call node.  Check types of parameters,
 *         resolve overloaded function, etc.
 *
 */

expr *NewFunctionCallOperator(SourceLoc *loc, expr *funExpr, expr *actuals)
{
    binary *result = NULL;
    Type *funType, *formalType, *actualType;
    TypeList *lFormals;
    expr *lExpr, *lActuals;
    Symbol *lSymb;
    int paramno, inout;
    int lop, lsubop = FUN_CALL_OP;

    funType = funExpr->common.type;
    if (IsCategory(funType, TYPE_CATEGORY_FUNCTION)) {
        lop = FUN_CALL_OP;
        lsubop = 0;
        if (funExpr->common.kind == SYMB_N) {
            lSymb = funExpr->sym.symbol;
            if (lSymb->kind == FUNCTION_S) {
                if (lSymb->details.fun.overload) {
                    lSymb = lResolveOverloadedFunction(loc, lSymb, actuals);
                    funExpr->sym.symbol = lSymb;
                    funType = funExpr->common.type = lSymb->type;
                }
                if (funType->properties & TYPE_MISC_INTERNAL) {
                    lop = FUN_BUILTIN_OP;
                    lsubop = (lSymb->details.fun.group << 16) | lSymb->details.fun.index;
                }
            } else {
                InternalError(loc, ERROR_S_SYMBOL_NOT_FUNCTION, GetAtomString(atable, lSymb->name));
            }
        }
        lFormals = funType->fun.paramtypes;
        lActuals = actuals;
        paramno = 0;
        while (lFormals && lActuals) {
            paramno++;
            formalType = lFormals->type;
            actualType = lActuals->common.type;
            inout = 0;
            if ((formalType->properties & TYPE_QUALIFIER_IN) ||
                !(formalType->properties & TYPE_QUALIFIER_INOUT))
                inout |= 1;
            if (formalType->properties & TYPE_QUALIFIER_OUT) {
                inout |= 2;
                lExpr = lActuals->bin.left;
                if (lExpr) {
                    if (lExpr->common.IsLValue) {
                        if (!(GetQualifiers(actualType) & TYPE_QUALIFIER_CONST)) {
                            if (IsSameUnqualifiedType(formalType, actualType) &&
                                IsPacked(formalType) == IsPacked(actualType))
                            {
                                SUBOP_SET_MASK(lActuals->bin.subop, inout);
                            } else {
                                SemanticError(loc, ERROR_D_OUT_PARAM_NOT_SAME_TYPE, paramno);
                            }
                        } else {
                            SemanticError(loc, ERROR_D_OUT_PARAM_IS_CONST, paramno);
                        }
                    } else {
                        SemanticError(loc, ERROR_D_OUT_PARAM_NOT_LVALUE, paramno);
                    }
                }
            } else if (ConvertType(lActuals->bin.left, formalType, actualType, &lExpr, 0, 0)) {
                lActuals->bin.left = lExpr;
                SUBOP_SET_MASK(lActuals->bin.subop, inout);
            } else {
                SemanticError(loc, ERROR_D_INCOMPATIBLE_PARAMETER, paramno);
            }
            lFormals = lFormals->next;
            lActuals = lActuals->bin.right;
        }
        if (lFormals) {
            if (!IsVoid(lFormals->type))
                SemanticError(loc, ERROR___TOO_FEW_PARAMS);
        } else if (lActuals) {
            SemanticError(loc, ERROR___TOO_MANY_PARAMS);
        }
        result = NewBinopSubNode(lop, lsubop, funExpr, actuals);
        result->IsLValue = 0;
        result->IsConst = 0;
        result->HasSideEffects |= lSymb->details.fun.HasOutParams;
        result->type = funExpr->common.type->fun.rettype;
    } else {
        SemanticError(loc, ERROR___CALL_OF_NON_FUNCTION);
    }
    if (!result) {
        result = NewBinopNode(FUN_CALL_OP, funExpr, actuals);
        result->type = UndefinedType;
    }
    return (expr *) result;
} // NewFunctionCallOperator

/*
 * NewSimpleAssignment() - Build a new simple assignment expression.
 *
 */

expr *NewSimpleAssignment(SourceLoc *loc, expr *fVar, expr *fExpr, int InInit)
{
    int lop, subop, base, len, vqualifiers, vdomain, edomain;
    Type *vType, *eType;
    expr *lExpr;

    vType = fVar->common.type;
    eType = fExpr->common.type;
    vqualifiers = GetQualifiers(vType);
    vdomain = GetDomain(vType);
    edomain = GetDomain(eType);
    if (!fVar->common.IsLValue)
        SemanticError(loc, ERROR___ASSIGN_TO_NON_LVALUE);
    //if ((vqualifiers & TYPE_QUALIFIER_CONST) && !InInit)
    if (fVar->common.IsConst && !InInit)
        SemanticError(loc, ERROR___ASSIGN_TO_CONST_VALUE);
    if (vdomain == TYPE_DOMAIN_UNIFORM && edomain == TYPE_DOMAIN_VARYING)
        SemanticError(loc, ERROR___ASSIGN_VARYING_TO_UNIFORM);
    if (ConvertType(fExpr, vType, eType, &lExpr, InInit, 0)) {
        fExpr = lExpr;
    } else {
        if (vType != UndefinedType && eType != UndefinedType)
            SemanticError(loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
    }
    base = GetBase(vType);
    if (IsScalar(vType)) {
        lop = ASSIGN_OP;
        subop = SUBOP__(base);
    } else if (IsVector(vType, &len)) {
        lop = ASSIGN_V_OP;
        subop = SUBOP_V(len, base);
    } else {
        lop = ASSIGN_GEN_OP;
        subop = SUBOP__(base);
    }
    lExpr = (expr *) NewBinopSubNode(lop, subop, fVar, fExpr);
    lExpr->common.type = vType;
    return lExpr;
} // NewSimpleAssignment

/*
 * NewSimpleAssignmentStmt() - Build a new simple assignment statement.
 *
 */

stmt *NewSimpleAssignmentStmt(SourceLoc *loc, expr *fVar, expr *fExpr, int InInit)
{
    return (stmt *) NewExprStmt(loc, NewSimpleAssignment(loc, fVar, fExpr, InInit));
} // NewSimpleAssignmentStmt

/*
 * NewCompoundAssignment() - Build a new simple assignment statement.
 *
 */

stmt *NewCompoundAssignmentStmt(SourceLoc *loc, opcode op, expr *fVar, expr *fExpr)
{
    return (stmt *) NewExprStmt(loc, (expr *) NewBinopNode(op, fVar, fExpr));
} // NewCompoundAssignment

#if 000
/*
 * NewMaskedAssignment() - Add a new masked assignment to an assignment statement.
 *
 * "fStmt" is an assignment stmt of the form: "exp" or "var = exp" ...
 * Returns a stmt of the form: "var@@mask = exp" or "var@@mask = (var = exp)" ...
 *
 */

stmt *NewMaskedAssignment(SourceLoc *loc, int mask, expr *fExpr, stmt *fStmt)
{
    int lop, subop, base, len;
    expr *lExpr;
    Type *lType;
    char str[5];
    int ii, kk;

    if (!fExpr->common.IsLValue)
        SemanticError(loc, ERROR___ASSIGN_TO_NON_LVALUE);
    if (ConvertType(fStmt->exprst.exp, fExpr->common.type, fStmt->exprst.exp->common.type,
                    &lExpr, 0)) {
        fStmt->exprst.exp = lExpr;
    } else {
        SemanticError(loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
    }
    lType = fExpr->common.type;
    base = GetBase(lType);
    if (IsScalar(lType)) {
        SemanticError(loc, ERROR___MASKED_ASSIGN_TO_VAR);
        lop = ASSIGN_OP;
        subop = SUBOP__(base);
    } else if (IsVector(lType, &len)) {
        if (len > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, "@@");
            len = 4;
        }
        if ((~0 << len) & mask) {
            kk = 0;
            for (ii = len; ii < 4; ii++) {
                if (mask & (1 << ii))
                    str[kk++] = "xyzw"[ii];
            }
            str[kk] = '\0';
            SemanticError(loc, ERROR_S_MASKED_ASSIGN_NON_EXISTENT, str);
            mask &= (1 << len) - 1;
        }
        lop = ASSIGN_MASKED_KV_OP;
        subop = SUBOP_KV(mask, len, base);
    } else {
        SemanticError(loc, ERROR___MASKED_ASSIGN_TO_VAR);
        lop = ASSIGN_GEN_OP;
        subop = SUBOP__(base);
    }
    fStmt->exprst.exp = (expr *) NewBinopSubNode(lop, subop, fExpr, fStmt->exprst.exp);
    fStmt->exprst.exp->common.type = lType;
    return fStmt;
} // NewMaskedAssignment

/*
 * NewConditionalAssignment() - Add a new simple assignment to an assignment statement.
 *
 * "fStmt" is an assignment stmt of the form: "exp" or "var = exp" ...
 * Returns a stmt of the form: "var@@(cond) = exp" or "var@@(cond) = (var = exp)" ...
 *
 */

stmt *NewConditionalAssignment(SourceLoc *loc, expr *fcond, expr *fExpr, stmt *fStmt)
{
    int lop, subop, base, len, clen;
    expr *lExpr;
    Type *lType, *ctype;

    if (!fExpr->common.IsLValue)
        SemanticError(loc, ERROR___ASSIGN_TO_NON_LVALUE);
    if (ConvertType(fStmt->exprst.exp, fExpr->common.type, fStmt->exprst.exp->common.type,
                    &lExpr, 0)) {
        fStmt->exprst.exp = lExpr;
    } else {
        SemanticError(loc, ERROR___ASSIGN_INCOMPATIBLE_TYPES);
    }
    lType = fExpr->common.type;
    base = GetBase(lType);
    ctype = fcond->common.type;
    if (IsScalar(lType)) {
        if (!IsScalar(ctype))
            SemanticError(loc, ERROR___SCALAR_BOOL_EXPR_EXPECTED);
        lop = ASSIGN_COND_OP;
        subop = SUBOP__(base);
    } else if (IsVector(lType, &len)) {
        if (len > 4) {
            SemanticError(loc, ERROR_S_VECTOR_OPERAND_GR_4, "@@()");
            len = 4;
        }
        if (IsScalar(ctype)) {
            lop = ASSIGN_COND_SV_OP;
        } else {
            lop = ASSIGN_COND_V_OP;
            if (IsVector(ctype, &clen)) {
                if (clen != len)
                    SemanticError(loc, ERROR_S_VECTOR_OPERANDS_DIFF_LEN, "@@()");
            } else {
                SemanticError(loc, ERROR_S_INVALID_CONDITION_OPERAND);
            }
        }
        subop = SUBOP_V(len, base);
    } else {
        lop = ASSIGN_COND_GEN_OP;
        subop = SUBOP__(base);
    }
    fStmt->exprst.exp = (expr *) NewTriopSubNode(lop, subop, fExpr, fcond, fStmt->exprst.exp);
    fStmt->exprst.exp->common.type = lType;
    return fStmt;
} // NewConditionalAssignment
#endif

/*************************************** misc: ******************************************/

/* 
 * InitTempptr() -- initialize the tempptr fields of an expr node -- designed
 *   be passed as an argument to ApplyToXXXX.  Sets tempptr[0] to its arg1
 *   argument and clears the rest of tempptr to NULL
 */

void InitTempptr(expr *fExpr, void *arg1, int arg2)
{
    fExpr->common.tempptr[0] = arg1;
    memset(&fExpr->common.tempptr[1], 0,
           sizeof(fExpr->common.tempptr) - sizeof(fExpr->common.tempptr[0]));
} // InitTempptr

/********************************** Error checking: ******************************************/

ErrorLoc *ErrorLocsFirst = NULL;
ErrorLoc *ErrorLocsLast = NULL;
int ErrorPending = 0;

/*
 * RecordErrorPos() - Record the fact that an error token was seen at source location LOC.
 *
 * Error tokens should be encountered in source line order, so we just add them to the end of
 * the list of error locations.
 */

void RecordErrorPos(SourceLoc *loc)
{
    ErrorLoc *eloc = (ErrorLoc *) malloc(sizeof(ErrorLoc));

    eloc->loc = *loc;
    eloc->hit = 0;
    eloc->next = NULL;
    if (ErrorLocsFirst == NULL) {
        ErrorLocsFirst = eloc;
    } else {
        ErrorLocsLast->next = eloc;
    }
    ErrorLocsLast = eloc;

    // If an error has been generated during parsing before the error
    // token was seen, then we mark this error token as being hit and
    // clear the error pending flag.

    if (ErrorPending) {
        eloc->hit = 1;
        ErrorPending = 0;
    }
} // RecordErrorPos

/*
 * MarkErrorPosHit() - Upon seeing an error at LOC, mark the
 * corresponding error location as hit.
 */

void MarkErrorPosHit(SourceLoc *loc)
{
    ErrorLoc *eloc;
    ErrorLoc *match = NULL;

    for (eloc = ErrorLocsFirst; eloc != NULL; eloc = eloc->next) {
        if (loc->line <= eloc->loc.line) {
            match = eloc;
        } else {
            break;
        }
    }
    
    // If we found an error token location that comes after LOC, then
    // mark it as hit, otherwise, we haven't seen the error token yet,
    // so make a note that there's an error pending.

    if (match != NULL) {
        match->hit = 1;
    } else {
        ErrorPending = 1;
    }
} // MakeErrorPosHit

/*
 * CheckAllErrorsGenerated() - Verify that at least one error was
 * generated in the appropriate location for each error token that
 * appeared in the program.
 */

void CheckAllErrorsGenerated(void)
{
    ErrorLoc *eloc;

    // Turn off ErrorMode so we can generate real errors for the
    // absence of errors.
    Cg->options.ErrorMode = 0;

    for (eloc = ErrorLocsFirst; eloc != NULL; eloc = eloc->next) {
        if (eloc->hit == 0) {
            SemanticError (&(eloc->loc), ERROR___NO_ERROR);
        }
    }
} // CheckAllErrorsGenerated

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of support.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

