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
// inline.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

struct InlineFunData_Rec {
    Scope *superGlobalScope;
    Scope *globalScope;
    Scope *masterScope;
    Scope *calleeScope;
    int *nextFunInlineIndex;
    int *nextTempIndex;
    int funInlineIndex;
    int funIndex;
    StmtList statements;
    void *scratch;
};

static void AddComment(InlineFunData *fFunData, SourceLoc *loc, const char *str)
{
    stmt *lStmt;

    lStmt = (stmt *) NewCommentStmt(Cg->pLastSourceLoc, str);
    AppendStatements(&fFunData->statements, lStmt);
} // AddComment

static stmt *DuplicateStatementTree(stmt *fStmt)
{
    stmt *lStmt, *mStmt, *nStmt, *pStmt, *headStmt, *lastStmt;
    expr *lExpr;

    headStmt = NULL;
    while (fStmt) {
        switch (fStmt->commonst.kind) {
        case EXPR_STMT:
            lExpr = DupExpr(fStmt->exprst.exp);
            lStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, lExpr);
            break;
        case IF_STMT:
            lExpr = DupExpr(fStmt->ifst.cond);
            mStmt = DuplicateStatementTree(fStmt->ifst.thenstmt);
            nStmt = DuplicateStatementTree(fStmt->ifst.elsestmt);
            lStmt = (stmt *) NewIfStmt(&fStmt->commonst.loc, lExpr, mStmt, nStmt);
            break;
        case WHILE_STMT:
        case DO_STMT:
            lExpr = DupExpr(fStmt->whilest.cond);
            mStmt = DuplicateStatementTree(fStmt->whilest.body);
            lStmt = (stmt *) NewWhileStmt(&fStmt->commonst.loc, fStmt->whilest.kind, lExpr, mStmt);
            break;
        case FOR_STMT:
            mStmt = DuplicateStatementTree(fStmt->forst.init);
            lExpr = DupExpr(fStmt->forst.cond);
            nStmt = DuplicateStatementTree(fStmt->forst.step);
            pStmt = DuplicateStatementTree(fStmt->forst.body);
            lStmt = (stmt *) NewForStmt(&fStmt->commonst.loc, mStmt, lExpr, nStmt, pStmt);
            break;
        case BLOCK_STMT:
            mStmt = DuplicateStatementTree(fStmt->blockst.body);
            lStmt = (stmt *) NewBlockStmt(&fStmt->commonst.loc, mStmt);
            break;
        case RETURN_STMT:
            lExpr = DupExpr(fStmt->exprst.exp);
            lStmt = (stmt *) NewReturnStmt(&fStmt->commonst.loc, NULL, lExpr);
            break;
        case DISCARD_STMT:
            lExpr = DupExpr(fStmt->discardst.cond);
            lStmt = (stmt *) NewDiscardStmt(&fStmt->commonst.loc, lExpr);
            break;
        case COMMENT_STMT:
            lStmt = (stmt *) NewCommentStmt(&fStmt->commonst.loc,
                                            GetAtomString(atable, fStmt->commentst.str));
            break;
        default:
            lStmt = fStmt;
            assert(!"DuplicateStatementTree() - not yet finished");
            break;
        }
        if (headStmt) {
            lastStmt->commonst.next = lStmt;
        } else {
            headStmt = lStmt;
        }
        lastStmt = lStmt;
        fStmt = fStmt->commonst.next;
    }
    return headStmt;
} // DuplicateStatementTree

/*
 * ConvertReturnStatement() - Convert return statements into assignments.
 *
 */

static stmt *ConvertReturnStatement(stmt *fStmt, void *arg1, int arg2)
{
    stmt *lStmt;
    Symbol *retSymb;
    expr *lExpr;

    if (fStmt) {
        switch (fStmt->commonst.kind) {
        case RETURN_STMT:
            if (arg1) {
                retSymb = (Symbol *) arg1;
                lExpr = (expr *) NewSymbNode(VARIABLE_OP, retSymb);
                lStmt = NewSimpleAssignmentStmt(Cg->pLastSourceLoc, lExpr, fStmt->exprst.exp, 1);
            } else {
                lStmt = NULL;
            }
            break;
        default:
            lStmt = fStmt;
            break;
        }
    } else {
        lStmt = NULL;
    }
    return lStmt;
} // ConvertReturnStatement

#define NOT_DUPLICATED          0
#define ALREADY_DUPLICATED      1
#define IN_MASTER_SCOPE         2
#define IN_GLOBAL_SCOPE         3
#define IN_SUPER_GLOBAL_SCOPE   4

/*
 * ConvertLocalReferences() - Convert return statements into assignments.  Assume that this
 *         can be done in place since we're working with a copy of the original symb structs.
 *
 */

static expr *ConvertLocalReferences(expr *fExpr, void *arg1, int arg2)
{
    InlineFunData *lFunData = (InlineFunData *) arg1;
    expr *lExpr = fExpr;
    Symbol *lSymb, *nSymb;
    Type *lType;
    int qualifiers;
    int name;

    lExpr = fExpr;
    if (fExpr) {
        switch (fExpr->common.kind) {
        case SYMB_N:
            lSymb = lExpr->sym.symbol;
            if (fExpr->sym.op == VARIABLE_OP && lSymb->kind == VARIABLE_S) {
                switch (lSymb->flags) {
                case NOT_DUPLICATED:
                    name = GetNumberedAtom(GetAtomString(atable, lSymb->name), lFunData->funInlineIndex, 4, '-');
                    if (LookUpLocalSymbol(lFunData->masterScope, name)) {
                        InternalError(Cg->pLastSourceLoc, 9999, "Name \"%s\"-%04d shouldn't be defined, but is!",
                                      GetAtomString(atable, lSymb->name), lFunData->funInlineIndex);
                        if (Cg->options.DumpParseTree) {
                            InternalError(Cg->pLastSourceLoc, 9999, "*** Scope %d definitions ***",
                                          lFunData->masterScope->level);
                            PrintSymbolTree(lFunData->masterScope->symbols);
                            InternalError(Cg->pLastSourceLoc, 9999, "*** End of Scope %d ***",
                                          lFunData->masterScope->level);
                        }
                    }
                    // Remove qualifiers if any present:
                    lType = lSymb->type;
                    qualifiers = GetQualifiers(lType);
                    if (qualifiers) {
                        lType = DupType(lType);
                        ClearTypeMisc(lType, TYPE_QUALIFIER_MASK);
                    }
                    nSymb = DefineVar(&lSymb->loc, lFunData->masterScope, name, lType);
                    nSymb->properties = lSymb->properties;
                    nSymb->flags = IN_MASTER_SCOPE;
                    lSymb->tempptr = (void *) nSymb;
                    lSymb->flags = ALREADY_DUPLICATED;
                    break;
                case IN_MASTER_SCOPE:
                case IN_GLOBAL_SCOPE:
                case IN_SUPER_GLOBAL_SCOPE:
                    nSymb = lSymb;
                    break;
                case ALREADY_DUPLICATED:
                    nSymb = (Symbol *) lSymb->tempptr;
                    break;
                default:
                    FatalError("Bad scope in ConvertLocalReferences()");
                    break;
                }
                lExpr->sym.symbol = nSymb;
            }
            break;
        case CONST_N:
        case UNARY_N:
        case BINARY_N:
        case TRINARY_N:
            break;
        case DECL_N:
        default:
            assert(!"bad kind to ConvertLocalReferences()");
            break;
        }
    }
    return lExpr;
} // ConvertLocalReferences

/*
 * ExpandInlineFunction() - Expand a function inline by duplicating it's statements.
 *
 */

static void ExpandInlineFunction(InlineFunData *fFunData, Symbol *funSymb, Symbol *retSymb, expr *fActuals)
{
    stmt *body;
    stmt *nStmt;
    expr *lExpr, *lActual;
    Symbol *lFormal;
    StmtList newStmts;

    // Bump function counter:

    fFunData->funInlineIndex = (*fFunData->nextFunInlineIndex)++;

    // Duplicate the body of the function:

    body = DuplicateStatementTree(funSymb->details.fun.statements);

    // Change return statements into assignments:

    body = PostApplyToStatements(ConvertReturnStatement, body, retSymb, 0);

    // Add numbered copies of expanded function's local variables to current scope,
    // and convert any references in copied body of function to new symbols:

    SetSymbolFlagsList(ScopeList/*GlobalScope*/, NOT_DUPLICATED);
    SetSymbolFlags(fFunData->masterScope->symbols, IN_MASTER_SCOPE);
    SetSymbolFlags(fFunData->globalScope->symbols, IN_GLOBAL_SCOPE);
    SetSymbolFlags(fFunData->superGlobalScope->symbols, IN_SUPER_GLOBAL_SCOPE);

    //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    // !!! BEGIN !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
    #define TYPE_BASE_TEXOBJ_FP30   (TYPE_BASE_FIRST_USER + 2)
    lFormal = funSymb->details.fun.params;
    lActual = fActuals;
    while (lFormal) {
        if (GetBase(lFormal->type) == TYPE_BASE_TEXOBJ_FP30) {
            assert(lActual->bin.left->common.kind == SYMB_N);
            lFormal->tempptr = (void *) lActual->bin.left->sym.symbol;
            lFormal->flags = ALREADY_DUPLICATED;
        }
        lFormal = lFormal->next;
        lActual = lActual->bin.right;
    }
    // !!! END !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
    //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    PostApplyToExpressions(ConvertLocalReferences, body, fFunData, 0);

    // Assign actual parameter expressions to parameters:

    newStmts.first = newStmts.last = NULL;
    lFormal = funSymb->details.fun.params;
    lActual = fActuals;
    while (lFormal) {
        assert(lActual->common.kind == BINARY_N);
        assert(lActual->bin.op == FUN_ARG_OP);
        if (((GetQualifiers(lFormal->type) & TYPE_QUALIFIER_IN) ||
             !(GetQualifiers(lFormal->type) & TYPE_QUALIFIER_OUT)) &&
            //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            //<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            // !!! BEGIN !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
             (GetBase(lFormal->type) != TYPE_BASE_TEXOBJ_FP30) &&
            // !!! Temporary patch for "texobj" parameters!!! Don't assign!!!
            //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
            //>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
             1)
        {
            lExpr = (expr *) NewSymbNode(VARIABLE_OP, lFormal);
            lExpr = ConvertLocalReferences(lExpr, fFunData, 0);
            nStmt = NewSimpleAssignmentStmt(Cg->pLastSourceLoc, lExpr, lActual->bin.left, 1);
            AppendStatements(&newStmts, nStmt);
        }
        lFormal = lFormal->next;
        lActual = lActual->bin.right;
    }
    AppendStatements(&newStmts, body);

    // Build assignment statements to copy "out" param's final values to actual parameters:

    lFormal = funSymb->details.fun.params;
    lActual = fActuals;
    while (lFormal) {
        if (GetQualifiers(lFormal->type) & TYPE_QUALIFIER_OUT) {
            lExpr = (expr *) NewSymbNode(VARIABLE_OP, lFormal);
            lExpr = ConvertLocalReferences(lExpr, fFunData, 0);
            nStmt = NewSimpleAssignmentStmt(Cg->pLastSourceLoc, lActual->bin.left, lExpr, 0);
            AppendStatements(&newStmts, nStmt);
        }
        lFormal = lFormal->next;
        lActual = lActual->bin.right;
    }

    // Recursively expands calls in newly inlined finction:

    body = ExpandInlineFunctionCalls(funSymb->details.fun.locals, newStmts.first, fFunData);

    // Do some more stuff here...

    // Append the new statements:

    AppendStatements(&fFunData->statements, body);

} // ExpandInlineFunction

/*
 * ExpandInlineFunctionCallsNode() - Expand inline function calls in an expression.
 *
 */

static expr *ExpandInlineFunctionCallsNode(expr *fExpr, void *arg1, int arg2)
{
#define TEMP_ROOT      "$temp"

    InlineFunData *lFunData = (InlineFunData *) arg1;
    Symbol *lSymb, *retSymb;
    expr *lExpr;
    Type *funType, *retType;
    int vname;

    switch (fExpr->common.kind) {
    case DECL_N:
    case SYMB_N:
    case CONST_N:
    case UNARY_N:
        break;
    case BINARY_N:
        if (fExpr->bin.op == FUN_CALL_OP) {
            lExpr = fExpr->bin.left;
            if (lExpr->common.kind == SYMB_N) {
                lSymb = lExpr->sym.symbol;
                assert(IsFunction(lSymb));
                if (IsInline(lSymb)) {
                    funType = lSymb->type;
                    retType = funType->fun.rettype;
                    if (!IsVoid(retType)) {
                        vname = GetNumberedAtom(TEMP_ROOT, (*lFunData->nextTempIndex)++, 4, '\0');
                        retSymb = DefineVar(&lSymb->loc, lFunData->masterScope, vname, retType);
                    } else {
                        retSymb = NULL;
                    }
                    if (Cg->options.Comments) {
                        AddComment(lFunData, Cg->pLastSourceLoc, "Begin inline function");
                        AddComment(lFunData, Cg->pLastSourceLoc, GetAtomString(atable, lSymb->name));
                    }
                    ExpandInlineFunction(lFunData, lSymb, retSymb, fExpr->bin.right);
                    if (Cg->options.Comments) {
                        AddComment(lFunData, Cg->pLastSourceLoc, "End inline function");
                        AddComment(lFunData, Cg->pLastSourceLoc, GetAtomString(atable, lSymb->name));
                    }
                    if (retSymb) {
                        fExpr = (expr *) NewSymbNode(VARIABLE_OP, retSymb);
                    } else {
                        fExpr = NULL; // function returning void
                    }
                } else {
                    // Not done yet:  Must continue to traverse call tree to find other
                    // functions that are called and inline their calls as needed.
                    // Or some such stuff...
                }
            }
        }
    case TRINARY_N:
        break;
    default:
        assert(!"bad kind to ExpandInlineFunctionCallsNode()");
        break;
    }
    return fExpr;
} // ExpandInlineFunctionCallsNode

/*
 * ExpandInlineFunctionCallsStmt() - Expand inline function calls in a statement.
 *
 */

static stmt *ExpandInlineFunctionCallsStmt(stmt *fStmt, void *arg1, int arg2)
{
    InlineFunData *lFunData = (InlineFunData *) arg1;

    lFunData->statements.first = NULL;
    lFunData->statements.last = NULL;
    PostApplyToExpressionsLocal(ExpandInlineFunctionCallsNode, fStmt, arg1, arg2);
    if (lFunData->statements.first) {
        lFunData->statements.last->commonst.next = fStmt;
        fStmt = lFunData->statements.first;
    }
    lFunData->statements.first = NULL;
    lFunData->statements.last = NULL;
    return fStmt;
} // ExpandInlineFunctionCallsStmt

/*
 * ExpandInlineFunctionCalls() - Recursively walk a program'm call graph from main
 *         expanding function calls that have the attribute "inline".
 *
 * Assumes no recursion in inlined functions.
 *
 */

stmt *ExpandInlineFunctionCalls(Scope *fscope, stmt *body, InlineFunData *fFunData)
{
    int funcount = 0, tempcount = 0;
    InlineFunData lFunData;
    stmt *lStmt;

    if (fFunData) {
        lFunData.superGlobalScope = fFunData->superGlobalScope;
        lFunData.globalScope = fFunData->globalScope;
        lFunData.masterScope = fFunData->masterScope;
        lFunData.calleeScope = fscope;
        lFunData.nextFunInlineIndex = fFunData->nextFunInlineIndex;
        lFunData.nextTempIndex = fFunData->nextTempIndex;
        lFunData.funInlineIndex = fFunData->funInlineIndex;
    } else {
        lFunData.superGlobalScope = fscope->parent->parent;
        lFunData.globalScope = fscope->parent;
        lFunData.masterScope = fscope;
        lFunData.calleeScope = NULL;
        lFunData.nextFunInlineIndex = &funcount;
        lFunData.nextTempIndex = &tempcount;
        lFunData.funInlineIndex = 999;
    }
    lFunData.funIndex = 0;
    lFunData.statements.first = NULL;
    lFunData.statements.last = NULL;
    lFunData.scratch = NULL;

    lStmt = PostApplyToStatements(ExpandInlineFunctionCallsStmt, body, &lFunData, 0);
    return lStmt;

} // ExpandInlineFunctionCalls

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of inline.c ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
