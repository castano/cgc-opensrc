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
// compile.c
//

// In a cygnus tools window in the "gen" directory, run the following command:
//
// bison parser.y --defines -output ../src/parser.c --verbose

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

/*
 * OpenOutputFile()
 *
 */

int OpenOutputFile(void)
{
    if (Cg->options.outputFileName) {
        Cg->options.outfd = fopen(Cg->options.outputFileName, "w");
        if (Cg->options.outfd) {
            Cg->options.OutputFileOpen = 1;
        } else {
            FatalError("Can't open output file \"%s\"", Cg->options.outputFileName);
            return 0;
        }
    } else {
        Cg->options.outfd = stdout;
        Cg->options.OutputFileOpen = 1;
    }
    Cg->theHAL->PrintCodeHeader(Cg->options.outfd);
    fprintf(Cg->options.outfd, "%s cgc version %d.%d.%04d%s, build date %s  %s\n", Cg->theHAL->comment,
            HSL_VERSION, HSL_SUB_VERSION, HSL_SUB_SUB_VERSION, NDA_STRING, Build_Date, Build_Time);
    return 1;
} // OpenOutputFile

/*
 * OpenListFile()
 *
 */

int OpenListFile(void)
{
    if (Cg->options.listFileName) {
        Cg->options.listfd = fopen(Cg->options.listFileName, "w");
        if (Cg->options.listfd) {
            Cg->options.ListFileOpen = 1;
        } else {
            FatalError("Can't open listing file \"%s\"", Cg->options.listFileName);
            return 0;
        }
    } else {
        Cg->options.listfd = stdout;
    }
    if (Cg->options.ListFileOpen) {
        fprintf(Cg->options.listfd, "%s cgc version %d.%d.%04d%s, build date %s  %s\n", Cg->theHAL->comment,
                HSL_VERSION, HSL_SUB_VERSION, HSL_SUB_SUB_VERSION, NDA_STRING, Build_Date, Build_Time);
    }
    return 1;
} // OpenListFile

/*
 * PrintOptions()
 *
 */

void PrintOptions(int argc, char **argv)
{
    int ii;

    if (argc > 1) {
        fprintf(Cg->options.outfd, "%s command line args:", Cg->theHAL->comment);
        for (ii = 1; ii < argc; ii++)
            fprintf(Cg->options.outfd, " %s", argv[ii]);
        fprintf(Cg->options.outfd, "\n");
    }
} // PrintOptions

/*
 * CloseOutputFiles()
 *
 */

int CloseOutputFiles(const char *mess)
{
    if (Cg->options.OutputFileOpen) {
        if (!Cg->options.ListFileOpen)
            fprintf(Cg->options.outfd, "%s %s\n", Cg->theHAL->comment, mess);
        Cg->options.OutputFileOpen = 0;
        if (fclose(Cg->options.outfd)) {
            FatalError("Error closing output file.");
            return 0;
        }
    }
    if (Cg->options.ListFileOpen) {
        fprintf(Cg->options.listfd, "%s %s\n", Cg->theHAL->comment, mess);
        Cg->options.ListFileOpen = 0;
        if (fclose(Cg->options.listfd)) {
            FatalError("Error closing listing file.");
            return 0;
        }
    }
    return 1;
} // CloseOutputFiles

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Misc Support Functions: ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * HasNumericSuffix() - See if there is a numeric suffix on a string.  If so, strip it off and
 *         retrun the value as an integer.
 */

int HasNumericSuffix(const char *fStr, char *root, int size, int *suffix)
{
    int val, HasSuffix, len, scale;
    char *s, ch;

    strncpy(root, fStr, size - 1);
    len = strlen(fStr);
    if (len >= size)
        len = size - 1;
    root[len] = 0;
    val = 0;
    HasSuffix = 0;
    scale = 1;
    s = &root[len];
    while (1) {
        ch = *--s;
        if (ch >= '0' && ch <= '9' && s >= root) {
            val = val + scale*(ch - '0');
            scale *= 10;
            HasSuffix = 1;
        } else {
            break;
        }
    }
    s[1] = '\0';
    *suffix = val;
    return HasSuffix;
} // HasNumericSuffix

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Analysis and Code Generation Control Functions ///////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * SetSymbolFlags() - Set the flags field for all symbols in this tree.
 *
 */

void SetSymbolFlags(Symbol *fSymb, int fVal)
{
    if (fSymb) {
        fSymb->flags = fVal;
        SetSymbolFlags(fSymb->left, fVal);
        SetSymbolFlags(fSymb->right, fVal);
    }
} // SetSymbolFlags

/*
 * SetSymbolFlagsList() - Walk a list of scopes and set the flags field for all symbols.
 *
 */

void SetSymbolFlagsList(Scope *fScope, int fVal)
{
    while (fScope) {
        SetSymbolFlags(fScope->symbols, fVal);
        fScope = fScope->next;
    }
} // SetSymbolFlagsList

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Various Utility Functions //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GetNumberedAtom() - Create an atom with the given root string and a numbered suffix.
 */

int GetNumberedAtom(const char *root, int number, int digits, char ch)
{
    char str[256], *s;
    int vname, len;

    strcpy(str, root);
    len = strlen(str);
    if (ch != '\0') {
        str[len] = ch;
        len++;
    }
    s = &str[len + digits];
    *s = '\0';
    while (digits-- > 0) {
        *--s = '0' + number % 10;
        number /= 10;
    }
    vname = AddAtom(atable, str);
    return vname;
} // GetNumberedAtom

/*
 * GetVarExprName() - Return a string representation of the variable described by fExpr.
 */

int GetVarExprName(char *str, int size, expr *fExpr)
{
    const char *name;
    int len = 0, len2;

    *str = '\0';
    switch (fExpr->common.kind) {
    case SYMB_N:
        switch (fExpr->sym.op) {
        case VARIABLE_OP:
        case MEMBER_OP:
            name = GetAtomString(atable, fExpr->sym.symbol->name);
            len2 = strlen(name);
            if (len2 >= size)
                len2 = size - 1;
            len += len2;
            while (len2--)
                *str++ = *name++;
            *str = '\0';
            break;
        }
        break;
    case BINARY_N:
        // Put struct.member code here:
        break;
    }
    return len;
} // GetVarExprName

/*
 * GenerateIndexName() - Create an atom with the given root string representing the variable
 *         by fExpr and a numbered suffix for the index.
 */
int GenerateIndexName(expr *fExpr, int index)
{
    char str[256];
    int vname;

    GetVarExprName(str, 256, fExpr);
    vname = GetNumberedAtom(str, index, 1, '$');
    return vname;
} // GenerateIndexeName

/*
 * ConcatStmts() - Concatenate two lists of statements.
 *
 */

stmt *ConcatStmts(stmt *first, stmt *last)
{
    stmt *lStmt;

    if (first) {
        if (last) {
            lStmt = first;
            while (lStmt->commonst.next)
                lStmt = lStmt->commonst.next;
            lStmt->commonst.next = last;
        }
        return first;
    } else {
        return last;
    }
} // ConcatStmts

/*
 * AppendStatements() - Append a list of statements to the end of another list.
 *
 */

void AppendStatements(StmtList *fStatements, stmt *fStmt)
{
    if (fStmt) {
        if (fStatements->first) {
            fStatements->last->commonst.next = fStmt;
        } else {
            fStatements->first = fStmt;
        }
        while (fStmt->commonst.next)
            fStmt = fStmt->commonst.next;
        fStatements->last = fStmt;
    }
} // AppendStatements

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Expression Functions ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * GenSymb() - Create a node that references a symbol.
 *
 */

expr *GenSymb(Symbol *fSymb)
{
    expr *lExpr;

    lExpr = (expr *) NewSymbNode(VARIABLE_OP, fSymb);
    return lExpr;
} // GenSymb

/*
 * GenMember() - Create a node that references a member of a struct or connector.
 *
 */

expr *GenMember(Symbol *fSymb)
{
    expr *lExpr;

    lExpr = (expr *) NewSymbNode(MEMBER_OP, fSymb);
    return lExpr;
} // GenMember

/*
 * GenMemberSelector() - Create a node that references a member of a struct or connector.
 *
 */

expr *GenMemberSelector(expr *sExpr, expr *mExpr)
{
    expr *lExpr;

    lExpr = (expr *) NewBinopNode(MEMBER_SELECTOR_OP, sExpr, mExpr);
    lExpr->common.type = mExpr->common.type;
    return lExpr;
} // GenMemberSelector

/*
 * GenMemberReference() - Build an expression to reference a member of a struct.
 *
 */

expr *GenMemberReference(expr *sExpr, Symbol *mSymb)
{
    expr *lExpr, *mExpr;

   // sExpr = (expr *) NewSymbNode(VARIABLE_OP, sSymb);
    mExpr = (expr *) NewSymbNode(MEMBER_OP, mSymb);
    lExpr = (expr *) NewBinopNode(MEMBER_SELECTOR_OP, sExpr, mExpr);
    lExpr->common.type = mExpr->common.type;
    lExpr->common.IsLValue = 1;
    lExpr->common.IsConst = (GetQualifiers(lExpr->common.type) & TYPE_QUALIFIER_CONST) != 0;
    return lExpr;
} // GenMemberReference

/*
 * GenVecIndex() - Create a vector index node.
 *
 */

expr *GenVecIndex(expr *vexpr, expr *xexpr, int base, int len, int len2)
{
    expr *lExpr;

    lExpr = (expr *) NewBinopNode(ARRAY_INDEX_OP, vexpr, xexpr);
    lExpr->bin.type = GetStandardType(base, 0, 0);
    return lExpr;
} // GenVecIndex

/*
 * GenMatIndex() - Create a matrix index node.
 *
 */

expr *GenMatIndex(expr *mExpr, expr *xexpr, int base, int len, int len2)
{
    expr *lExpr;

    lExpr = (expr *) NewBinopNode(ARRAY_INDEX_OP, mExpr, xexpr);
    lExpr->bin.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenMatIndex

/*
 * GenBoolConst() - Create a Boolean constant node.
 *
 */

expr *GenBoolConst(int fval)
{
    expr *lExpr;

    lExpr = (expr *) NewBConstNode(BCONST_OP, fval, TYPE_BASE_BOOLEAN);
    return lExpr;
} // GenBoolConst

/*
 * GenIntConst() - Create an integer constant node.
 *
 */

expr *GenIntConst(int fval)
{
    expr *lExpr;

    lExpr = (expr *) NewIConstNode(ICONST_OP, fval, TYPE_BASE_INT);
    return lExpr;
} // GenIntConst

/*
 * GenFConstV() - Create a vector floating point constant node.
 *
 */

expr *GenFConstV(float *fval, int len, int base)
{
    expr *lExpr;

    lExpr = (expr *) NewFConstNodeV(FCONST_V_OP, fval, len, base);
    return lExpr;
} // GenFConstV

/*
 * GenBoolNot() - Create a Boolean NOT.
 *
 */

expr *GenBoolNot(expr *fExpr)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP__(TYPE_BASE_BOOLEAN);
    lExpr = (expr *) NewUnopSubNode(BNOT_OP, lsubop, fExpr);
    lExpr->un.type = BooleanType;
    return lExpr;
} // GenBoolNot

/*
 * GenBoolAssign() - Create a Boolean assignment.
 *
 */

expr *GenBoolAssign(expr *fVar, expr *fExpr)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP__(TYPE_BASE_BOOLEAN);
    lExpr = (expr *) NewBinopSubNode(ASSIGN_OP, lsubop, fVar, fExpr);
    lExpr->bin.type = BooleanType;
    return lExpr;
} // GenBoolAssign

/*
 * GenSAssign() - Create a scalar assignment.
 *
 */

expr *GenSAssign(expr *fVar, expr *fExpr, int base)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_S(base);
    lExpr = (expr *) NewBinopSubNode(ASSIGN_OP, lsubop, fVar, fExpr);
    lExpr->bin.type = GetStandardType(base, 0, 0);
    return lExpr;
} // GenSAssign

/*
 * GenVAssign() - Create a vector assignment.
 *
 */

expr *GenVAssign(expr *fVar, expr *fExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, base);
    lExpr = (expr *) NewBinopSubNode(ASSIGN_V_OP, lsubop, fVar, fExpr);
    lExpr->bin.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenCondSVAssign

/*
 * GenMAssign() - Create a matrix assignment.
 *
 */

expr *GenMAssign(expr *fVar, expr *fExpr, int base, int len, int len2)
{
    expr *lExpr;

    lExpr = (expr *) NewBinopNode(ASSIGN_GEN_OP, fVar, fExpr);
    lExpr->bin.type = GetStandardType(base, len, len2);
    return lExpr;
} // GenMAssign

/*
 * GenCondSAssign() - Create a scalar conditional assignment.
 *
 */

expr *GenCondSAssign(expr *fVar, expr *fCond, expr *fExpr, int base)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_S(base);
    lExpr = (expr *) NewTriopSubNode(ASSIGN_COND_OP, lsubop, fVar, fCond, fExpr);
    lExpr->tri.type = GetStandardType(base, 0, 0);
    return lExpr;
} // GenCondSAssign

/*
 * GenCondSVAssign() - Create a vector conditional assignment with a scalar Boolean argument.
 *
 */

expr *GenCondSVAssign(expr *fVar, expr *fCond, expr *fExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_SV(len, base);
    lExpr = (expr *) NewTriopSubNode(ASSIGN_COND_SV_OP, lsubop, fVar, fCond, fExpr);
    lExpr->tri.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenCondSVAssign

/*
 * GenCondVAssign() - Create a vector conditional assignment with a vector Boolean argument.
 *
 */

expr *GenCondVAssign(expr *fVar, expr *fCond, expr *fExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, base);
    lExpr = (expr *) NewTriopSubNode(ASSIGN_COND_V_OP, lsubop, fVar, fCond, fExpr);
    lExpr->tri.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenCondVAssign

/*
 * GenCondGenAssign() - Create a geeral conditional assignment.
 *
 */

expr *GenCondGenAssign(expr *fVar, expr *fCond, expr *fExpr)
{
    expr *lExpr;
    int lsubop;

    lsubop = 0;
    lExpr = (expr *) NewTriopSubNode(ASSIGN_COND_GEN_OP, lsubop, fVar, fCond, fExpr);
    lExpr->tri.type = fVar->common.type;
    return lExpr;
} // GenCondGenAssign

/*
 * GenBoolAnd() - Create a Boolean &&.
 *
 */

expr *GenBoolAnd(expr *aExpr, expr *bExpr)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP__(TYPE_BASE_BOOLEAN);
    lExpr = (expr *) NewBinopSubNode(BAND_OP, lsubop, aExpr, bExpr);
    lExpr->bin.type = BooleanType;
    return lExpr;
} // GenBoolAnd

/*
 * GenBoolAndVec() - Create a Boolean Vector &&.
 *
 */

expr *GenBoolAndVec(expr *aExpr, expr *bExpr, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, TYPE_BASE_BOOLEAN);
    lExpr = (expr *) NewBinopSubNode(BAND_V_OP, lsubop, aExpr, bExpr);
    lExpr->bin.type = GetStandardType(TYPE_BASE_BOOLEAN, len, 0);
    return lExpr;
} // GenBoolAndVec

/*
 * GenBoolSmear() - Smear a Boolean Vector @xxxx.
 *
 */

expr *GenBoolSmear(expr *fExpr, int len)
{
    expr *lExpr;
    int lsubop, mask;

    mask = (0 << 6) | (0 << 4) | (0 << 2) | 0;
    lsubop = SUBOP_Z(mask, len, 0, TYPE_BASE_BOOLEAN);
    lExpr = (expr *) NewUnopSubNode(SWIZZLE_Z_OP, lsubop, fExpr);
    lExpr->bin.type = GetStandardType(TYPE_BASE_BOOLEAN, len, 0);
    return lExpr;
} // GenBoolSmear

/*
 * GenExprList() - Add a node to an expression list.
 *
 */

expr *GenExprList(expr *fExpr, expr *nExpr, Type *ftype)
{
    expr *lExpr, *tExpr;

    lExpr = (expr *) NewBinopNode(EXPR_LIST_OP, nExpr, NULL);
    lExpr->common.type = ftype;
    if (fExpr) {
        tExpr = fExpr;
        while (tExpr->bin.right)
            tExpr = tExpr->bin.right;
        tExpr->bin.right = lExpr;
    } else {
        fExpr = lExpr;
    }
    return fExpr;
} // GenExprList

/*
 * GenVecMult() - Create a Vector Multiply
 *
 */

expr *GenVecMult(expr *aExpr, expr *bExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, base);
    lExpr = (expr *) NewBinopSubNode(MUL_V_OP, lsubop, aExpr, bExpr);
    lExpr->bin.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenVecMult

/*
 * GenScaleVecMult() - Create a smeared Scalar-Vector Multiply
 *
 */

expr *GenScaleVecMult(expr *aExpr, expr *bExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, base);
    lExpr = (expr *) NewBinopSubNode(MUL_VS_OP, lsubop, aExpr, bExpr);
    lExpr->bin.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenScaleVecMult

/*
 * GenVecAdd() - Create a Vector Addition
 *
 */

expr *GenVecAdd(expr *aExpr, expr *bExpr, int base, int len)
{
    expr *lExpr;
    int lsubop;

    lsubop = SUBOP_V(len, base);
    lExpr = (expr *) NewBinopSubNode(ADD_V_OP, lsubop, aExpr, bExpr);
    lExpr->bin.type = GetStandardType(base, len, 0);
    return lExpr;
} // GenVecAdd

/*
 * GenConvertVectorSize() - Convert a vector to another length, or to a scalar if newlen is 1.
 *        Return original expression if no conversion needed.
 */

expr *GenConvertVectorLength(expr *fExpr, int base, int len, int newlen)
{
    int mask, ii;
    expr *lExpr;

    if (newlen != len) {
        for (ii = 0; ii < newlen; ii++) {
            if (ii < len) {
                mask |= ii << (ii*2);
            } else {
                mask |= len - 1; // Use last component if source shorter than dest
            }
        }
        if (newlen == 1)
            newlen = 0; // I.e. scalar, not array[1]
        lExpr = (expr *) NewUnopSubNode(SWIZZLE_Z_OP, SUBOP_Z(mask, newlen, len, base), fExpr);
        lExpr->un.type = GetStandardType(base, newlen, 0);
    } else {
        lExpr = fExpr;
    }
    return lExpr;
} // GenConvertVectorLength

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Duplicate Functions /////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * DuplicateNode() - Duplicate a node.
 *
 */

expr *DuplicateNode(expr *fExpr, void *arg1, int arg2)
{
    expr *lExpr;

    if (fExpr) {
        switch (fExpr->common.kind) {
        case SYMB_N:
            lExpr = (expr *) DupSymbNode(&fExpr->sym);
            break;
        case CONST_N:
            lExpr = (expr *) DupConstNode(&fExpr->co);
            break;
        case UNARY_N:
            lExpr = (expr *) DupUnaryNode(&fExpr->un);
            break;
        case BINARY_N:
            lExpr = (expr *) DupBinaryNode(&fExpr->bin);
            break;
        case TRINARY_N:
            lExpr = (expr *) DupTrinaryNode(&fExpr->tri);
            break;
        default:
            assert(!"bad kind to DuplicateNode()");
            lExpr = fExpr;
            break;
        }
    } else {
        lExpr = NULL;
    }
    return lExpr;
} // DuplicateNode

/*
 * DupExpr() - Duplicate an expression tree.
 *
 */

expr *DupExpr(expr *fExpr)
{
    return PreApplyToNodes(DuplicateNode, fExpr, NULL, 0);
} // DupExpr

/*
 * DuplicateStatement() - Duplicate a statement.
 *
 */

stmt *DuplicateStatement(stmt *fStmt, void *arg1, int arg2)
{
    stmt *lStmt;

    if (fStmt) {
        switch (fStmt->commonst.kind) {
        case EXPR_STMT:
            lStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, fStmt->exprst.exp);
            break;
        case IF_STMT:
            lStmt = (stmt *) NewIfStmt(&fStmt->commonst.loc, fStmt->ifst.cond,
                                fStmt->ifst.thenstmt, fStmt->ifst.elsestmt);
            break;
        case WHILE_STMT:
        case DO_STMT:
            lStmt = (stmt *) NewWhileStmt(&fStmt->commonst.loc, fStmt->whilest.kind,
                                fStmt->whilest.cond, fStmt->whilest.body);
            break;
        case FOR_STMT:
            lStmt = (stmt *) NewForStmt(&fStmt->commonst.loc, fStmt->forst.init,
                                fStmt->forst.cond, fStmt->forst.step, fStmt->forst.body);
            break;
        case BLOCK_STMT:
            lStmt = (stmt *) NewBlockStmt(&fStmt->commonst.loc, fStmt->blockst.body);
            break;
        case RETURN_STMT:
            lStmt = (stmt *) NewReturnStmt(&fStmt->commonst.loc, NULL, fStmt->exprst.exp);
            break;
        case DISCARD_STMT:
            lStmt = (stmt *) NewDiscardStmt(&fStmt->commonst.loc, fStmt->discardst.cond);
            break;
        case COMMENT_STMT:
            lStmt = (stmt *) NewCommentStmt(&fStmt->commonst.loc,
                                            GetAtomString(atable, fStmt->commentst.str));
            break;
        default:
            lStmt = fStmt;
            assert(!"DuplicateStatement() - not yet finished");
            break;
        }
        PreApplyToExpressions(DuplicateNode, lStmt, arg1, arg2);
    } else {
        lStmt = NULL;
    }
    return lStmt;
} // DuplicateStatement

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Named Constant Substitution /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ConvertNamedConstantsExpr() - Replace references to names constants with a const node.
 *
 */

expr *ConvertNamedConstantsExpr(expr *fExpr, void *arg1, int arg2)
{
    Symbol *lSymb;
    Type *lType;
    expr *lExpr;
    int base;

    lExpr = fExpr;
    if (fExpr) {
        switch (fExpr->common.kind) {
        case SYMB_N:

            // The only named constants are "true" and "false":

            lSymb = fExpr->sym.symbol;
            lType = lSymb->type;
            if (lSymb->kind == CONSTANT_S) {
                if (IsScalar(lType)) {
                    base = GetBase(lType);
                    switch (base) {
                    case TYPE_BASE_BOOLEAN:
                        lExpr = (expr *) NewBConstNode(BCONST_OP, lSymb->details.con.value,
                                                       TYPE_BASE_BOOLEAN);
                        break;
                    }
                }
            }
            break;
        case CONST_N:
        case UNARY_N:
        case BINARY_N:
        case TRINARY_N:
            break;
        default:
            FatalError("bad kind to ConvertNamedConstantsExpr()");
            break;
        }
    } else {
        lExpr = NULL;
    }
    return lExpr;
} // ConvertNamedConstantsExpr

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// Remove Empty Statements //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * RemoveEmptyStatementsStmt() - Expand inline function calls in a statement.
 *
 */

stmt *RemoveEmptyStatementsStmt(stmt *fStmt, void *arg1, int arg2)
{
    if (fStmt->commonst.kind == EXPR_STMT) {
        if (!fStmt->exprst.exp)
            fStmt = NULL;
    }
    return fStmt;
} // RemoveEmptyStatementsStmt

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////// Remove Empty Statements //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

typedef struct DebugFunctionData_Rec {
    Symbol *color;
    Symbol *flag;
    Symbol *outConnector;
    Symbol *COLSymb;
} DebugFunctionData;

/*
 * ConvertDebugCallsStmt() - Expand inline function calls in a statement.
 *
 */

stmt *ConvertDebugCallsStmt(stmt *fStmt, void *arg1, int arg2)
{
    DebugFunctionData *lDebugData = (DebugFunctionData *) arg1;
    expr *eExpr, *sExpr, *lExpr, *mExpr, *bExpr, *rExpr;
    stmt *lStmt, *mStmt;
    Symbol *lSymb;

    if (fStmt->commonst.kind == EXPR_STMT) {

        // Look for a call to "debug(float4)":

        eExpr = fStmt->exprst.exp;
        if (eExpr && eExpr->common.kind == BINARY_N && eExpr->bin.op == FUN_BUILTIN_OP) {
            sExpr = eExpr->bin.left;
            if (sExpr->common.kind == SYMB_N) {
                lSymb = sExpr->sym.symbol;
#define BUILTIN_GROUP_NV30FP_DBG     0
                if (lSymb->details.fun.group == BUILTIN_GROUP_NV30FP_DBG &&
                    lSymb->details.fun.index == 0x444)
                {
                    if (arg2) {

                        // Turn: "debug(arg);" statements into:
                        //
                        //     $debug-color@@(!$debug-set) = arg;
                        //     $debug-set = true;

                        rExpr = eExpr->bin.right->bin.left;
                        mExpr = GenSymb(lDebugData->flag);
                        mExpr = GenBoolNot(mExpr);
                        lExpr = GenSymb(lDebugData->color);
                        lExpr = GenCondSVAssign(lExpr, mExpr, rExpr, TYPE_BASE_FLOAT, 4);
                        lStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, lExpr);
                        lExpr = GenSymb(lDebugData->flag);
                        rExpr = GenBoolConst(1);
                        lExpr = GenBoolAssign(lExpr, rExpr);
                        mStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, lExpr);
                        lStmt->commonst.next = mStmt;
                        fStmt = lStmt;
                    } else {

                        // Eliminate: "debug(arg);" statements:

                        fStmt = NULL;
                    }
                }
            }
        }
    } else if (arg2 && fStmt->commonst.kind == RETURN_STMT) {
        rExpr = GenSymb(lDebugData->color);
        mExpr = GenSymb(lDebugData->flag);
        lExpr = GenSymb(lDebugData->outConnector);
        bExpr = GenMember(lDebugData->COLSymb);
        lExpr = GenMemberSelector(lExpr, bExpr);
        lExpr = GenCondSVAssign(lExpr, mExpr, rExpr, TYPE_BASE_FLOAT, 4);
        lStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, lExpr);
        lStmt->commonst.next = fStmt;
        fStmt = lStmt;
    }
    return fStmt;
} // ConvertDebugCallsStmt

/*
 * ConvertDebugCalls() - Convert calls to debug to either assignments to global variables, or
 *         delete them, depending on the value of DebugFlag.  Also defines global variables.
 *
 */

stmt *ConvertDebugCalls(SourceLoc *loc, Scope *fScope, stmt *fStmt, int DebugFlag)
{
    Symbol *debugColor, *debugSet;
    DebugFunctionData debugData;
    expr *lExpr, *rExpr;
    stmt *lStmt, *rStmt;
    StmtList lStatements;
    Type *lType;
    int vname, COLname;
    float fdata[4];

    rStmt = fStmt;
    if (DebugFlag) {
        lStatements.first = lStatements.last = NULL;
        //outputSymb = fScope->outConnector;

        // 1) Define global vars used by "-debug" mode:
        //
        //     float $debug-color[4];
        //     bool  $debug-set = 0.0f;

        vname = AddAtom(atable, "$debug-color");
        lType = GetStandardType(TYPE_BASE_FLOAT, 4, 0);
        debugColor = DefineVar(loc, fScope, vname, lType);
        vname = AddAtom(atable, "$debug-set");
        debugSet = DefineVar(loc, fScope, vname, BooleanType);
        lExpr = GenSymb(debugSet);
        rExpr = GenBoolConst(0);
        lExpr = GenBoolAssign(lExpr, rExpr);
        lStmt = (stmt *) NewExprStmt(loc, lExpr);
        AppendStatements(&lStatements, lStmt);

        // 1A) Must initialize $debug-color to something or else the code generator
        //     gets all bent out of shape:
        //
        //     $debug-color = { 0.0f, 0.0f, 0.0f, .0f };

        fdata[0] = fdata[1] = fdata[2] = fdata[3] = 0.0f;
        rExpr = GenFConstV(fdata, 4, TYPE_BASE_FLOAT);
        lExpr = GenSymb(debugColor);
        lExpr = GenVAssign(lExpr, rExpr, TYPE_BASE_FLOAT, 4);
        lStmt = (stmt *) NewExprStmt(loc, lExpr);
        AppendStatements(&lStatements, lStmt);

        // 2) Expand calls to "debug(float4);" into the following code:
        //
        //     $debug-color@@(!$debug-set) = float4;
        //     $debug-set = true;

        debugData.color = debugColor;
        debugData.flag = debugSet;
        debugData.outConnector = Cg->theHAL->varyingOut;
        assert(debugData.outConnector);
        COLname = AddAtom(atable, "COL");
        lType = debugData.outConnector->type;
        assert(IsCategory(lType, TYPE_CATEGORY_CONNECTOR));
        debugData.COLSymb = LookUpLocalSymbol(lType->str.members, COLname);

        // 3) And add the following statement to the end of the program (i.e. before each
        //    "return" statement):
        //
        //     output.COL@@($debug-set) = $debug-color;

        lStmt = PostApplyToStatements(ConvertDebugCallsStmt, fStmt, &debugData, 1);
        AppendStatements(&lStatements, lStmt);

        rStmt = lStatements.first;
    } else {
        rStmt = PostApplyToStatements(ConvertDebugCallsStmt, fStmt, NULL, 0);
    }
    return rStmt;
} // ConvertDebugCalls

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////// Flatten Chained Assignment Statements ///////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

int IsAssignSVOp(expr *fExpr)
{
    int lop;

    if (fExpr) {
        if (fExpr->common.kind == BINARY_N) {
            lop = fExpr->bin.op;
            if (lop == ASSIGN_OP || lop == ASSIGN_V_OP || lop == ASSIGN_GEN_OP ||
                lop == ASSIGN_MASKED_KV_OP)
            {
                return 1;
            }
        }
    }
    return 0;
} // IsAssignSVOp

int IsAssignCondSVOp(expr *fExpr)
{
    int lop;

    if (fExpr) {
        if (fExpr->common.kind == TRINARY_N) {
            lop = fExpr->tri.op;
            if (lop == ASSIGN_COND_OP || lop == ASSIGN_COND_V_OP || lop == ASSIGN_COND_SV_OP ||
                lop == ASSIGN_COND_GEN_OP)
            {
                return 1;
            }
        }
    }
    return 0;
} // IsAssignCondSVOp

int IsCastOp(expr *fExpr)
{
    int lop;

    if (fExpr) {
        if (fExpr->common.kind == UNARY_N) {
            lop = fExpr->un.op;
            if (lop == CAST_CS_OP || lop == CAST_CV_OP || lop == CAST_CM_OP)
            {
                return 1;
            }
        }
    }
    return 0;
} // IsCastOp

/*
 * NewTmp() - Return a symbol expression for a temp symbol that can
 * hold the value of fExpr.
 *
 */

symb *NewTmp(const expr *fExpr)
{
    Symbol *tmpSym;
    Type *tmpType = DupType(fExpr->common.type);
    tmpType->co.properties &= ~(TYPE_DOMAIN_MASK | TYPE_QUALIFIER_MASK);
    tmpSym = UniqueSymbol(CurrentScope, tmpType, VARIABLE_S);
    return NewSymbNode(VARIABLE_OP, tmpSym);
} // NewTmp

/*
 * FlattenChainedAssignmentsStmt() - Transform chained assignments into multiple simple
 *         assignments.
 *
 *  float3 A, C;  half B;
 *
 *  Simle case:
 *
 *  A = B = C;    (which is equivanemt to:)     A = (float) ( B = (half) C ) ;
 *
 *      =                        =               =
 *    /   \                    /   \           /   \
 *  A      (f)     >>-->>     B     (h)      A      (f)
 *            \                        \               \
 *              =                        C               B'
 *            /   \
 *          B      (h)
 *                    \
 *                      C
 *
 *  where B' is a duplicate of B.
 *
 *  If B contains any function calls, they must be hoisted prior to this step.
 *  
 */

stmt *FlattenChainedAssignmentsStmt(stmt *fStmt, void *arg1, int arg2)
{
    stmt *bStmt, *rStmt;
    expr *assigna, *assignb;
    expr *pb, **ppassignb;

    if (fStmt->commonst.kind == EXPR_STMT) {
        rStmt = fStmt;
        assigna = fStmt->exprst.exp;
        while (1) {
            if (IsAssignSVOp(assigna)) {
                ppassignb = &assigna->bin.right;
            } else if (IsAssignCondSVOp(assigna)) {
                ppassignb = &assigna->tri.arg3;
            } else {
                break;
            }
            // Traverse list of type casts, if any:
            while (IsCastOp(*ppassignb))
                ppassignb = &((**ppassignb).un.arg);
            if (IsAssignSVOp(*ppassignb)) {
                pb = (**ppassignb).bin.left;
            } else if (IsAssignCondSVOp(*ppassignb)) {
                pb = (**ppassignb).tri.arg1;
            } else {
                break;
            }
            assignb = *ppassignb;
            bStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, assignb);
            *ppassignb = PreApplyToNodes(DuplicateNode, pb, arg1, arg2);
            bStmt->commonst.next = rStmt;
            rStmt = bStmt;
            assigna = assignb;
        }
    } else {
        rStmt = fStmt;
    }
    return rStmt;
} // FlattenChainedAssignmentsStmt

/*
 * PutIndexEpxrsInTemps() - Puts all array indicies in temps and
 * builds a comma list of assignments of the indices to the
 * temporaries.
 *
 * A[e1]...[eN] -> t1 = e1, ..., tN = eN : A[t1]...[tN]
 */
expr *PutIndexEpxrsInTemps(expr *fExpr)
{
    expr *assignments = NULL;
    expr *assign;
    expr *tmp;
    
    assert(IsArrayIndex(fExpr));
    if (IsArrayIndex(fExpr->bin.left))
        assignments = PutIndexEpxrsInTemps(fExpr->bin.left);

    tmp = (expr *) NewTmp(fExpr->bin.right);
    assign = NewSimpleAssignment(Cg->pLastSourceLoc, DupNode(tmp), fExpr->bin.right, 0);

    if (!assignments)
        assignments = assign;
    else
        assignments = (expr *) NewBinopNode(COMMA_OP, assignments, assign);

    if (IsArrayIndex(fExpr->bin.right))
        assignments = (expr *) NewBinopNode(COMMA_OP, assignments,
                                            PutIndexEpxrsInTemps(fExpr->bin.right));

    fExpr->bin.right = tmp;
    return assignments;
} // PutIndexEpxrsInTemps

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////// Expand Increment/Decrement Expressions ////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ExpandIncDecExpr() - Expand increment/decrement operations.
 *
 * Pre increment/decrement is the simple case:
 *
 *   ++A -> A += 1
 *
 * Post increment/decrement is a little more tricky:
 *
 *   If A is simple (i.e. not an array reference):
 *
 *     A++ -> tmp = A, A += 1, tmp
 *
 *   If A is an array reference:
 *
 *     A[i1]...[iN]++ ->
 *       tmpi1 = i1, ..., tmpiN = iN, tmpv = A[tmpi1]...[tmpiN],
 *       A[tmpi1]...[tmpiN] += 1, tmpv
 *
 */

expr *ExpandIncDecExpr(expr *fExpr, void *arg1, int arg2)
{
    int pre = 0;
    int newop = -1;
    expr *oneExpr;
    expr *result = NULL;

    if (fExpr->common.kind == UNARY_N) {
        switch (fExpr->un.op) {
        case PREDEC_OP:
            pre = 1;
        case POSTDEC_OP:
            newop = ASSIGNMINUS_OP;
            break;
        case PREINC_OP:
            pre = 1;
        case POSTINC_OP:
            newop = ASSIGNPLUS_OP;
            break;
        }
    }

    if (newop == -1)
        return fExpr;

    oneExpr = (expr *) NewIConstNode(ICONST_OP, 1, TYPE_BASE_INT); 
    
    if (pre)
        result = (expr *) NewBinopNode(newop, fExpr->un.arg, oneExpr);
    else {
        expr *idxAssigns = IsArrayIndex(fExpr->un.arg)
            ? PutIndexEpxrsInTemps(fExpr->un.arg) : NULL;
        expr *tmp = (expr *) NewTmp(fExpr->un.arg);
        expr *tmpAssign = NewSimpleAssignment(Cg->pLastSourceLoc, tmp, fExpr->un.arg, 0);
        expr *incdec = (expr *) NewBinopNode(newop, DupNode(fExpr->un.arg), oneExpr);
        expr *rval = DupNode(tmp);
        result = (expr *) NewBinopNode(COMMA_OP, incdec, rval);
        result = (expr *) NewBinopNode(COMMA_OP, tmpAssign, result);
        if (idxAssigns)
            result = (expr *) NewBinopNode(COMMA_OP, idxAssigns, result);
    }

    return result;

} // ExpandIncDecExpr

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////// Expand Compound Assignment Expressions ////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * ExpandCompoundAssignmentExpr() - Transform compound assignments
 * into simple assignments.
 *
 * If A is simple (i.e. not an array reference):
 *
 *   A op= B -> A = A op B
 *
 * If A is an array reference:
 *
 *   A[e1]...[eN] op= B ->
 *     tmp1 = e1, ..., tmpN = eN, A[tmp1]...[tmpN] = A[tmp1]...[tmpN] op B
 *  
 */

expr *ExpandCompoundAssignmentExpr(expr *fExpr, void *arg1, int arg2)
{
    int newop = -1;
    int opname = 0;
    int intOnly = 0;
    expr *lexpr, *rExpr;
    expr *result = NULL;
    expr *idxAssigns = NULL;

    if (fExpr->common.kind == BINARY_N) {
        switch (fExpr->bin.op) {
        case ASSIGNMINUS_OP:
            newop = SUB_OP;
            opname = '-';
            break;
        case ASSIGNMOD_OP:
            newop = MOD_OP;
            opname = '%';
            intOnly = 1;
            break;
        case ASSIGNPLUS_OP:
            newop = ADD_OP;
            opname = '+';
            break;
        case ASSIGNSLASH_OP:
            newop = DIV_OP;
            opname = '/';
            break;
        case ASSIGNSTAR_OP:
            newop = MUL_OP;
            opname = '*';
            break;
        }
    }

    if (newop == -1)
        return fExpr;

    lexpr = fExpr->bin.left;
    rExpr = fExpr->bin.right;

    if (IsArrayIndex(lexpr))
        idxAssigns = PutIndexEpxrsInTemps(lexpr);
    
    result = NewBinaryOperator(Cg->pLastSourceLoc, newop, opname, lexpr, rExpr, intOnly);
    result = NewSimpleAssignment(Cg->pLastSourceLoc, DupNode(lexpr), result, 0);
    if (idxAssigns)
        result = (expr *) NewBinopNode(COMMA_OP, idxAssigns, result);

    return result;
} // ExpandCompoundAssignmentExpr

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////// Flatten Chained Assignment Statements ///////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

typedef struct MData_rec {
    Scope *scope;
    int numtemps;
    StmtList statements;
} MData;

/*
 * IsConstant()
 *
 */

int IsConstant(expr *fExpr)
{
    switch (fExpr->common.kind) {
    case CONST_N:
        return 1;
    default:
        break;
    }
    return 0;
} // IsConstant

/*
 * IsVariable()
 *
 */

int IsVariable(expr *fExpr)
{
    switch (fExpr->common.kind) {
    case SYMB_N:
        return 1;
    default:
        break;
    }
    return 0;
} // IsVariable

/*
 * IsCompileTimeAddress()
 *
 */

int IsCompileTimeAddress(expr *fExpr)
{
    if (fExpr->common.IsLValue) {
        switch (fExpr->common.kind) {
        case SYMB_N:
            return 1;
        case BINARY_N:
            switch (fExpr->bin.op) {
            case MEMBER_SELECTOR_OP:
                return IsCompileTimeAddress(fExpr->bin.left);
            case ARRAY_INDEX_OP:
                if (IsCompileTimeAddress(fExpr->bin.left)) {
                    if (IsConstant(fExpr->bin.right)) {
                        return 1;
                    }
                }
                break;
            default:
                break;
            }
            break;
        default:
            break;
        }
    }
    return 0;
} // IsCompileTimeAddress

/*
 * GetConstIndex()
 *
 */

int GetConstIndex(expr *fExpr)
{
    switch (fExpr->common.kind) {
    case CONST_N:
        switch (fExpr->co.op) {
        case ICONST_OP:
        case BCONST_OP:
            return fExpr->co.val[0].i;
            break;
        case FCONST_OP:
        case HCONST_OP:
        case XCONST_OP:
            return (int) fExpr->co.val[0].f;
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return 0;
} // GetConstIndex

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////// Flatten comma expressions into statement lists /////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

static int IsComma(const expr *fExpr)
{
    return fExpr->common.kind == BINARY_N && fExpr->bin.op == COMMA_OP;
} // IsComma

/*
 * FlattenCommasExpr()
 *
 * op (a, b) -> a, (op b)
 * (a, b) op c -> a, (b op c)
 * A[e1]...[eN] = (b, c) -> (tmp1 = e1), ..., (tmpN = eN), b, A[tmp1]...[tmpN] = c
 * a = (b, c) -> b, a = c
 * a op (b, c) -> (tmpa = a), b, (tmpa op c)
 * (a, b) ? c : d -> a, (b ? c : d)
 * a ? (b, c) : d -> (tmpa = a), b, (tmpa ? c : d)
 * a ? b : (c, d) -> (tmpa = a), (tmpb = b), c, (tmpa ? tmpb : d)
 */

static expr *FlattenCommasExpr(expr *fExpr, void *arg1, int arg2)
{
    switch (fExpr->common.kind) {
    case UNARY_N:
        if (IsComma(fExpr->un.arg)) {
            expr *comma = fExpr->un.arg;
            fExpr->un.arg = comma->bin.right;
            fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, fExpr);
        }
        break;
    case BINARY_N: {
        binary *bin = &fExpr->bin;
        if (IsComma(bin->left)) {
            expr *comma = bin->left;
            bin->left = comma->bin.right;
            fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, (expr *) bin);
        }
        if (bin->right && IsComma(bin->right)) {
            // Assignments/lvalues are special
            if (bin->op == ASSIGN_OP || bin->op == ASSIGN_V_OP) {
                expr *idxAssigns = IsArrayIndex(bin->left)
                    ? PutIndexEpxrsInTemps(bin->left) : NULL;
                fExpr = (expr *) NewBinopNode(COMMA_OP, bin->right->bin.left,
                                              fExpr);
                bin->right = bin->right->bin.right;
                if (idxAssigns)
                    fExpr = (expr *) NewBinopNode(COMMA_OP, idxAssigns, fExpr);
            } else if (bin->op != COMMA_OP) {
                // no need to lift comma ops above a comma op
                expr *comma = bin->right;
                expr *tmp = (expr *) NewTmp(bin->left);
                expr *assign = NewSimpleAssignment(Cg->pLastSourceLoc, tmp, bin->left, 0);
                bin->left = DupNode(tmp);
                bin->right = comma->bin.right;
                fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, (expr *) bin);
                fExpr = (expr *) NewBinopNode(COMMA_OP, assign, fExpr);
            }
        }
        break;
    }
    case TRINARY_N: {
        trinary *tri = &fExpr->tri;
        if (IsComma(tri->arg1)) {
            expr *comma = tri->arg1;
            tri->arg1 = comma->bin.right;
            fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, (expr *) tri);
        }
        if (IsComma(tri->arg2)) {
            expr *comma = tri->arg2;
            expr *tmp = (expr *) NewTmp(tri->arg1);
            expr *assign = NewSimpleAssignment(Cg->pLastSourceLoc, tmp, tri->arg1, 0);
            tri->arg1 = DupNode(tmp);
            tri->arg2 = comma->bin.right;
            fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, (expr *) tri);
            fExpr = (expr *) NewBinopNode(COMMA_OP, assign, fExpr);
        }
        if (IsComma(tri->arg3)) {
            expr *comma = tri->arg3;
            expr *tmp1 = (expr *) NewTmp(tri->arg1);
            expr *tmp2 = (expr *) NewTmp(tri->arg2);
            expr *assign1 = NewSimpleAssignment(Cg->pLastSourceLoc, tmp1, tri->arg1, 0);
            expr *assign2 = NewSimpleAssignment(Cg->pLastSourceLoc, tmp2, tri->arg2, 0);
            tri->arg1 = DupNode(tmp1);
            tri->arg2 = DupNode(tmp2);
            tri->arg3 = comma->bin.right;
            fExpr = (expr *) NewBinopNode(COMMA_OP, comma->bin.left, (expr *) tri);
            fExpr = (expr *) NewBinopNode(COMMA_OP, assign2, fExpr);
            fExpr = (expr *) NewBinopNode(COMMA_OP, assign1, fExpr);
        }
        break;
    }
    }

    return fExpr;
} // FlattenCommasExpr

/*
 * LinearizeCommasExpr() - Gets rid of all top-level comma expressions
 * by pulling them out into an expression list (returned through
 * arg1).
 *
 */

static expr *LinearizeCommasExpr(expr *fExpr, void *arg1, int arg2)
{
    stmt ** fStmts = (stmt **) arg1;
    while (IsComma(fExpr)) {
        stmt * fStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, fExpr->bin.left);
        *fStmts = ConcatStmts(*fStmts, fStmt);
        fExpr = fExpr->bin.right;
    }

    return fExpr;
} // LinearizeCommasExpr

/*
 * FlattenCommas()
 *
 */

static stmt *FlattenCommasStmt(stmt *fStmt, void *arg1, int arg2)
{
    stmt *preCommaStmts = NULL;
    PreApplyToExpressionsLocal(FlattenCommasExpr, fStmt, NULL, 0);
    PreApplyToExpressionsLocal(LinearizeCommasExpr, fStmt, &preCommaStmts, 0);
    return ConcatStmts(preCommaStmts, fStmt);
} // FlattenCommas

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Chop Matrices up into Vectors ///////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * DeconstructMatricesExpr()
 *
 */

expr *DeconstructMatricesExpr(expr *fExpr, void *arg1, int arg2)
{
    Symbol *lSymb;
    MData *mdata;
    Type *lType;
    int base, len, len2;
    int vname, index;

    mdata = (MData *) arg1;
    switch (fExpr->common.kind) {
    case BINARY_N:
        switch (fExpr->bin.op) {
        case ARRAY_INDEX_OP:
            lType = fExpr->bin.type;
            if (IsMatrix(fExpr->bin.left->common.type, &len, &len2)) {
                base = GetBase(lType);
                if (IsConstant(fExpr->bin.right)) {
                    if (IsVariable(fExpr->bin.left)) {
                        index = GetConstIndex(fExpr->bin.right);
                        vname = GenerateIndexName(fExpr->bin.left, index);
                        lSymb = LookUpLocalSymbol(mdata->scope, vname);
                        if (!lSymb)
                            lSymb = DefineVar(&lSymb->loc, mdata->scope, vname, lType);
                        fExpr = GenSymb(lSymb);
                    } else {
                        SemanticError(Cg->pLastSourceLoc, ERROR___MATRIX_NOT_SIMPLE);
                    }
                } else {
                    SemanticError(Cg->pLastSourceLoc, ERROR___ARRAY_INDEX_NOT_CONSTANT);
                }
            }
            break;
        default:
            break;
        }
        break;
    default:
        break;
    }
    return fExpr;
} // DeconstructMatricesExpr

/*
 * DeconstructMatricesStmt()
 *
 */

stmt *DeconstructMatricesStmt(stmt *fStmt, void *arg1, int arg2)
{
    MData *mdata;

    mdata = (MData *) arg1;
    mdata->statements.first = mdata->statements.last = NULL;
    PostApplyToExpressionsLocal(DeconstructMatricesExpr, fStmt, arg1, arg2);
    if (mdata->statements.first) {
        mdata->statements.last->commonst.next = fStmt;
        fStmt = mdata->statements.first;
    }
    return fStmt;
} // DeconstructMatricesStmt

/*
 * DeconstructMatrices()
 *
 */

stmt *DeconstructMatrices(Scope *fscope, stmt *fStmt)
{
    MData mdata;

    InternalError(Cg->pLastSourceLoc, ERROR___NO_MATRIX_DECONSTRUCTION);
    mdata.scope = fscope;
    mdata.numtemps = 0;
    fStmt = PreApplyToStatements(DeconstructMatricesStmt, fStmt, &mdata, 0);
    return fStmt;
} // DeconstructMatrices

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Flatten Struct Assignments ////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * AssignStructMembers() - Assign individual memebers of one struct to another.
 *
 */

static void AssignStructMembers(StmtList *fStatements, int fop, Type *fType, expr *varExpr,
                                expr *valExpr, expr *condExpr, int VectorCond)
{
    expr *lExpr, *mExpr, *rExpr;
    int base, len, len2;
    Symbol *lSymb;
    Type *lType;
    stmt *lStmt;

    lSymb = fType->str.members->symbols;
    while (lSymb) {
        len = len2 = 0;
        lType = lSymb->type;
        if (IsScalar(lType) || IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
            base = GetBase(lType);
            lExpr = DupExpr(varExpr);
            mExpr = GenMember(lSymb);
            lExpr = GenMemberSelector(lExpr, mExpr);
            rExpr = DupExpr(valExpr);
            mExpr = GenMember(lSymb);
            rExpr = GenMemberSelector(rExpr, mExpr);
            if (condExpr != NULL) {
                if (len2 > 0) {
                    lExpr = GenCondGenAssign(lExpr, rExpr, condExpr);
                } else if (len > 0) {
                    if (VectorCond) {
                        lExpr = GenCondSVAssign(lExpr, rExpr, condExpr, base, len);
                    } else {
                        lExpr = GenCondVAssign(lExpr, rExpr, condExpr, base, len);
                    }
                } else {
                    lExpr = GenCondSAssign(lExpr, rExpr, condExpr, base);
                }
                lStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, lExpr);
            } else {
                if (len2 > 0) {
                    lExpr = GenMAssign(lExpr, rExpr, base, len, len2);
                } else if (len > 0) {
                    lExpr = GenVAssign(lExpr, rExpr, base, len);
                } else {
                    lExpr = GenSAssign(lExpr, rExpr, base);
                }
                lStmt = (stmt *) NewExprStmt(Cg->pLastSourceLoc, lExpr);
            }
            AppendStatements(fStatements, lStmt);
        } else {
            switch (GetCategory(lType)) {
            case TYPE_CATEGORY_STRUCT:
                lExpr = DupExpr(varExpr);
                mExpr = GenMember(lSymb);
                lExpr = GenMemberSelector(lExpr, mExpr);
                rExpr = DupExpr(valExpr);
                mExpr = GenMember(lSymb);
                rExpr = GenMemberSelector(rExpr, mExpr);
                AssignStructMembers(fStatements, fop, lType, lExpr, rExpr, condExpr, VectorCond);
                break;
            case TYPE_CATEGORY_ARRAY:
                // XYZZY Not Done Yet XYZZY //
                break;
            default:
                break;
            }
        }
        lSymb = lSymb->next;
    }
} // AssignStructMembers

/*
 * FlattenStructAssignment() - Convert struct assignments into multiple assignments of members.
 *
 */

stmt *FlattenStructAssignment(stmt *fStmt, void *arg1, int flevel)
{
    stmt *rStmt;
    expr *eExpr, *lExpr, *rExpr, *cExpr;
    int lop, lsubop;
    StmtList lStatements;
    Type *lType;

    if (fStmt) {
        switch (fStmt->commonst.kind) {
        case EXPR_STMT:
            eExpr = fStmt->exprst.exp;
            if (IsAssignSVOp(eExpr)) {
                lType = eExpr->bin.type;
                if (IsStruct(lType)) {
                    if (IsAssignCondSVOp(eExpr)) {
                        lop = eExpr->tri.op;
                        lExpr = eExpr->tri.arg1;
                        cExpr = eExpr->tri.arg2;
                        rExpr = eExpr->tri.arg3;
                        if (IsScalar(cExpr->common.type)) {
                            AssignStructMembers(&lStatements, lop, lType, lExpr, rExpr, cExpr, 0);
                        } else {
                            AssignStructMembers(&lStatements, lop, lType, lExpr, rExpr, cExpr, 1);
                        }
                        rStmt = lStatements.first;
                    } else {
                        lop = eExpr->bin.op;
                        lExpr = eExpr->bin.left;
                        rExpr = eExpr->bin.right;
                        lsubop = eExpr->bin.subop;
                        lStatements.first = NULL;
                        lStatements.last = NULL;
                        AssignStructMembers(&lStatements, lop, lType, lExpr, rExpr, NULL, 0);
                        rStmt = lStatements.first;
                    }
                } else {
                    rStmt = fStmt;
                }
            } else {
                rStmt = fStmt;
            }
            break;
        default:
            rStmt = fStmt;
            break;
        }
    } else {
        rStmt = fStmt;
    }
    return rStmt;
} // FlattenStructAssignment

/*
 * FlattenStructAssignments() - Convert struct assignments into multiple member assignments.
 *  
 */

static stmt *FlattenStructAssignments(Scope *fScope, stmt *fStmt)
{
    stmt *lStmt;

    lStmt = PreApplyToStatements(FlattenStructAssignment, fStmt, NULL, 0);
    return lStmt;
} // FlattenStructAssignments


///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Flatten If Statements ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * FlattenIfStatementsStmt() - Convert if statements into conditional assignments.
 *
 *     if (A > B)
 *         C = D;
 *     else
 *         E = F;
 *
 * becomes:
 *
 *     $if1 = A > B;
 *     C@@($if1) = D;
 *     E@@(!$if1) = F;
 *
 * and:
 *
 *     if (A > B)
 *         if (C > D)
 *             E = F;
 *         else
 *             G = H;
 *     else
 *         if (J > K)
 *             L = M;
 *         else
 *             N = P;
 *
 * becomes:
 *
 *     $if1 = A > B;
 *     $ife2 = C > D;
 *     $if2 = $if1 && $ife2;
 *     E@@($if2) = F;
 *     $if2 = $if1 && !$ife2;
 *     G@@($if2) = H;
 *     $ife2 = J > K;
 *     $if2 = !$if1 && $ife2;
 *     L@@($if2) = M;
 *     $if2 = !$if1 && !$ife2;
 *     N@@($if2) = P;
 *
 * Existing conditional assignments:
 *
 *     A@@XZ = B;
 *     C@@(D) = E;
 *
 * become:
 *
 *     A@@({ $if1, 0, $if1, 0 }) = B;
 *     C@@($if1@xyzw && D) = E;     or:        C@@($if1 && D) = E;
 *
 * Issues:
 *
 *   1) "out" parameters to function calls: not a problem if function calls
 *      have been previously inlined.
 *   2) Assumes "chained" assignments have already been split into simple assignments.
 *   3) Assumes all large asignments (structs, matrices, vectors > 4) have been eliminated.
 *
 */

typedef struct FlattenIf_Rec {
    Scope *funScope;
    Symbol *ifSymbTotal;  // Current combined if value: "$if2" for level 2, NULL for level 0.
    Symbol *ifSymbParent; // Enclosing if's combined value: "$if1" for level 2, NULL for level <= 1.
    Symbol *ifSymbLocal;  // Current level's simple if value:  "$ife2" for level 2
} FlattenIf;

static stmt *FlattenIfStatementsStmt(stmt *fStmt, void *arg1, int flevel)
{
#define IF_ROOT "$if"
#define IF_ROOT_E "$iflocal"

    stmt *rStmt, *lStmt, *nStmt;
    expr *eExpr, *lExpr, *rExpr, *ifvar, *nExpr, *mExpr, *tExpr;
    int level, lop, lsubop, nop, nsubop, vname, mask, len, base, ii;
    FlattenIf *lFlatten;
    Symbol *lSymbTotal, *lSymbLocal, *ifSymbTotalSave, *ifSymbParentSave;
    StmtList lStatements;
    Type *lType;

    if (fStmt) {
        lFlatten = (FlattenIf *) arg1;
        level = flevel >= 0 ? flevel : -flevel;
        switch (fStmt->commonst.kind) {
        case IF_STMT:
            
            // Find $if1 variable(s) for this level:

            vname = GetNumberedAtom(IF_ROOT, level + 1, 1, '\0');
            lSymbTotal = LookUpSymbol(lFlatten->funScope, vname);
            if (!lSymbTotal) {
                lSymbTotal = DefineVar(&fStmt->commonst.loc, lFlatten->funScope, vname, BooleanType);
            }
            if (level > 0) {
                vname = GetNumberedAtom(IF_ROOT_E, level + 1, 1, '\0');
                lSymbLocal = LookUpSymbol(lFlatten->funScope, vname);
                if (!lSymbLocal) {
                    lSymbLocal = DefineVar(&fStmt->commonst.loc, lFlatten->funScope, vname, BooleanType);
                }
            } else {
                lSymbLocal = lSymbTotal;
            }

            // Create assignment statement for local expression:

            lStatements.first = NULL;
            lStatements.last = NULL;
            lExpr = GenSymb(lSymbLocal);
            lExpr = GenBoolAssign(lExpr, fStmt->ifst.cond);
            lStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, lExpr);
            AppendStatements(&lStatements, lStmt);

            ifSymbTotalSave = lFlatten->ifSymbTotal;
            ifSymbParentSave = lFlatten->ifSymbParent;
            lFlatten->ifSymbParent = lFlatten->ifSymbLocal;
            lFlatten->ifSymbLocal = lSymbLocal;
            lFlatten->ifSymbTotal = lSymbTotal;

            // Compute effective Boolean expression if necessary:

            if (level > 0) {
                lExpr = GenSymb(lFlatten->ifSymbParent);
                if (flevel == -1) {
                    // Top level if's don't create a negated value:
                    lExpr = GenBoolNot(lExpr);
                }
                rExpr = GenSymb(lSymbLocal);
                rExpr = GenBoolAnd(lExpr, rExpr);
                lExpr = GenSymb(lFlatten->ifSymbTotal);
                lExpr = GenBoolAssign(lExpr, rExpr);
                lStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, lExpr);
                AppendStatements(&lStatements, lStmt);
            }

            // Walk sub-statements and transform assignments into conditional assignments:

            lStmt = fStmt->ifst.thenstmt;
            fStmt->ifst.thenstmt = NULL;
            while (lStmt) {
                nStmt = lStmt->commonst.next;
                lStmt->commonst.next = NULL;
                lStmt = FlattenIfStatementsStmt(lStmt, arg1, level + 1);
                AppendStatements(&lStatements, lStmt);
                lStmt = nStmt;
            }
            if (fStmt->ifst.elsestmt) {

                // Compute effective Boolean expression if necessary:

                if (level > 0) {
                    lExpr = GenSymb(lFlatten->ifSymbParent);
                    if (flevel == -1)
                        lExpr = GenBoolNot(lExpr);
                    rExpr = GenSymb(lSymbLocal);
                    rExpr = GenBoolNot(rExpr);
                    rExpr = GenBoolAnd(lExpr, rExpr);
                    lExpr = GenSymb(lFlatten->ifSymbTotal);
                    lExpr = GenBoolAssign(lExpr, rExpr);
                    lStmt = (stmt *) NewExprStmt(&fStmt->commonst.loc, lExpr);
                    AppendStatements(&lStatements, lStmt);
                }
                lStmt = fStmt->ifst.elsestmt;
                fStmt->ifst.elsestmt = NULL;
                while (lStmt) {
                    nStmt = lStmt->commonst.next;
                    lStmt->commonst.next = NULL;
                    lStmt = FlattenIfStatementsStmt(lStmt, arg1, -(level + 1));
                    AppendStatements(&lStatements, lStmt);
                    lStmt = nStmt;
                }
            }
            lFlatten->ifSymbTotal = ifSymbTotalSave;
            lFlatten->ifSymbLocal = lFlatten->ifSymbParent;
            lFlatten->ifSymbParent = ifSymbParentSave;
            rStmt = lStatements.first;
            break;
        case EXPR_STMT:
            if (level > 0) {
                eExpr = fStmt->exprst.exp;
                if (IsAssignSVOp(eExpr)) {
                    lExpr = eExpr->bin.left;
                    rExpr = eExpr->bin.right;
                    lop = eExpr->bin.op;
                    lsubop = eExpr->bin.subop;
                    lType = eExpr->bin.type;
                    ifvar = GenSymb(lFlatten->ifSymbTotal);
                    if (flevel == -1)
                        ifvar = GenBoolNot(ifvar);
                    if (lop == ASSIGN_MASKED_KV_OP) {
                        mask = SUBOP_GET_MASK(lsubop);
                        len = SUBOP_GET_S(lsubop);
                        base = SUBOP_GET_T(lsubop);
                        // Create vector of $if/FALSE values:
                        mExpr = NULL;
                        for (ii = 0; ii < len; ii++) {
                            if (mask & 1) {
                                tExpr = GenSymb(lFlatten->ifSymbTotal);
                            } else {
                                tExpr = GenBoolConst(0);
                            }
                            mExpr = GenExprList(mExpr, tExpr, BooleanType);
                            mask >>= 1;
                        }
                        ifvar = (expr *) NewUnopSubNode(VECTOR_V_OP, SUBOP_V(len, TYPE_BASE_BOOLEAN), mExpr);
                        ifvar->un.type = GetStandardType(TYPE_BASE_BOOLEAN, len, 0);
                        nop = ASSIGN_COND_V_OP;
                        nsubop = SUBOP_V(len, base);
                    } else {
                        // Normal assign.  Convert it to simple conditional assignment:
                        switch (lop) {
                        case ASSIGN_OP:
                            nop = ASSIGN_COND_OP;
                            nsubop = lsubop;
                            break;
                        case ASSIGN_V_OP:
                            nop = ASSIGN_COND_SV_OP;
                            nsubop = lsubop;
                            break;
                        case ASSIGN_GEN_OP:
                            nop = ASSIGN_COND_GEN_OP;
                            nsubop = lsubop;
                            break;
                        default:
                            assert(0);
                            break;
                        }
                    }
                    nExpr = (expr *) NewTriopSubNode(nop, nsubop, lExpr, ifvar, rExpr);
                    nExpr->tri.type = lType;
                    fStmt->exprst.exp = nExpr;
                    rStmt = fStmt;
                } else if (IsAssignCondSVOp(eExpr)) {
                    switch (eExpr->tri.op) {
                    case ASSIGN_COND_OP:
                    case ASSIGN_COND_SV_OP:
                    case ASSIGN_COND_GEN_OP:
                        ifvar = GenSymb(lFlatten->ifSymbTotal);
                        if (flevel == -1)
                            ifvar = GenBoolNot(ifvar);
                        eExpr->tri.arg2 = GenBoolAnd(eExpr->tri.arg2, ifvar);
                        break;
                    case ASSIGN_COND_V_OP:
                        lsubop = eExpr->tri.subop;
                        len = SUBOP_GET_S(lsubop);
                        // Create vector of $if values:
                        mExpr = NULL;
                        for (ii = 0; ii < len; ii++) {
                            tExpr = GenSymb(lFlatten->ifSymbTotal);
                            if (flevel == -1)
                                tExpr = GenBoolNot(tExpr);
                            mExpr = GenExprList(mExpr, tExpr, BooleanType);
                        }
                        ifvar = (expr *) NewUnopSubNode(VECTOR_V_OP, SUBOP_V(len, TYPE_BASE_BOOLEAN), mExpr);
                        ifvar->un.type = GetStandardType(TYPE_BASE_BOOLEAN, len, 0);
                        eExpr->tri.arg2 = GenBoolAndVec(eExpr->tri.arg2, ifvar, len);
                        break;
                    default:
                        assert(0);
                        break;
                    }
                    rStmt = fStmt;
                } else {
                    rStmt = fStmt;
                }
            } else {
                rStmt = fStmt;
            }
            break;
        case BLOCK_STMT:
            if (level > 0) {
                lStatements.first = NULL;
                lStatements.last = NULL;
                lStmt = fStmt->blockst.body;
                while (lStmt) {
                    nStmt = lStmt->commonst.next;
                    lStmt->commonst.next = NULL;
                    lStmt = FlattenIfStatementsStmt(lStmt, arg1, flevel);
                    AppendStatements(&lStatements, lStmt);
                    lStmt = nStmt;
                }
                rStmt = lStatements.first;
            } else {
                rStmt = fStmt;
            }
            break;
        case DISCARD_STMT:
            if (level > 0) {
                ifvar = GenSymb(lFlatten->ifSymbTotal);
                if (flevel == -1)
                    ifvar = GenBoolNot(ifvar);
                if (fStmt->discardst.cond->un.arg) {
                    lType = fStmt->discardst.cond->common.type;
                    if (IsVector(lType, &len)) {
                        ifvar = GenBoolSmear(ifvar, len);
                        ifvar = GenBoolAndVec(ifvar, fStmt->discardst.cond->un.arg, len);
                    } else {
                        ifvar = GenBoolAnd(ifvar, fStmt->discardst.cond->un.arg);
                    }
                }
                fStmt->discardst.cond->un.arg = ifvar;
            }
            rStmt = fStmt;
            break;
        default:
            rStmt = fStmt;
            break;
        }
    } else {
        rStmt = fStmt;
    }
    return rStmt;
} // FlattenIfStatementsStmt

/*
 * FlattenIfStatements() - Convert if statements into conditional assignments.
 *  
 */

static stmt *FlattenIfStatements(Scope *fScope, stmt *fStmt)
{
    FlattenIf lFlatten;
    stmt *lStmt;

    lFlatten.funScope = fScope;
    lFlatten.ifSymbTotal = NULL;
    lFlatten.ifSymbParent = NULL;
    lFlatten.ifSymbLocal = NULL;
    lStmt = PreApplyToStatements(FlattenIfStatementsStmt, fStmt, &lFlatten, 0);
    return lStmt;
} // FlattenIfStatements

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Output Support Functions //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * lOutputVendorVersion() - Output vendor and profile version.
 *
 */

static void lOutputVendorVersion(FILE *out, slHAL *fHAL)
{

    // 1. Profile:

    fprintf(out, "%svendor %s\n", fHAL->comment, fHAL->vendor);

    // 2. Program:

    fprintf(out, "%sversion %s\n", fHAL->comment, fHAL->version);

} // lOutputVendorVersion

static void lPrintUniformVariableDescription(FILE *out, const char *symbolName, Type *fType,
                                             Binding *fBind, int bindOffset,
                                             const char *semanticName, int paramNo)
{
    char str1[100], str2[100], newSymbolName[1024], newSemanticName[1024];
    int category, len, len2, ii;
    Symbol *lSymb;

    category = GetCategory(fType);
    switch (category) {
    case TYPE_CATEGORY_SCALAR:
    case TYPE_CATEGORY_ARRAY:
        if (IsScalar(fType) || IsVector(fType, &len) || IsMatrix(fType, &len, &len2)) {
            FormatTypeStringRT(str1, sizeof(str1), str2, sizeof(str1), fType, 1);
            fprintf(out, "%svar %s ", Cg->theHAL->comment, str1);
            fprintf(out, "%s%s : %s", symbolName, str2, semanticName);
            if (fBind) {
                switch (fBind->none.kind) {
                case BK_REGARRAY:
                    fprintf(out, " : %s[%d]", GetAtomString(atable, fBind->reg.rname),
                            fBind->reg.regno + bindOffset);
                    ii = GetQuadRegSize(fType);
                    if (ii > 1)
                        fprintf(out, ", %d", ii);
                    break;
                case BK_TEXUNIT:
                    fprintf(out, " : texunit %d", fBind->texunit.unitno);
                    break;
                case BK_NONE:
                    fprintf(out, " : ");
                    break;
                default:
                    fprintf(out, " : %s", "?????");
                    break;
                }
            } else {
                // No binding
                fprintf(out, " : ");
            }
            fprintf(out, " : %d : %d", paramNo, 1);
            fprintf(out, "\n");
        } else {
            for (ii = 0; ii < fType->arr.numels; ii++) {
                sprintf(newSymbolName, "%s[%d]", symbolName, ii);
                if (semanticName && semanticName[0] != '\0') {
                    sprintf(newSemanticName, "%s[%d]", semanticName, ii);
                } else {
                    newSemanticName[0] = '\0';
                }
                lPrintUniformVariableDescription(out, newSymbolName, fType->arr.eltype, fBind,
                        bindOffset, newSemanticName, paramNo);
                bindOffset += GetQuadRegSize(fType->arr.eltype);
            }
        }
        break;
    case TYPE_CATEGORY_STRUCT:
        lSymb = fType->str.members->symbols;
        while (lSymb) {
            sprintf(newSymbolName, "%s.%s", symbolName, GetAtomString(atable, lSymb->name));
            if (semanticName && semanticName[0] != '\0') {
                sprintf(newSemanticName, "%s.%s", semanticName, GetAtomString(atable, lSymb->name));
            } else {
                newSemanticName[0] = '\0';
            }
            lPrintUniformVariableDescription(out, newSymbolName, lSymb->type, fBind,
                    bindOffset + (lSymb->details.var.addr >> 2), newSemanticName, paramNo);
            lSymb = lSymb->next;
        }
        break;
    default:
        fprintf(out, "%s *** unknown category 0x%08x ***\n", Cg->theHAL->comment, category);
        break;
    }
} // lPrintUniformVariableDescription

static void lPrintVaryingVariableDescription(FILE *out, const char *symbolName,
                                             Symbol *fSymb, const char *scopeName, int paramNo)
{
    char str1[100], str2[100], newSymbolName[1024];
    int category, len, len2, mname, semantics;
    const char *bindingRegName;
    Binding *lBind;
    Symbol *lSymb;
    Type *lType;

    lType = fSymb->type;
    if (GetCategory(lType) == TYPE_CATEGORY_FUNCTION) {
        lType = lType->fun.rettype;
        semantics = 0;
        lBind = NULL;
    } else {
        semantics = fSymb->details.var.semantics;
        lBind = fSymb->details.var.bind;
    }
    category = GetCategory(lType);
    switch (category) {
    case TYPE_CATEGORY_SCALAR:
    case TYPE_CATEGORY_ARRAY:
        if (IsScalar(lType) || IsVector(lType, &len) || IsMatrix(lType, &len, &len2)) {
            FormatTypeStringRT(str1, sizeof(str1), str2, sizeof(str1), lType, 1);
            if (lBind) {
                if (lBind->none.properties & BIND_HIDDEN)
                    return;
                mname = semantics ? semantics : lBind->conn.rname;
                switch (lBind->none.kind) {
                case BK_CONNECTOR:
                    bindingRegName = GetAtomString(atable, lBind->conn.rname);
                    break;
                default:
                    bindingRegName = "?????";
                    break;
                }
            } else {
                // No binding
                bindingRegName = "";
            }
            fprintf(out, "%svar %s %s%s", Cg->theHAL->comment, str1, symbolName, str2);
            fprintf(out, " : %s.%s", scopeName, GetAtomString(atable, mname));
            fprintf(out, " : %s", bindingRegName);
            fprintf(out, " : %d", paramNo);
            fprintf(out, " : %d", 1);
            fprintf(out, "\n");
        } else {
            // Can't have a varying array!  No way to specify bindings!
            fprintf(out, "%s *** Can't have varying nonpacked array ***\n", Cg->theHAL->comment);
        }
        break;
    case TYPE_CATEGORY_STRUCT:
        lSymb = lType->str.members->symbols;
        while (lSymb) {
            if (*symbolName != '\0') {
                sprintf(newSymbolName, "%s.%s", symbolName, GetAtomString(atable, lSymb->name));
            } else {
                strcpy(newSymbolName, GetAtomString(atable, lSymb->name));
            }
            lPrintVaryingVariableDescription(out, newSymbolName, lSymb, scopeName, paramNo);
            lSymb = lSymb->next;
        }
        break;
    default:
        fprintf(out, "%s *** unknown category 0x%08x ***\n", Cg->theHAL->comment, category);
        break;
    }
} // lPrintVaryingVariableDescription

/*
 * lOutputParameterTypes()
 *
 */

static void lOutputParameterTypes(FILE *out, Symbol *program)
{
    Symbol *lSymb;
    SymbolList *lGlobals;
    UniformSemantic *lUniform;
    const char *semanticName, *symbolName, *varyingScopeName;
    int paramNo;
    Type *lType, *retType;

    // 1. Profile:

    fprintf(out, "%sprofile %s\n", Cg->theHAL->comment, GetAtomString(atable, Cg->theHAL->profileName));

    // 2. Program:

    fprintf(out, "%sprogram %s\n", Cg->theHAL->comment, GetAtomString(atable, program->name));

    // 3. Uniform semantic table:

    lUniform = Cg->theHAL->uniforms;
    while (lUniform) {
        fprintf(out, "%ssemantic ", Cg->theHAL->comment);
        if (lUniform->gname)
            fprintf(out, "%s.", GetAtomString(atable, lUniform->gname));
        fprintf(out, "%s", GetAtomString(atable, lUniform->vname));
        if (lUniform->semantic)
            fprintf(out, " : %s", GetAtomString(atable, lUniform->semantic));
        fprintf(out, "\n");
        lUniform = lUniform->next;
    }

    // 4. Global uniform variables:

    lGlobals = Cg->theHAL->uniformGlobal;
    while (lGlobals) {
        lSymb = lGlobals->symb;
        if (lSymb) {
            if (lSymb->details.var.semantics) {
                semanticName = GetAtomString(atable, lSymb->details.var.semantics);
            } else {
                semanticName = "";
            }
            symbolName = GetAtomString(atable, lSymb->name);
            lPrintUniformVariableDescription(out, symbolName, lSymb->type,
                            lSymb->details.var.bind, 0, semanticName, -1);
        }
        lGlobals = lGlobals->next;
    }

    // 5. Parameters:

    paramNo = 0;
    lSymb = program->details.fun.params;
    while (lSymb) {
        symbolName = GetAtomString(atable, lSymb->name);
        if (GetDomain(lSymb->type) & TYPE_DOMAIN_UNIFORM) {
            if (lSymb->details.var.semantics) {
                semanticName = GetAtomString(atable, lSymb->details.var.semantics);
            } else {
                semanticName = "";
            }
            lPrintUniformVariableDescription(out, symbolName, lSymb->type,
                            lSymb->details.var.bind, 0, semanticName, paramNo);
        } else {
            if (GetQualifiers(lSymb->type) & TYPE_QUALIFIER_OUT) {
                varyingScopeName = "$vout";
            } else {
                varyingScopeName = "$vin";
            }
            lPrintVaryingVariableDescription(out, symbolName, lSymb, varyingScopeName, paramNo);
        }
        paramNo++;
        lSymb = lSymb->next;
    }

    // 6. Function return value:

    lType = program->type;
    retType = lType->fun.rettype;
    if (GetCategory(retType) == TYPE_CATEGORY_STRUCT) {
        lPrintVaryingVariableDescription(out, "", program, "$vout", -1);
    }
} // lOutputParameterTypes

/*
 * lOutputConstantBindings() - Output the constant bindings in the HAL.
 *
 */

static void lOutputConstantBindings(FILE *out, slHAL *fHAL)
{
    BindingList *lBindList;
    Binding *lBind;
    int ii;

    lBindList = fHAL->constantBindings;
    while (lBindList) {
        lBind = lBindList->binding;
        if (lBind->none.kind == BK_CONSTANT) {
            fprintf(out, "%sconst %s[%d] =", fHAL->comment,
                GetAtomString(atable, lBind->constdef.rname), lBind->constdef.regno);
            for (ii = 0; ii < lBind->constdef.size; ii++)
                fprintf(out, " %.7g", lBind->constdef.val[ii]);
            fprintf(out, "\n");
        }
        lBindList = lBindList->next;
    }
} // lOutputConstantBindings

/*
 * lOutputDefaultBindings() - Output the constant bindings in the HAL.
 *
 */

static void lOutputDefaultBindings(FILE *out, slHAL *fHAL)
{
    BindingList *lBindList;
    Binding *lBind;
    int ii;

    lBindList = fHAL->defaultBindings;
    while (lBindList) {
        lBind = lBindList->binding;
        if (lBind->none.kind == BK_DEFAULT) {
            fprintf(out, "%sdefault ", fHAL->comment);
            if (lBind->constdef.gname)
                fprintf(out, " %s.", GetAtomString(atable, lBind->constdef.gname));
            fprintf(out, "%s =", GetAtomString(atable, lBind->constdef.lname));
            for (ii = 0; ii < lBind->constdef.size; ii++)
                fprintf(out, " %.7g", lBind->constdef.val[ii]);
            fprintf(out, "\n");
        }
        lBindList = lBindList->next;
    }
} // lOutputDefaultBindings

void OutputBindings(FILE *out, slHAL *fHAL, Symbol *program)
{
    lOutputVendorVersion(out, fHAL);
    lOutputParameterTypes(out, program);
    lOutputConstantBindings(out, fHAL);
    lOutputDefaultBindings(out, fHAL);
} // OutputBindings

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////// Main Compile Control ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * CompileProgram() - The final call from the parser.  If there weren't any errors, perform
 *         optimizations, register allocation, code generation, etc.
 *
 * Returns: Number of errors.
 *
 */

int CompileProgram(CgStruct *Cg, SourceLoc *loc, Scope *fScope)
{
    slHAL *theHAL = Cg->theHAL;
    Symbol *program;
    Scope *lScope;
    stmt *lStmt;

    if (GetErrorCount() == 0) {
        if (fScope->programs) {
            theHAL->globalScope = fScope;
            program = fScope->programs->symb;
            BuildSemanticStructs(loc, fScope, program);
            CheckFunctionDefinitions(loc, fScope, program);
            if (GetErrorCount() != 0)
                goto done;

            // Convert to Basic Blocks Format goes here...

            // Inline appropriate function calls...

            lScope = program->details.fun.locals;
            lStmt = program->details.fun.statements;
            lStmt = ConcatStmts(fScope->initStmts, lStmt);
            if (GetErrorCount() == 0) {
                lStmt = ExpandInlineFunctionCalls(lScope, lStmt, NULL);
                PostApplyToExpressions(CheckForHiddenVaryingReferences, lStmt, NULL, 0);
                if (Cg->options.DumpParseTree || Cg->options.DumpNodeTree) {
                    program->details.fun.statements = lStmt;
                    printf("=======================================================================\n");
                    printf("After inlining functions:\n");
                    printf("=======================================================================\n");
                    PrintSymbolTree(program->details.fun.locals->symbols);
                    if (Cg->options.DumpParseTree)
                        PrintFunction(program);
                    if (Cg->options.DumpNodeTree)
                        BPrintFunction(program);
                    printf("=======================================================================\n");
                }

                CheckConnectorUsageMain(program, lStmt);
                lStmt = ConvertDebugCalls(loc, lScope, lStmt, Cg->options.DebugMode);
                PostApplyToExpressions(ExpandIncDecExpr, lStmt, NULL, 0);
                PostApplyToExpressions(ExpandCompoundAssignmentExpr, lStmt, NULL, 0);
                lStmt = PostApplyToStatements(FlattenCommasStmt, lStmt, NULL, 0);
                lStmt = PostApplyToStatements(RemoveEmptyStatementsStmt, lStmt, NULL, 0);
                lStmt = PostApplyToStatements(FlattenChainedAssignmentsStmt, lStmt, NULL, 0);
                PostApplyToExpressions(ConvertNamedConstantsExpr, lStmt, NULL, 0);
                if (theHAL->GetCapsBit(CAPS_DECONSTRUCT_MATRICES))
                    lStmt = DeconstructMatrices(lScope, lStmt);
                lStmt = FlattenStructAssignments(lScope, lStmt);
                if (!theHAL->GetCapsBit(CAPS_DONT_FLATTEN_IF_STATEMENTS))
                    lStmt = FlattenIfStatements(lScope, lStmt);

                // Optimizations:

                PostApplyToExpressions(ConstantFoldNode, lStmt, NULL, 0);

                // Lots more optimization stuff goes here...

                if (Cg->options.DumpFinalTree) {
                    program->details.fun.statements = lStmt;
                    printf("=======================================================================\n");
                    printf("Final program:\n");
                    printf("=======================================================================\n");
                    PrintSymbolTree(program->details.fun.locals->symbols);
                    PrintFunction(program);
                    if (Cg->options.DumpNodeTree)
                        BPrintFunction(program);
                    printf("=======================================================================\n");
                }
            }

            program->details.fun.statements = lStmt;
            if (!theHAL->GetCapsBit(CAPS_LATE_BINDINGS))
                OutputBindings(Cg->options.outfd, theHAL, program);
            if (GetErrorCount() == 0) {
                if (!Cg->options.NoCodeGen)
                    theHAL->GenerateCode(loc, fScope, program);
            }

            if (Cg->options.ErrorMode)
                CheckAllErrorsGenerated();

        } else {
            SemanticError(loc, ERROR___NO_PROGRAM);
        }
    }
done:
    return GetErrorCount();
} // CompileProgram

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of compile.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
