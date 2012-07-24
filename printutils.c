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
// printutils.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////// Debug Printing Functions: //////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

#undef PICK
#define PICK(a, b, c, d, e) b

const char *opcode_name[] = {
    OPCODE_TABLE
};

#undef PICK
#define PICK(a, b, c, d, e) e

const subopkind subop_table[] = {
    OPCODE_TABLE
};

#undef PICK

/*
 * lIndent()
 *
 */

static void lIndent(int level)
{
    int ii;

    for (ii = 0; ii < level; ii++)
        printf("    ");
} // lIndent

/*
 * lBPrintExpression() - Print a low level representation of an expression tree.
 *
 */

static void lBPrintExpression(expr *fexpr, int level)
{
    char s[32];
    int subop, mask, ii, nn, bval;
    int OK = 1, HasString = 0;

    lIndent(level);
    if (fexpr) {
        switch (fexpr->common.kind) {
        case DECL_N:
            printf("DECLARATION");
            OK = 0;
            break;
        case SYMB_N:
            printf("S ");
            break;
        case CONST_N:
            printf("C ");
            break;
        case UNARY_N:
            printf("U ");
            break;
        case BINARY_N:
            printf("B ");
            break;
        case TRINARY_N:
            printf("T ");
            break;
        default:
            printf("<kind=%02x>", fexpr->common.kind);
            OK = 0;
            break;
        }
        if (OK) {
            printf("%c%c", fexpr->sym.IsLValue ? 'L' : ' ',
                    fexpr->common.HasSideEffects ? '+' : ' ');
            subop = fexpr->co.subop;
            switch (subop_table[fexpr->co.op]) {
                case SUB_NONE:
                    printf("- - - ");
                    mask = ~0;
                    break;
                case SUB_S:
                    printf("- - %1x ", SUBOP_GET_T(subop));
                    mask = 0x0000000f;
                    break;
                case SUB_V:
                case SUB_VS:
                case SUB_SV:
                    printf("- %1x %1x ", SUBOP_GET_S(subop), SUBOP_GET_T(subop));
                    mask = 0x000000ff;
                    break;
                case SUB_M:
                case SUB_VM:
                case SUB_MV:
                    printf("%1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_S1(subop),
                            SUBOP_GET_T1(subop));
                    mask = 0x0000f0ff;
                    break;
                case SUB_Z:
                    printf("%1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_S1(subop),
                            SUBOP_GET_T1(subop));
                    mask = SUBOP_GET_MASK(subop);
                    nn = SUBOP_GET_S2(subop);
                    if (nn == 0)
                        nn = 1; // var.x is scalar, not array[1]
                    s[nn] = '\0';
                    for (ii = 0; ii < nn; ii++)
                        s[ii] = "xyzw"[(mask >> ii*2) & 3];
                    mask = 0x00fff0ff;
                    HasString = 1;
                    break;
                case SUB_ZM:
                    printf("%1x %1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_T2(subop),
                            SUBOP_GET_S1(subop), SUBOP_GET_T1(subop));
                    mask = SUBOP_GET_MASK16(subop);
                    nn = SUBOP_GET_T2(subop);
                    if (nn == 0)
                        nn = 1; // var.x is scalar, not array[1]
                    s[nn*3] = '\0';
                    for (ii = 0; ii < nn; ii++) {
                        s[ii*3] = '_';
                        s[ii*3 + 1] = '0' + ((mask >> (ii*4 + 2)) & 3);
                        s[ii*3 + 2] = '0' + ((mask >> ii*4) & 3);
                    }
                    mask = 0xffffffff;
                    HasString = 1;
                    break;
                case SUB_CS:
                    printf("%1x - %1x ", SUBOP_GET_T2(subop), SUBOP_GET_T1(subop));
                    mask = 0x00000f0f;
                    break;
                case SUB_CV:
                    printf("%1x %1x %1x ", SUBOP_GET_T2(subop), SUBOP_GET_S1(subop),
                            SUBOP_GET_T1(subop));
                    mask = 0x00000fff;
                    break;
                case SUB_CM:
                    printf("%1x %1x %1x %1x ", SUBOP_GET_S2(subop), SUBOP_GET_T2(subop),
                            SUBOP_GET_S1(subop), SUBOP_GET_T1(subop));
                    mask = 0x0000ffff;
                    break;
                case SUB_KV:
                    printf("- %1x %1x ", SUBOP_GET_S(subop), SUBOP_GET_T(subop));
                    mask = SUBOP_GET_MASK(subop) & 0xf;
                    for (ii = 0; ii < 4; ii++)
                        printf("%c", (mask >> ii) & 1 ? "xyzw"[ii] : '-');
                    printf(" ");
                    mask = 0x000f00ff;
                    break;
                default:
                    mask = 0;
                    break;
            }
            if (subop & ~mask)
                printf("<<non-zero:%08x>> ", subop & ~mask);
            printf("%-6s ", opcode_name[fexpr->co.op]);
            switch (fexpr->common.kind) {
            case SYMB_N:
                printf("\"%s\"", GetAtomString(atable, fexpr->sym.symbol->name));
                break;
            case CONST_N:
                switch (fexpr->co.op) {
                case ICONST_OP:
                    printf("%d", fexpr->co.val[0].i);
                    break;
                case ICONST_V_OP:
                    nn = SUBOP_GET_S(subop);
                    printf("{ ");
                    for (ii = 0; ii < nn; ii++) {
                        if (ii > 0)
                            printf(", ");
                        printf("%d", fexpr->co.val[ii].i);
                    }
                    printf(" }");
                    break;
                case BCONST_OP:
                    bval = fexpr->co.val[0].i;
                    if (bval == 0) {
                        printf("false");
                    } else if (bval == 1) {
                        printf("true");
                    } else {
                        printf("<<bad-bool-%08x>>", fexpr->co.val[0].i);
                    }
                    break;
                case BCONST_V_OP:
                    nn = SUBOP_GET_S(subop);
                    printf("{ ");
                    for (ii = 0; ii < nn; ii++) {
                        if (ii > 0)
                            printf(", ");
                        bval = fexpr->co.val[ii].i;
                        if (bval == 0) {
                            printf("false");
                        } else if (bval == 1) {
                            printf("true");
                        } else {
                            printf("<<bad-bool-%08x>>", fexpr->co.val[ii].i);
                        }
                    }
                    printf(" }");
                    break;
                case FCONST_OP:
                case HCONST_OP:
                case XCONST_OP:
                    printf("%1.6g", fexpr->co.val[0].f);
                    break;
                case FCONST_V_OP:
                case HCONST_V_OP:
                case XCONST_V_OP:
                    nn = SUBOP_GET_S(subop);
                    printf("{ ");
                    for (ii = 0; ii < nn; ii++) {
                        if (ii > 0)
                            printf(", ");
                        printf("%1.6g", fexpr->co.val[ii].f);
                    }
                    printf(" }");
                    break;
                default:
                    printf("UNKNOWN-CONSTANT");
                    break;
                }
                break;
            case UNARY_N:
                break;
            case BINARY_N:
                break;
            case TRINARY_N:
                break;
            }
            if (HasString)
                printf(" %s", s);
            printf(" ");
            PrintType(fexpr->common.type, 1);
            printf("\n");
            switch (fexpr->common.kind) {
            case SYMB_N:
                break;
            case CONST_N:
                break;
            case UNARY_N:
                lBPrintExpression(fexpr->un.arg, level + 1);
                break;
            case BINARY_N:
                lBPrintExpression(fexpr->bin.left, level + 1);
                lBPrintExpression(fexpr->bin.right, level + 1);
                break;
            case TRINARY_N:
                lBPrintExpression(fexpr->tri.arg1, level + 1);
                lBPrintExpression(fexpr->tri.arg2, level + 1);
                lBPrintExpression(fexpr->tri.arg3, level + 1);
                break;
            }
        } else {
            printf("\n");
        }
    } else {
        printf("NULL\n");
    }
} // lBPrintExpression

void BPrintExpression(expr *fexpr)
{
    lBPrintExpression(fexpr, 0);
}

static void lBPrintStmt(stmt *fstmt, int level);

/*
 * lBPrintStmtList()
 *
 */

void lBPrintStmtList(stmt *fstmt, int level)
{
    while (fstmt) {
        lBPrintStmt(fstmt, level);
        fstmt = fstmt->commonst.next;
    }
} // lBPrintStmtList

void BPrintStmtList(stmt *fstmt)
{
    lBPrintStmtList(fstmt, 0);
}

/*
 * lBPrintStmt()
 *
 */

static void lBPrintStmt(stmt *fstmt, int level)
{
    stmt *lstmt;

    switch (fstmt->exprst.kind) {
    case EXPR_STMT:
        if (fstmt->exprst.exp) {
            lBPrintExpression(fstmt->exprst.exp, level);
        } else {
            printf("/* empty statement */\n");
        }
        break;
    case IF_STMT:
        lIndent(level);
        printf("if\n");
        lBPrintExpression(fstmt->ifst.cond, level + 1);
        lIndent(level);
        printf("then\n");
        lBPrintStmtList(fstmt->ifst.thenstmt, level + 1);
        if (fstmt->ifst.elsestmt) {
            lIndent(level);
            printf("else\n");
            lBPrintStmtList(fstmt->ifst.elsestmt, level + 1);
        }
        break;
    case WHILE_STMT:
        lIndent(level);
        printf("while\n");
        lBPrintExpression(fstmt->whilest.cond, level + 1);
        lBPrintStmtList(fstmt->whilest.body, level + 1);
        break;
    case DO_STMT:
        lIndent(level);
        printf("do\n");
        lBPrintStmtList(fstmt->whilest.body, level + 1);
        lIndent(level);
        printf("while\n");
        lBPrintExpression(fstmt->whilest.cond, level + 1);
        break;
    case FOR_STMT:
        lIndent(level);
        printf("for\n");
        lstmt = fstmt->forst.init;
        if (lstmt) {
            lBPrintStmtList(fstmt->forst.init, level + 1);
        }
        printf("for-cond\n");
        if (fstmt->forst.cond) {
            lBPrintExpression(fstmt->forst.cond, level + 1);
        }
        printf("for-step\n");
        lstmt = fstmt->forst.step;
        if (lstmt) {
            lBPrintStmtList(fstmt->forst.step, level + 1);
        }
        printf("for-body\n");
        lBPrintStmtList(fstmt->forst.body, level + 1);
        break;
    case BLOCK_STMT:
        if (level > 1)
            lIndent(level - 1);
        printf("{\n");
        lBPrintStmtList(fstmt->blockst.body, level);
        if (level > 1)
            lIndent(level - 1);
        printf("}\n");
        break;
    case RETURN_STMT:
        lIndent(level);
        printf("return\n");
        if (fstmt->returnst.exp) {
            lBPrintExpression(fstmt->returnst.exp, level + 1);
        }
        break;
    case DISCARD_STMT:
        lIndent(level);
        printf("discard\n");
        if (fstmt->discardst.cond)
            lBPrintExpression(fstmt->discardst.cond, level + 1);
        break;
    case COMMENT_STMT:
        lIndent(level);
        printf("// %s\n", GetAtomString(atable, fstmt->commentst.str));
        break;
    default:
        lIndent(level);
        printf("<!BadStmt-0x%2x>\n", fstmt->exprst.kind);
    }
} // lBPrintStmt

void BPrintStmt(stmt *fstmt)
{
    lBPrintStmt(fstmt, 0);
}

/*
 * FormatTypeString() - Build a printable string of a type.
 *
 * Arrays are shown as: "packed float[4]" instead of "float4".
 *
 */

void FormatTypeString(char *name, int size, char *name2, int size2, Type *fType)
{
    int qualifiers, category, base, cid;
    char tname[32];

    strcpy(name2, "");
    if (fType) {
        strcpy(name, "");

        base = GetBase(fType);

        qualifiers = GetQualifiers(fType);
        if (qualifiers & TYPE_QUALIFIER_CONST)
            strcat(name, "const ");
        if ((qualifiers & TYPE_QUALIFIER_INOUT) == TYPE_QUALIFIER_INOUT) {
            strcat(name, "inout ");
        } else {
            if (qualifiers & TYPE_QUALIFIER_IN)
                strcat(name, "in ");
            if (qualifiers & TYPE_QUALIFIER_OUT)
                strcat(name, "out ");
        }

        category = GetCategory(fType);
        switch (category) {
        case TYPE_CATEGORY_NONE:
            strcat(name, "<<category=NONE>>");
            break;
        case TYPE_CATEGORY_SCALAR:
            strcat(name, GetBaseTypeNameString(base));
            break;
        case TYPE_CATEGORY_ARRAY:
            FormatTypeString(name, size, name2, size2, fType->arr.eltype);
            sprintf(tname, "[%d]", fType->arr.numels);
            strcat(name2, tname);
            break;
        case TYPE_CATEGORY_FUNCTION:
            strcat(name, "FUNCTION");
            break;
        case TYPE_CATEGORY_STRUCT:
            strcat(name, "struct ");
            strcat(name, GetAtomString(atable, fType->str.tag));
            break;
        case TYPE_CATEGORY_CONNECTOR:
            cid = Cg->theHAL->GetConnectorAtom(fType->str.variety);
            strcat(name, GetAtomString(atable, cid));
            strcat(name, " connector ");
            strcat(name, GetAtomString(atable, fType->str.tag));
            break;
        default:
            strcat(name, "<<bad-category>>");
            break;
        }
    } else {
        strcpy(name, "<<NULL>>");
    }
} // FormatTypeString

/*
 * FormatTypeStringRT() - Build a printable string of a type for export to the run-time.
 *
 * Arrays are shown as predefined typedefs: "float4" instead of "packed float[4]"
 *
 */

void FormatTypeStringRT(char *name, int size, char *name2, int size2, Type *fType, int Unqualified)
{
    int qualifiers, category, base, cid;
    char tname[32];
    int len, len2;

    strcpy(name2, "");
    if (fType) {
        strcpy(name, "");

        base = GetBase(fType);

        if (!Unqualified) {
            qualifiers = GetQualifiers(fType);
            if (qualifiers & TYPE_QUALIFIER_CONST)
                strcat(name, "const ");
            if ((qualifiers & TYPE_QUALIFIER_INOUT) == TYPE_QUALIFIER_INOUT) {
                strcat(name, "inout ");
            } else {
                if (qualifiers & TYPE_QUALIFIER_IN)
                    strcat(name, "in ");
                if (qualifiers & TYPE_QUALIFIER_OUT)
                    strcat(name, "out ");
            }
        }

        category = GetCategory(fType);
        switch (category) {
        case TYPE_CATEGORY_NONE:
            strcat(name, "<<category=NONE>>");
            break;
        case TYPE_CATEGORY_SCALAR:
            strcat(name, GetBaseTypeNameString(base));
            break;
        case TYPE_CATEGORY_ARRAY:
            if (IsMatrix(fType, &len, &len2)) {
                strcat(name, GetBaseTypeNameString(base));
                sprintf(tname, "%dx%d", len2, len);
                strcat(name, tname);
            } else if (IsVector(fType, &len)) {
                strcat(name, GetBaseTypeNameString(base));
                tname[0] = '0' + len;
                tname[1] = '\0';
                strcat(name, tname);
            } else {
                FormatTypeStringRT(name, size, name2, size2, fType->arr.eltype, Unqualified);
                sprintf(tname, "[%d]", fType->arr.numels);
                strcat(name2, tname);
            }
            break;
        case TYPE_CATEGORY_FUNCTION:
            strcat(name, "FUNCTION");
            break;
        case TYPE_CATEGORY_STRUCT:
            strcat(name, "struct ");
            strcat(name, GetAtomString(atable, fType->str.tag));
            break;
        case TYPE_CATEGORY_CONNECTOR:
            cid = Cg->theHAL->GetConnectorAtom(fType->str.variety);
            strcat(name, GetAtomString(atable, cid));
            strcat(name, " connector ");
            strcat(name, GetAtomString(atable, fType->str.tag));
            break;
        default:
            strcat(name, "<<bad-category>>");
            break;
        }
    } else {
        strcpy(name, "<<NULL>>");
    }
} // FormatTypeStringRT

/*
 * PrintType()
 *
 */

void PrintType(Type *fType, int level)
{
    int base, category, qualifiers, domain, cid;
    TypeList *lTypeList;

    if (fType) {
        qualifiers = GetQualifiers(fType);
        if (qualifiers & TYPE_QUALIFIER_CONST)
            printf("const ");
        if (qualifiers & TYPE_QUALIFIER_IN)
            printf("in ");
        if (qualifiers & TYPE_QUALIFIER_OUT)
            printf("out ");

        domain = GetDomain(fType);
        switch (domain) {
        case TYPE_DOMAIN_UNKNOWN:
            break;
        case TYPE_DOMAIN_UNIFORM:
            printf("uniform ");
            break;
        case TYPE_DOMAIN_VARYING:
            printf("varying ");
            break;
        default:
            printf("<<domain=%02x>>", domain >> TYPE_DOMAIN_SHIFT);
            break;
        }

        category = GetCategory(fType);
        switch (category) {
        case TYPE_CATEGORY_NONE:
            printf("<<category=NONE>>");
            break;
        case TYPE_CATEGORY_SCALAR:
            base = GetBase(fType);
            printf("%s", GetBaseTypeNameString(base));
            break;
        case TYPE_CATEGORY_ARRAY:
            PrintType(fType->arr.eltype, level);
            printf("[%d]", fType->arr.numels);
            break;
        case TYPE_CATEGORY_FUNCTION:
            printf("(");
            lTypeList = fType->fun.paramtypes;
            while (lTypeList) {
                PrintType(lTypeList->type, level);
                lTypeList = lTypeList->next;
                if (lTypeList)
                    printf(", ");
            }
            printf(")");
            break;
        case TYPE_CATEGORY_STRUCT:
            if (fType->str.tag) {
                printf("struct %s", GetAtomString(atable, fType->str.tag));
            } else {
                printf("struct");
            }
            break;
        case TYPE_CATEGORY_CONNECTOR:
            if (fType->str.tag) {
                cid = Cg->theHAL->GetConnectorAtom(fType->str.variety);
                printf("%s connector %s",
                       GetAtomString(atable, cid),
                       GetAtomString(atable, fType->str.tag));
            } else {
                printf("connector");
            }
            break;
        default:
            printf("<<category=%02x>>", category >> TYPE_CATEGORY_SHIFT);
            break;
        }
        //printf(" ");
    } else {
        printf("<<NULL-TYPE>>");
    }
} // PrintType

/*
 * PrintSymbolTree()
 *
 */

void PrintSymbolTree(Symbol *fSymb)
{
    Symbol *lSymb;
    int DoType;

    if (fSymb) {
        DoType = 1;
        PrintSymbolTree(fSymb->left);
        switch (fSymb->kind) {
        case TYPEDEF_S:
            printf("TYP: %s : %d:%d", GetAtomString(atable, fSymb->name),
                   Cg->theHAL->GetSizeof(fSymb->type), Cg->theHAL->GetAlignment(fSymb->type));
            break;
        case VARIABLE_S:
            if (fSymb->properties & SYMB_IS_PARAMETER) {
                printf("PAR: %s ", GetAtomString(atable, fSymb->name));
            } else {
                printf("VAR: %s ", GetAtomString(atable, fSymb->name));
            }
            break;
        case CONSTANT_S:
            printf("CON: %s ", GetAtomString(atable, fSymb->name));
            break;
        case FUNCTION_S:
            lSymb = fSymb;
            while (lSymb) {
                if (lSymb == fSymb) {
                    printf("FUN");
                } else {
                    printf("   ");
                }
                printf(": %s ", GetAtomString(atable, fSymb->name));
                PrintType(lSymb->type, 0);
                if (!lSymb->properties & SYMB_IS_DEFINED)
                    printf(" UNDEFINED");
                if (lSymb->properties & SYMB_IS_BUILTIN)
                    printf(" BUILTIN");
                if (lSymb->properties & SYMB_IS_INLINE_FUNCTION)
                    printf(" INLINE");
                printf("\n");
                lSymb = lSymb->details.fun.overload;
            }
            DoType = 0;
            break;
        default:
            printf("???%04x???: %s ", fSymb->kind, GetAtomString(atable, fSymb->name));
            break;
        }
        if (DoType) {
            PrintType(fSymb->type, 0);
            printf("\n");
        }
        PrintSymbolTree(fSymb->right);
    }
} // PrintSymbolTree

/*
 * PrintScopeDeclarations()
 *
 */

void PrintScopeDeclarations(void)
{
    printf("*** Scope %d definitions: ***\n", CurrentScope->level);
    PrintSymbolTree(CurrentScope->symbols);
    printf("*** End of Scope %d ***\n\n", CurrentScope->level);
} // PrintScopeDeclarations

/*
 * lPrintExpr()
 *
 */

void lPrintExpr(expr *fexpr)
{
    int ii, mask, len;
    unsigned int uval;
    char s[16], tag;

    switch (fexpr->common.kind) {
    case SYMB_N:
        printf("%s", GetAtomString(atable, fexpr->sym.symbol->name));
        break;
    case CONST_N:
        switch (fexpr->co.op) {
        case ICONST_OP:
            printf("%d", fexpr->co.val[0].i);
            break;
        case ICONST_V_OP:
            printf("{ %d", fexpr->co.val[0].i);
            len = SUBOP_GET_S(fexpr->co.subop);
            for (ii = 1; ii < len; ii++)
                printf(", %d", fexpr->co.val[ii].i);
            printf(" }");
            break;
        case BCONST_OP:
            if (fexpr->co.val[0].i == 0) {
                printf("false");
            } else if (fexpr->co.val[0].i == 1) {
                printf("true");
            } else {
                printf("<<BBCONST=%d>>", fexpr->co.val[0].i);
            }
            break;
        case BCONST_V_OP:
            printf("{ ");
            len = SUBOP_GET_S(fexpr->co.subop);
            for (ii = 0; ii < len; ii++)
                if (ii) printf(", ");
                if (fexpr->co.val[ii].i == 0) {
                    printf("false");
                } else if (fexpr->co.val[ii].i == 1) {
                    printf("true");
                } else {
                    printf("<<BBCONST=%d>>", fexpr->co.val[ii].i);
                }
            printf(" }");
            break;
        case FCONST_OP:
            printf("%.6gf", fexpr->co.val[0].f);
            break;
        case HCONST_OP:
            printf("%.6gh", fexpr->co.val[0].f);
            break;
        case XCONST_OP:
            printf("%.6gx", fexpr->co.val[0].f);
            break;
        case FCONST_V_OP:
            tag = 'f';
            goto floatvec;
        case HCONST_V_OP:
            tag = 'h';
            goto floatvec;
        case XCONST_V_OP:
            tag = 'x';
        floatvec:
            printf("{ %.6g%c", fexpr->co.val[0].f, tag);
            len = SUBOP_GET_S(fexpr->co.subop);
            for (ii = 1; ii < len; ii++)
                printf(", %.6g%c", fexpr->co.val[ii].f, tag);
            printf(" }");
            break;
        }
        break;
    case UNARY_N:
        switch (fexpr->un.op) {
        case CAST_CS_OP:
            printf("(%s) ", GetBaseTypeNameString(SUBOP_GET_T2(fexpr->un.subop)));
            break;
        case CAST_CV_OP:
            printf("(%s [%d]) ",
                   GetBaseTypeNameString(SUBOP_GET_T2(fexpr->un.subop)),
                   SUBOP_GET_S1(fexpr->un.subop));
            break;
        case CAST_CM_OP:
            printf("(%s [%d][%d]) ",
                   GetBaseTypeNameString(SUBOP_GET_T2(fexpr->un.subop)),
                   SUBOP_GET_S2(fexpr->un.subop), SUBOP_GET_S1(fexpr->un.subop));
            break;
        case VECTOR_V_OP:
            printf("{ ");
            break;
        case NEG_OP:
        case NEG_V_OP:
            printf("-");
            break;
        case POS_OP:
        case POS_V_OP:
            printf("+");
            break;
        case NOT_OP:
        case NOT_V_OP:
            printf("~");
            break;
        case BNOT_OP:
        case BNOT_V_OP:
            printf("!");
            break;
        case SWIZZLE_Z_OP:
        case SWIZMAT_Z_OP:
            break;
        case PREDEC_OP:
            printf("--");
            break;
        case PREINC_OP:
            printf("++");
            break;
        }
        lPrintExpr(fexpr->un.arg);
        switch (fexpr->un.op) {
        case SWIZZLE_Z_OP:
            mask = SUBOP_GET_MASK(fexpr->un.subop);
            ii = SUBOP_GET_S2(fexpr->un.subop);
            if (ii == 0)
                ii = 1; // var.x is scalar, not array[1]
            s[ii] = '\0';
            while (ii > 0) {
                ii--;
                s[ii] = "xyzw"[(mask >> ii*2) & 3];
            }
            printf(".%s", s);
            break;
        case SWIZMAT_Z_OP:
            mask = SUBOP_GET_MASK16(fexpr->un.subop);
            ii = SUBOP_GET_T2(fexpr->un.subop);
            if (ii == 0)
                ii = 1; // var.x is scalar, not array[1]
            s[ii*3] = '\0';
            while (ii > 0) {
                ii--;
                uval = (mask >> ii*4) & 15;
                s[ii*3] = '_';
                s[ii*3 + 1] = '0' + ((uval >> 2) & 3);
                s[ii*3 + 2] = '0' + (uval & 3);
            }
            printf(".m%s", s);
            break;
        case VECTOR_V_OP:
            printf(" }");
            break;
        case POSTDEC_OP:
            printf("--");
            break;
        case POSTINC_OP:
            printf("++");
            break;
        }
        break;
    case BINARY_N:
        lPrintExpr(fexpr->bin.left);
        switch (fexpr->bin.op) {
        case MEMBER_SELECTOR_OP:
            printf(".");
            break;
        case ARRAY_INDEX_OP:
            printf("[");
            break;
        case FUN_CALL_OP:
        case FUN_BUILTIN_OP:
            printf("(");
            break;
        case FUN_ARG_OP:
        case EXPR_LIST_OP:
            if (fexpr->bin.right)
                printf(", ");
            break;
        case MUL_OP:
        case MUL_SV_OP:
        case MUL_VS_OP:
        case MUL_V_OP:
            printf("*");
            break;
        case DIV_OP:
        case DIV_SV_OP:
        case DIV_VS_OP:
        case DIV_V_OP:
            printf("/");
            break;
        case MOD_OP:
        case MOD_SV_OP:
        case MOD_VS_OP:
        case MOD_V_OP:
            printf("%");
            break;
        case ADD_OP:
        case ADD_SV_OP:
        case ADD_VS_OP:
        case ADD_V_OP:
            printf(" + ");
            break;
        case SUB_OP:
        case SUB_SV_OP:
        case SUB_VS_OP:
        case SUB_V_OP:
            printf(" - ");
            break;
        case SHL_OP:
        case SHL_V_OP:
            printf(" << ");
            break;
        case SHR_OP:
        case SHR_V_OP:
            printf(" >> ");
            break;
        case LT_OP:
        case LT_SV_OP:
        case LT_VS_OP:
        case LT_V_OP:
            printf(" < ");
            break;
        case GT_OP:
        case GT_SV_OP:
        case GT_VS_OP:
        case GT_V_OP:
            printf(" > ");
            break;
        case LE_OP:
        case LE_SV_OP:
        case LE_VS_OP:
        case LE_V_OP:
            printf(" <= ");
            break;
        case GE_OP:
        case GE_SV_OP:
        case GE_VS_OP:
        case GE_V_OP:
            printf(" >= ");
            break;
        case EQ_OP:
        case EQ_SV_OP:
        case EQ_VS_OP:
        case EQ_V_OP:
            printf(" == ");
            break;
        case NE_OP:
        case NE_SV_OP:
        case NE_VS_OP:
        case NE_V_OP:
            printf(" != ");
            break;
        case AND_OP:
        case AND_SV_OP:
        case AND_VS_OP:
        case AND_V_OP:
            printf(" & ");
            break;
        case XOR_OP:
        case XOR_SV_OP:
        case XOR_VS_OP:
        case XOR_V_OP:
            printf(" ^ ");
            break;
        case OR_OP:
        case OR_SV_OP:
        case OR_VS_OP:
        case OR_V_OP:
            printf(" | ");
            break;
        case BAND_OP:
        case BAND_SV_OP:
        case BAND_VS_OP:
        case BAND_V_OP:
            printf(" && ");
            break;
        case BOR_OP:
        case BOR_SV_OP:
        case BOR_VS_OP:
        case BOR_V_OP:
            printf(" || ");
            break;
        case ASSIGN_OP:
        case ASSIGN_V_OP:
        case ASSIGN_GEN_OP:
            printf(" = ");
            break;
        case ASSIGNMINUS_OP:
            printf(" -= ");
            break;
        case ASSIGNMOD_OP:
            printf(" %= ");
            break;
        case ASSIGNPLUS_OP:
            printf(" += ");
            break;
        case ASSIGNSLASH_OP:
            printf(" /= ");
            break;
        case ASSIGNSTAR_OP:
            printf(" *= ");
            break;
        case ASSIGN_MASKED_KV_OP:
            printf("@@");
            mask = SUBOP_GET_MASK(fexpr->bin.subop);
            for (ii = 3; ii >= 0; ii--) {
                if (mask & 1)
                    printf("%c", "wzyx"[ii]);
                mask >>= 1;
            }
            printf(" = ");
            break;
        case COMMA_OP:
            printf(" , ");
            break;
        default:
            printf("<!BINOP=%d>", fexpr->bin.op);
            break;
        }
        if (fexpr->bin.right)
            lPrintExpr(fexpr->bin.right);
        switch (fexpr->bin.op) {
        case ARRAY_INDEX_OP:
            printf("]");
            break;
        case FUN_CALL_OP:
        case FUN_BUILTIN_OP:
            printf(")");
            break;
        default:
            break;
        }
        break;
    case TRINARY_N:
        lPrintExpr(fexpr->tri.arg1);
        switch (fexpr->tri.op) {
        case COND_OP:
        case COND_V_OP:
        case COND_SV_OP:
        case COND_GEN_OP:
            printf(" ? ");
            if (fexpr->tri.arg2)
                lPrintExpr(fexpr->tri.arg2);
            printf(" : ");
            if (fexpr->tri.arg3)
                lPrintExpr(fexpr->tri.arg3);
            break;
        case ASSIGN_COND_OP:
        case ASSIGN_COND_V_OP:
        case ASSIGN_COND_SV_OP:
        case ASSIGN_COND_GEN_OP:
            printf("@@(");
            if (fexpr->tri.arg2)
                lPrintExpr(fexpr->tri.arg2);
            printf(") = ");
            if (fexpr->tri.arg3)
                lPrintExpr(fexpr->tri.arg3);
            break;
        default:
            printf("<!TRIOP=%d>", fexpr->tri.op);
            break;
        }
        break;
    default:
        printf("<!NODEKIND=%d>", fexpr->common.kind);
        break;
    }
} // lPrintExpr

/*
 * PrintExpression()
 *
 */

void PrintExpression(expr *fexpr)
{
    printf("expr: ");
    lPrintExpr(fexpr);
    printf("\n");
} // PrintExpression

static void lPrintStmt(stmt *fstmt, int level, const char *fcomment);

/*
 * lPrintStmtList()
 *
 */

static void lPrintStmtList(stmt *fstmt, int level, const char *fcomment)
{
    while (fstmt) {
        lPrintStmt(fstmt, level, fcomment);
        fstmt = fstmt->commonst.next;
    }
}

/*
 * lPrintStmt()
 *
 */

static void lPrintStmt(stmt *fstmt, int level, const char *fcomment)
{
    stmt *lstmt;

    switch (fstmt->exprst.kind) {
    case EXPR_STMT:
        lIndent(level);
        if (fstmt->exprst.exp) {
            lPrintExpr(fstmt->exprst.exp);
        } else {
            printf("/* empty statement */");
        }
        printf(";\n");
        break;
    case IF_STMT:
        lIndent(level);
        printf("if (");
        lPrintExpr(fstmt->ifst.cond);
        printf(")\n");
        lPrintStmtList(fstmt->ifst.thenstmt, level + 1, NULL);
        if (fstmt->ifst.elsestmt) {
            lIndent(level);
            printf("else\n");
            lPrintStmtList(fstmt->ifst.elsestmt, level + 1, NULL);
        }
        break;
    case WHILE_STMT:
        lIndent(level);
        printf("while (");
        lPrintExpr(fstmt->whilest.cond);
        printf(")\n");
        lPrintStmtList(fstmt->whilest.body, level + 1, NULL);
        break;
    case DO_STMT:
        lIndent(level);
        printf("do\n");
        lPrintStmtList(fstmt->whilest.body, level + 1, NULL);
        lIndent(level);
        printf("while (");
        lPrintExpr(fstmt->whilest.cond);
        printf(");\n");
        break;
    case FOR_STMT:
        lIndent(level);
        printf("for (");
        lstmt = fstmt->forst.init;
        if (lstmt) {
            while (lstmt) {
                if (lstmt->exprst.kind == EXPR_STMT) {
                    lPrintExpr(lstmt->exprst.exp);
                } else {
                    printf("*** BAD STMT KIND ***");
                }
                if (lstmt->exprst.next)
                    printf(", ");
                lstmt = lstmt->exprst.next;
            }
        }
        printf(";");
        if (fstmt->forst.cond) {
            printf(" ");
            lPrintExpr(fstmt->forst.cond);
        }
        printf(";");
        lstmt = fstmt->forst.step;
        if (lstmt) {
            printf(" ");
            while (lstmt) {
                if (lstmt->exprst.kind == EXPR_STMT) {
                    lPrintExpr(lstmt->exprst.exp);
                } else {
                    printf("*** BAD STMT KIND ***");
                }
                if (lstmt->exprst.next)
                    printf(", ");
                lstmt = lstmt->exprst.next;
            }
        }
        printf(")\n");
        lPrintStmtList(fstmt->forst.body, level + 1, NULL);
        break;
    case BLOCK_STMT:
        if (level > 1)
            lIndent(level - 1);
        printf("{\n");
        lPrintStmtList(fstmt->blockst.body, level, NULL);
        if (level > 1)
            lIndent(level - 1);
        if (fcomment) {
            printf("} // %s\n", fcomment);
        } else {
            printf("}\n");
        }
        break;
    case RETURN_STMT:
        lIndent(level);
        printf("return");
        if (fstmt->returnst.exp) {
            printf(" ");
            lPrintExpr(fstmt->returnst.exp);
        }
        printf(";\n");
        break;
    case DISCARD_STMT:
        lIndent(level);
        printf("discard");
        if (fstmt->discardst.cond) {
            printf(" ");
            lPrintExpr(fstmt->discardst.cond);
        }
        printf(";\n");
        break;
    case COMMENT_STMT:
        lIndent(level);
        printf("// %s\n", GetAtomString(atable, fstmt->commentst.str));
        break;
    default:
        lIndent(level);
        printf("<!BadStmt-0x%2x>\n", fstmt->exprst.kind);
    }
} // lPrintStmt

/*
 * PrintStmt()
 *
 */

void PrintStmt(stmt *fstmt)
{
    lPrintStmt(fstmt, 0, NULL);
} // PrintStmt


/*
 * PrintStmtList()
 *
 */

void PrintStmtList(stmt *fstmt)
{
    lPrintStmtList(fstmt, 0, NULL);
} // PrintStmtList

/*
 * PrintFunction()
 *
 */

void PrintFunction(Symbol *symb)
{
    const char *sname, *pname;
    char tname[100], uname[100];
    Symbol *params;

    if (symb) {
        sname = GetAtomString(atable, symb->name);
        if (symb->kind == FUNCTION_S) {
            if (symb->type) {
                FormatTypeString(tname, sizeof tname, uname, sizeof uname, symb->type->fun.rettype);
            } else {
                strcpy(tname, "NULL");
            }
            printf("%s %s%s(",tname, sname, uname);
            params = symb->details.fun.params;
            while (params) {
                pname = GetAtomString(atable, params->name);
                FormatTypeString(tname, sizeof tname, uname, sizeof uname, params->type);
                printf("%s %s%s",tname, pname, uname);
                params = params->next;
                if (params)
                    printf(", ");
            }
            printf(")\n");
            printf("{\n");
            lPrintStmtList(symb->details.fun.statements, 1, "function");
            printf("} // %s\n", sname);
        } else {
            printf("PrintFunction: Symbol \"%s\" not a function\n", sname);
        }
    } else {
        printf("<<NULL-Function-Symbol>>\n");
    }
} // PrintFunction

/*
 * BPrintFunction()
 *
 */

void BPrintFunction(Symbol *symb)
{
    const char *sname;

    if (symb) {
        sname = GetAtomString(atable, symb->name);
        if (symb->kind == FUNCTION_S) {
            printf("{\n");
            lBPrintStmtList(symb->details.fun.statements, 0);
            printf("} // %s\n", sname);
        } else {
            printf("BPrintFunction: Symbol \"%s\" not a function\n", sname);
        }
    } else {
        printf("<<NULL-Function-Symbol>>\n");
    }
} // BPrintFunction

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// End of printutils.c ////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

