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
// support_iter.c
//
// Routines to iterate over the expr/stmt graph
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "slglobals.h"

/*
 * ApplyToNodes() - Walk an expression tree and apply "pre" and "post" to
 * each node.  pre is applied in prefix order, and post in postfix
 *
 */
expr *ApplyToNodes(expr *(*pre)(expr *, void *, int),
                   expr *(*post)(expr *, void *, int),
                   expr *fExpr, void *arg1, int arg2)
{
    if (fExpr) {
        if (pre) fExpr = pre(fExpr, arg1, arg2);
        switch (fExpr->common.kind) {
        case DECL_N:
        case SYMB_N:
        case CONST_N:
            break;
        case UNARY_N:
            fExpr->un.arg = ApplyToNodes(pre, post, fExpr->un.arg, arg1, arg2);
            break;
        case BINARY_N:
            fExpr->bin.left = ApplyToNodes(pre, post, fExpr->bin.left, arg1, arg2);
            fExpr->bin.right = ApplyToNodes(pre, post, fExpr->bin.right, arg1, arg2);
            break;
        case TRINARY_N:
            fExpr->tri.arg1 = ApplyToNodes(pre, post, fExpr->tri.arg1, arg1, arg2);
            fExpr->tri.arg2 = ApplyToNodes(pre, post, fExpr->tri.arg2, arg1, arg2);
            fExpr->tri.arg3 = ApplyToNodes(pre, post, fExpr->tri.arg3, arg1, arg2);
            break;
        default:
            assert(!"bad kind to ApplyToNodes()");
            break;
        }
        if (post) fExpr = post(fExpr, arg1, arg2);
    }
    return fExpr;
} // ApplyToNodes

/*
 * ApplyToExpressions() - Walk a source tree and apply "fun" to each node in each
 *         expression in prefix order.
 */

void ApplyToExpressions(expr *(*pre)(expr *, void *, int),
                        expr *(*post)(expr *, void *, int),
                        stmt *fStmt, void *arg1, int arg2)
{
    while (fStmt) {
        Cg->lastSourceLoc = fStmt->commonst.loc;
        switch (fStmt->exprst.kind) {
        case EXPR_STMT:
            fStmt->exprst.exp = ApplyToNodes(pre, post, fStmt->exprst.exp, arg1, arg2);
            break;
        case IF_STMT:
            fStmt->ifst.cond = ApplyToNodes(pre, post, fStmt->ifst.cond, arg1, arg2);
            ApplyToExpressions(pre, post, fStmt->ifst.thenstmt, arg1, arg2);
            ApplyToExpressions(pre, post, fStmt->ifst.elsestmt, arg1, arg2);
            break;
        case WHILE_STMT:
        case DO_STMT:
            fStmt->whilest.cond = ApplyToNodes(pre, post, fStmt->whilest.cond, arg1, arg2);
            ApplyToExpressions(pre, post, fStmt->whilest.body, arg1, arg2);
            break;
        case FOR_STMT:
            ApplyToExpressions(pre, post, fStmt->forst.init, arg1, arg2);
            fStmt->forst.cond = ApplyToNodes(pre, post, fStmt->forst.cond, arg1, arg2);
            ApplyToExpressions(pre, post, fStmt->forst.body, arg1, arg2);
            ApplyToExpressions(pre, post, fStmt->forst.step, arg1, arg2);
            break;
        case BLOCK_STMT:
            ApplyToExpressions(pre, post, fStmt->blockst.body, arg1, arg2);
            break;
        case RETURN_STMT:
            fStmt->returnst.exp = ApplyToNodes(pre, post, fStmt->returnst.exp, arg1, arg2);
            break;
        case DISCARD_STMT:
            fStmt->discardst.cond = ApplyToNodes(pre, post, fStmt->discardst.cond, arg1, arg2);
            break;
        case COMMENT_STMT:
            break;
        default:
            assert(0);
            break;
        }
        fStmt = fStmt->exprst.next;
    }
} // ApplyToExpressions

/*
 * ApplyToExpressionsLocal() - Apply a function to each node in the expressions contained in
 *         a single statement.
 */

void ApplyToExpressionsLocal(expr *(*pre)(expr *, void *, int),
                             expr *(*post)(expr *, void *, int),
                             stmt *fStmt, void *arg1, int arg2)
{
    if (fStmt) {
        Cg->lastSourceLoc = fStmt->commonst.loc;
        switch (fStmt->exprst.kind) {
        case EXPR_STMT:
            fStmt->exprst.exp = ApplyToNodes(pre, post, fStmt->exprst.exp, arg1, arg2);
            break;
        case IF_STMT:
            fStmt->ifst.cond = ApplyToNodes(pre, post, fStmt->ifst.cond, arg1, arg2);
            break;
        case WHILE_STMT:
        case DO_STMT:
            fStmt->whilest.cond = ApplyToNodes(pre, post, fStmt->whilest.cond, arg1, arg2);
            break;
        case FOR_STMT:
            fStmt->forst.cond = ApplyToNodes(pre, post, fStmt->forst.cond, arg1, arg2);
            break;
        case BLOCK_STMT:
            break;
        case RETURN_STMT:
            fStmt->returnst.exp = ApplyToNodes(pre, post, fStmt->returnst.exp, arg1, arg2);
            break;
        case DISCARD_STMT:
            fStmt->discardst.cond = ApplyToNodes(pre, post, fStmt->discardst.cond, arg1, arg2);
            break;
        case COMMENT_STMT:
            break;
        default:
            assert(0);
            break;
        }
        //fStmt = fStmt->exprst.next;
    }
} // ApplyToExpressionsLocal

/*
 * ApplyToTopExpressions() - Walk a source tree and apply a function to each expression.
 *
 */

void ApplyToTopExpressions(expr *(*fun)(expr *, void *, int), stmt *fStmt, void *arg1, int arg2)
{
    while (fStmt) {
        Cg->lastSourceLoc = fStmt->commonst.loc;
        switch (fStmt->exprst.kind) {
        case EXPR_STMT:
            fStmt->exprst.exp = fun(fStmt->exprst.exp, arg1, arg2);
            break;
        case IF_STMT:
            fStmt->ifst.cond = fun(fStmt->ifst.cond, arg1, arg2);
            ApplyToTopExpressions(fun, fStmt->ifst.thenstmt, arg1, arg2);
            ApplyToTopExpressions(fun, fStmt->ifst.elsestmt, arg1, arg2);
            break;
        case WHILE_STMT:
        case DO_STMT:
            fStmt->whilest.cond = fun(fStmt->whilest.cond, arg1, arg2);
            ApplyToTopExpressions(fun, fStmt->whilest.body, arg1, arg2);
            break;
        case FOR_STMT:
            ApplyToTopExpressions(fun, fStmt->forst.init, arg1, arg2);
            fStmt->forst.cond = fun(fStmt->forst.cond, arg1, arg2);
            ApplyToTopExpressions(fun, fStmt->forst.body, arg1, arg2);
            ApplyToTopExpressions(fun, fStmt->forst.step, arg1, arg2);
            break;
        case BLOCK_STMT:
            ApplyToTopExpressions(fun, fStmt->blockst.body, arg1, arg2);
            break;
        case RETURN_STMT:
            fStmt->returnst.exp = fun(fStmt->returnst.exp, arg1, arg2);
            break;
        case DISCARD_STMT:
            fStmt->discardst.cond = fun(fStmt->discardst.cond, arg1, arg2);
            break;
        case COMMENT_STMT:
            break;
        default:
            assert(0);
            break;
        }
        fStmt = fStmt->exprst.next;
    }
} // ApplyToTopExpressions

/*
 * ApplyToStatements() - Walk a source tree and apply a transformations to
 *    each statememt
 */

stmt *ApplyToStatements(stmt *(*pre)(stmt *, void *, int),
                        stmt *(*post)(stmt *, void *, int),
                        stmt *fStmt, void *arg1, int arg2)
{
    stmt *head = NULL, *last = NULL, *lStmt, *next, *rest = fStmt;

    while (fStmt) {
        // Transform each statement into a possible NULL list of statements:
        // Prepend any statements returned to the list to be processed, and
        // remember what the next one to be done is (rest), so we don't
        // rerun pre on any of the returned statements directly.
        if (pre && rest == fStmt) {
            Cg->lastSourceLoc = fStmt->commonst.loc;
            rest = fStmt->commonst.next;
            fStmt->commonst.next = NULL;
            lStmt = pre(fStmt, arg1, arg2);
            if (lStmt) {
                fStmt = lStmt;
                while (lStmt->commonst.next && lStmt->commentst.next != rest) {
                    lStmt = lStmt->commonst.next;
                }
                lStmt->commonst.next = rest;
            } else {
                // Nothing returned - go to next statement:
                fStmt = rest;
                continue;
            }
        }

        // Now apply transformation to substatements:

        switch (fStmt->exprst.kind) {
        case EXPR_STMT:
            break;
        case IF_STMT:
            fStmt->ifst.thenstmt = ApplyToStatements(pre, post, fStmt->ifst.thenstmt, arg1, arg2);
            fStmt->ifst.elsestmt = ApplyToStatements(pre, post, fStmt->ifst.elsestmt, arg1, arg2);
            break;
        case WHILE_STMT:
        case DO_STMT:
            fStmt->whilest.body = ApplyToStatements(pre, post, fStmt->whilest.body, arg1, arg2);
            break;
        case FOR_STMT:
            fStmt->forst.init = ApplyToStatements(pre, post, fStmt->forst.init, arg1, arg2);
            fStmt->forst.body = ApplyToStatements(pre, post, fStmt->forst.body, arg1, arg2);
            fStmt->forst.step = ApplyToStatements(pre, post, fStmt->forst.step, arg1, arg2);
            break;
        case BLOCK_STMT:
            fStmt->blockst.body = ApplyToStatements(pre, post, fStmt->blockst.body, arg1, arg2);
            break;
        case RETURN_STMT:
        case DISCARD_STMT:
        case COMMENT_STMT:
            break;
        default:
            assert(0);
            break;
        }

        // Append any statements returned by "post" to the end of the list:

        next = fStmt->commonst.next;
        if (post) {
            Cg->lastSourceLoc = fStmt->commonst.loc;
            lStmt = post(fStmt, arg1, arg2);
        } else {
            lStmt = fStmt;
        }
        if (lStmt) {
            if (head) {
                last->commonst.next = lStmt;
            } else {
                head = lStmt;
            }
            last = lStmt;
            while (last->commonst.next && last->commentst.next != next) {
                last = last->commonst.next;
            }
            last->commonst.next = NULL;
        }
        fStmt = next;
    }
    return head;
} // ApplyToStatements

/*
 * PostApplyToChildStatements() - Apply a postfix order transformation to each child
 *         statememt of this statement.
 */

void PostApplyToChildStatements(stmt *(*fun)(stmt *, void *, int), stmt *fStmt, void *arg1, int arg2)
{
    if (fStmt) {

        // Apply a transformation to each nested statement, but not the top level statements:

        Cg->lastSourceLoc = fStmt->commonst.loc;
        switch (fStmt->exprst.kind) {
        case EXPR_STMT:
            break;
        case IF_STMT:
            fStmt->ifst.thenstmt = PostApplyToStatements(fun, fStmt->ifst.thenstmt, arg1, arg2);
            fStmt->ifst.elsestmt = PostApplyToStatements(fun, fStmt->ifst.elsestmt, arg1, arg2);
            break;
        case WHILE_STMT:
        case DO_STMT:
            fStmt->whilest.body = PostApplyToStatements(fun, fStmt->whilest.body, arg1, arg2);
            break;
        case FOR_STMT:
            fStmt->forst.init = PostApplyToStatements(fun, fStmt->forst.init, arg1, arg2);
            fStmt->forst.body = PostApplyToStatements(fun, fStmt->forst.body, arg1, arg2);
            fStmt->forst.step = PostApplyToStatements(fun, fStmt->forst.step, arg1, arg2);
            break;
        case BLOCK_STMT:
            fStmt->blockst.body = PostApplyToStatements(fun, fStmt->blockst.body, arg1, arg2);
            break;
        case RETURN_STMT:
        case DISCARD_STMT:
        case COMMENT_STMT:
            break;
        default:
            assert(0);
            break;
        }
    }
} // PostApplyToChildStatements

