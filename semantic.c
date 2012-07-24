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
// semantic.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"

/*
 * GetVectorConst()
 *
 */

static void GetVectorConst(float *fVal, expr *fExpr)
{
    int Oops = 0;
    constant *pconst;
    binary *pbin;
    unary *pun;
    int ii;

    for (ii = 0; ii < 4; ii++)
        fVal[ii] = 0.0f;
    if (fExpr) {
        switch (fExpr->common.kind) {
        case UNARY_N:
            pun = (unary *) fExpr;
            switch (pun->op) {
            //case FCONST_V_OP:
            //    for (ii = 0; ii < SUBOP_GET_S1(pun->subop); ii++)
            //        fVal[ii] = 1.1f;
            //    break;
            case VECTOR_V_OP:
                Oops = 2;
                break;
            default:
                Oops = 1;
                break;
            }
            break;
        case BINARY_N:
            pbin = (binary *) fExpr;
            switch (pbin->op) {
            case EXPR_LIST_OP:
                if (pbin->right == NULL &&
                    pbin->left->common.kind == CONST_N)
                {
                    pconst = (constant *) pbin->left;
                    switch (pconst->op) {
                    case FCONST_V_OP:
                    case HCONST_V_OP:
                    case XCONST_V_OP:
                        for (ii = 0; ii < SUBOP_GET_S1(pconst->subop); ii++)
                            fVal[ii] = pconst->val[ii].f;
                        break;
                    case ICONST_V_OP:
                    case BCONST_V_OP:
                        for (ii = 0; ii < SUBOP_GET_S1(pconst->subop); ii++)
                            fVal[ii] = (float) pconst->val[ii].i;
                        break;
                    case FCONST_OP:
                    case HCONST_OP:
                    case XCONST_OP:
                        fVal[0] = pconst->val[0].f;
                        break;
                    case ICONST_OP:
                    case BCONST_OP:
                        fVal[0] = (float) pconst->val[0].i;
                        break;
                    default:
                        Oops = 4;
                        break;
                    }
                } else {
                    Oops = 3;
                }
                break;
            default:
                Oops = 1;
                break;
            }
            break;
        default:
            Oops = 1;
            break;
        }
    }
    if (Oops)
        SemanticWarning(Cg->tokenLoc, 9999, "*** GetVectorConst() not finished ***");
} // GetVectorConst

/*
 * lNewUniformSemantic() - Add an entry to the uniform semantic table.
 *
 */
static void lNewUniformSemantic(int gname, Symbol *fSymb, int semantics)
{
    UniformSemantic *lUniform, *nUniform;

    lUniform = NewUniformSemantic(gname, fSymb->name, semantics);
    nUniform = Cg->theHAL->uniforms;
    if (nUniform) {
        while (nUniform->next)
            nUniform = nUniform->next;
        nUniform->next = lUniform;
    } else {
        Cg->theHAL->uniforms = lUniform;
    }
} // lNewUniformSemantic

/*
 * lBindUniformVariable() - Bind a uniform variable to $uniform connector.  Record semantics
 *         value if present.
 */

static int lBindUniformVariable(Symbol *fSymb, int gname, int IsParameter)
{
    int category, domain, qualifiers, OK;
    BindingTree *ltree;
    Binding *lBind;
    SymbolList **lList, *mList, *nList;

    OK = 0;
    category = GetCategory(fSymb->type);
    domain = GetDomain(fSymb->type);
    qualifiers = GetQualifiers(fSymb->type);
    switch (category) {
    case TYPE_CATEGORY_SCALAR:
    case TYPE_CATEGORY_ARRAY:
    case TYPE_CATEGORY_STRUCT:
        lNewUniformSemantic(gname, fSymb, fSymb->details.var.semantics);
        if (IsParameter) {
            lList = &Cg->theHAL->uniformParam;
        } else {
            lList = &Cg->theHAL->uniformGlobal;
        }
        lBind = /* theHAL-> */ NewBinding(gname, fSymb->name);
        lBind->none.properties = BIND_INPUT | BIND_UNIFORM;
        ltree = LookupBinding(gname, fSymb->name);
        if (ltree) {
            if (Cg->theHAL->BindUniformPragma(&fSymb->loc, fSymb, lBind, &ltree->binding)) {
                if (!(lBind->none.properties & BIND_UNIFORM)) {
                    SemanticError(&fSymb->loc, ERROR_S_NON_UNIF_BIND_TO_UNIF_VAR,
                                  GetAtomString(atable, fSymb->name));
                    lList = NULL;
                }
            } else {
                SemanticError(&fSymb->loc, ERROR_S_INCOMPATIBLE_BIND_DIRECTIVE,
                              GetAtomString(atable, fSymb->name));
                lList = NULL;
            }
        }
        if (lList && !(lBind->none.properties & BIND_HIDDEN)) {
            OK = 1;
            fSymb->details.var.bind = lBind;
            nList = (SymbolList*) malloc(sizeof(SymbolList));
            nList->next = NULL;
            nList->symb = fSymb;
            if (*lList) {
                mList = *lList;
                while (mList->next != NULL)
                    mList = mList->next;
                mList->next = nList;
            } else {
                *lList = nList;
            }
        }
        break;
    default:
        SemanticError(&fSymb->loc, ERROR_S_ILLEGAL_TYPE_UNIFORM_VAR,
                      GetAtomString(atable, fSymb->name));
        break;
    }
    return OK;
} // lBindUniformVariable

/*
 * lBindVaryingVariable() - Bind a variable to $vin, $vout, or $uniform connector.
 *
 */

static Symbol *lBindVaryingVariable(Symbol *fSymb, int gname, int IsOutVal, int IsStructMember,
                                    int structSemantics)
{
    int category, domain, qualifiers;
    Symbol *lSymb, *mSymb;
    BindingTree *ltree;
    Binding *lBind;
    Scope *lScope;
    int lname;

    lSymb = NULL;
    category = GetCategory(fSymb->type);
    domain = GetDomain(fSymb->type);
    qualifiers = GetQualifiers(fSymb->type);
    switch (category) {
    case TYPE_CATEGORY_SCALAR:
    case TYPE_CATEGORY_ARRAY:
        lScope = NULL;
        lname = 0;
        lBind = /* theHAL-> */ NewBinding(gname, fSymb->name);
        ltree = LookupBinding(gname, fSymb->name);
        if (fSymb->details.var.semantics) {
            if (ltree) {
                SemanticWarning(&fSymb->loc, WARNING_S_SEMANTICS_AND_BINDING,
                                GetAtomString(atable, fSymb->name));
            }
            lname = fSymb->details.var.semantics;
            if (Cg->theHAL->BindVaryingSemantic(&fSymb->loc, fSymb, lname, lBind, IsOutVal)) {
                if (lBind->none.properties & BIND_INPUT) {
                    if (IsOutVal) {
                        SemanticError(&fSymb->loc, ERROR_S_OUT_QUALIFIER_IN_SEMANTIC,
                                      GetAtomString(atable, fSymb->name));
                    }
                    lScope = Cg->theHAL->varyingIn->type->str.members;
                } else {
                    if (!IsOutVal) {
                        SemanticError(&fSymb->loc, ERROR_S_IN_QUALIFIER_OUT_SEMANTIC,
                                      GetAtomString(atable, fSymb->name));
                    }
                    lScope = Cg->theHAL->varyingOut->type->str.members;
                }
            } else {
                SemanticError(&fSymb->loc, ERROR_S_UNKNOWN_SEMANTICS,
                              GetAtomString(atable, fSymb->name));
            }
        } else {
            // If no semantics, check for #pragma bind directive.
            lname = fSymb->name;
            if (ltree) {
                if (Cg->theHAL->BindVaryingPragma(&fSymb->loc, fSymb, lBind, &ltree->binding, IsOutVal)) {
                    if (lBind->none.properties & BIND_UNIFORM) {
                        SemanticError(&fSymb->loc, ERROR_S_UNIF_BIND_TO_NON_UNIF_VAR,
                                      GetAtomString(atable, fSymb->name));
                    } else {
                        if (lBind->none.properties & BIND_INPUT) {
                            lScope = Cg->theHAL->varyingIn->type->str.members;
                        } else {
                            lScope = Cg->theHAL->varyingOut->type->str.members;
                        }
                    }
                } else {
                    SemanticError(&fSymb->loc, ERROR_S_INCOMPATIBLE_BIND_DIRECTIVE,
                                  GetAtomString(atable, fSymb->name));
                }
            } else {
                // If no semantics or #pragma bind, get default binding from profile if it allows them:
                if (Cg->theHAL->BindVaryingUnbound(&fSymb->loc, fSymb, lname, structSemantics, lBind, IsOutVal)) {
                    if (IsOutVal) {
                        lScope = Cg->theHAL->varyingOut->type->str.members;
                    } else {
                        lScope = Cg->theHAL->varyingIn->type->str.members;
                    }
                } else {
                    SemanticError(&fSymb->loc, ERROR_S_SEMANTIC_NOT_DEFINED_VOUT,
                                  GetAtomString(atable, fSymb->name));
                }
            }
        }
        if (lScope) {
            lBind->none.lname = lname;
            fSymb->details.var.bind = lBind;
            if (!(lBind->none.properties & BIND_HIDDEN)) {
                lSymb = LookUpLocalSymbol(lScope, lname);
                if (lSymb) {
                    // Already defined - second use of this name.
                } else {
                    lSymb = AddSymbol(&fSymb->loc, lScope, lname, fSymb->type, VARIABLE_S);
                    lSymb->details.var.bind = lBind;
                    if (lScope->symbols != lSymb) {
                        mSymb = lScope->symbols;
                        while (mSymb->next)
                            mSymb = mSymb->next;
                        mSymb->next = lSymb;
                    }
                }
            }
        }
        break;
    case TYPE_CATEGORY_STRUCT:
        SemanticError(&fSymb->loc, ERROR_S_NESTED_SEMANTIC_STRUCT,
                      GetAtomString(atable, fSymb->name));
        break;
    default:
        SemanticError(&fSymb->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                      GetAtomString(atable, fSymb->name));
        break;
    }
    return lSymb;
} // lBindVaryingVariable

/*
 * lVerifyConnectorDirection() - Verify that this connector name is valid for the current
 *         profile and is of the appropriate direction.
 */

static void lVerifyConnectorDirection(SourceLoc *loc, int semantics, int IsOutParam)
{
    int cid, uses;

    // If connector semantics present make sure that connector direction matches parameter's:

    if (semantics) {
        cid = Cg->theHAL->GetConnectorID(semantics);
        if (cid) {
            uses = Cg->theHAL->GetConnectorUses(cid, Cg->theHAL->pid);
            if (IsOutParam) {
                if (!(uses & CONNECTOR_IS_OUTPUT))
                    SemanticError(loc, ERROR_S_CONNECT_FOR_INPUT,
                                  GetAtomString(atable, semantics));
            } else {
                if (!(uses & CONNECTOR_IS_INPUT))
                    SemanticError(loc, ERROR_S_CONNECT_FOR_OUTPUT,
                                  GetAtomString(atable, semantics));
            }
        } else {
            SemanticError(loc, ERROR_S_CONNECTOR_TYPE_INVALID,
                          GetAtomString(atable, semantics));
        }
    }
} // lVerifyConnectorDirection

/*
 * BuildSemanticStructs() - Build the three global semantic type structure,  Check main for
 *         type errors in its arguments.
 */

void BuildSemanticStructs(SourceLoc *loc, Scope *fScope, Symbol *program)
{
    int category, domain, qualifiers, len, rlen;
    Scope *vinScope, *voutScope, *lScope;
    Type *vinType, *voutType;
    Symbol *vinVar, *voutVar;
    int vinTag, voutTag;
    Symbol *formal, *member, *lSymb;
    expr *lExpr, *rExpr, *vExpr;
    Type *lType, *rettype;
    StmtList instmts, outstmts;
    Binding *lBind;
    int IsOutParam;
    float lVal[4];
    stmt *lStmt;

    // Define pseudo type structs for semantics:

    vinScope = NewScope();
    vinScope->HasSemantics = 1;
    vinScope->level = 1;
    vinScope->IsStructScope = 1;
    voutScope = NewScope();
    voutScope->HasSemantics = 1;
    voutScope->level = 1;
    voutScope->IsStructScope = 1;

    vinTag = AddAtom(atable, "$vin");
    vinType = StructHeader(loc, fScope, 0, vinTag);
    vinType->str.members = vinScope;
    Cg->theHAL->varyingIn = vinVar = DefineVar(loc, fScope, vinTag, vinType);
    //vinTypedef = DefineTypedef(loc, fScope, vinTag, vinType); // Not sure this is neessary
    voutTag = AddAtom(atable, "$vout");
    voutType = StructHeader(loc, fScope, 0, voutTag);
    voutType->str.members = voutScope;
    Cg->theHAL->varyingOut = voutVar = DefineVar(loc, fScope, voutTag, voutType);
    //voutTypedef = DefineTypedef(loc, fScope, voutTag, voutType); // Not sure this is neessary

    instmts.first = instmts.last = NULL;
    outstmts.first = outstmts.last = NULL;

    // Walk list of formals creating semantic struct members for all parameters:

    formal = program->details.fun.params;
    while (formal) {
        category = GetCategory(formal->type);
        domain = GetDomain(formal->type);
        qualifiers = GetQualifiers(formal->type);
        if ((qualifiers & TYPE_QUALIFIER_INOUT) == TYPE_QUALIFIER_INOUT)
            SemanticError(&formal->loc, ERROR_S_MAIN_PARAMS_CANT_BE_INOUT,
                          GetAtomString(atable, formal->name));
        if (domain == TYPE_DOMAIN_UNIFORM) {
            if (qualifiers & TYPE_QUALIFIER_OUT) {
                SemanticError(&formal->loc, ERROR_S_UNIFORM_ARG_CANT_BE_OUT,
                              GetAtomString(atable, formal->name));
            }
            switch (category) {
            case TYPE_CATEGORY_SCALAR:
            case TYPE_CATEGORY_ARRAY:
            case TYPE_CATEGORY_STRUCT:
                if (lBindUniformVariable(formal, program->name, 1) && formal->details.var.init) {
                    formal->details.var.init = FoldConstants(formal->details.var.init);
                    GetVectorConst(lVal, formal->details.var.init);
                    lBind = NewConstDefaultBinding(0, formal->name, 4, 0, 0, lVal);
                    lBind->constdef.kind = BK_DEFAULT;
                    AddDefaultBinding(lBind);
                }
                break;
            default:
                SemanticError(&formal->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                              GetAtomString(atable, formal->name));
                break;
            }
        } else {
            IsOutParam = (qualifiers & TYPE_QUALIFIER_OUT) != 0;
            switch (category) {
            case TYPE_CATEGORY_SCALAR:
            case TYPE_CATEGORY_ARRAY:
                lSymb = lBindVaryingVariable(formal, program->name, IsOutParam, 0, 0);
                if (lSymb) {
                    lBind = lSymb->details.var.bind;
                    if (lBind && !(lBind->none.properties & BIND_HIDDEN)) {
                        lExpr = GenSymb(formal);
                        if (IsScalar(formal->type) || IsVector(formal->type, &len)) {
                            if (lBind->none.properties & BIND_INPUT) {
                                // Assign $vin member to bound variable:
                                vExpr = (expr *) NewSymbNode(VARIABLE_OP, vinVar);
                                rExpr = GenMemberReference(vExpr, lSymb);
                                if (IsVector(lSymb->type, &rlen))
                                    rExpr = GenConvertVectorLength(rExpr, GetBase(lSymb->type), rlen, len);
                                lStmt = NewSimpleAssignmentStmt(&program->loc, lExpr, rExpr, 0);
                                AppendStatements(&instmts, lStmt);
                            } else {
                                // Assign bound variable to $vout member:
                                vExpr = (expr *) NewSymbNode(VARIABLE_OP, voutVar);
                                rExpr = GenMemberReference(vExpr, lSymb);
                                if (IsVector(lSymb->type, &rlen))
                                    lExpr = GenConvertVectorLength(lExpr, GetBase(formal->type), len, rlen);
                                lStmt = NewSimpleAssignmentStmt(&program->loc, rExpr, lExpr, 0);
                                AppendStatements(&outstmts, lStmt);
                            }
                        } else {
                            FatalError("Parameter of unsupported type");
                            // xxx
                        }
                    }
                }
                break;
            case TYPE_CATEGORY_STRUCT:
                lType = formal->type;
                lVerifyConnectorDirection(&formal->loc, lType->str.semantics, IsOutParam);
                lScope = lType->str.members;
                member = lScope->symbols;
                while (member) {
                    lSymb = lBindVaryingVariable(member, lType->str.tag, IsOutParam, 1,
                                                 lType->str.semantics);
                    if (lSymb) {
                        lBind = lSymb->details.var.bind;
                        if (lBind && !(lBind->none.properties & BIND_HIDDEN)) {
                            lExpr = GenMemberReference((expr *) NewSymbNode(VARIABLE_OP, formal), member);
                            if (IsScalar(member->type) || IsVector(member->type, &len)) {
                                if (lBind->none.properties & BIND_INPUT) {
                                    // Assign $vin member to bound variable:
                                    vExpr = (expr *) NewSymbNode(VARIABLE_OP, vinVar);
                                    rExpr = GenMemberReference(vExpr, lSymb);
                                    if (IsVector(lSymb->type, &rlen))
                                        rExpr = GenConvertVectorLength(rExpr, GetBase(lSymb->type), rlen, len);
                                    lStmt = NewSimpleAssignmentStmt(&program->loc, lExpr, rExpr, 0);
                                    AppendStatements(&instmts, lStmt);
                                } else {
                                    // Assign bound variable to $vout member:
                                    vExpr = (expr *) NewSymbNode(VARIABLE_OP, voutVar);
                                    rExpr = GenMemberReference(vExpr, lSymb);
                                    if (IsVector(lSymb->type, &rlen))
                                        lExpr = GenConvertVectorLength(lExpr, GetBase(member->type), len, rlen);
                                    lStmt = NewSimpleAssignmentStmt(&program->loc, rExpr, lExpr, 0);
                                    AppendStatements(&outstmts, lStmt);
                                }
                            } else {
                                FatalError("Parameter of unsupported type");
                                // xxx
                            }
                        }
                    }
                    member = member->next;
                }
                break;
            default:
                SemanticError(&formal->loc, ERROR_S_ILLEGAL_PARAM_TO_MAIN,
                              GetAtomString(atable, formal->name));
                break;
            }
        }
        formal = formal->next;
    }

    // Add return value's semantics to the $vout connector:

    lType = program->type;
    rettype = lType->fun.rettype;
    category = GetCategory(rettype);
    if (!IsVoid(rettype)) {
        if (category == TYPE_CATEGORY_STRUCT) {
            lVerifyConnectorDirection(&program->loc, rettype->str.semantics, 1);
            lScope = rettype->str.members;
            member = lScope->symbols;
            while (member) {
                lSymb = lBindVaryingVariable(member, rettype->str.tag, 1, 1,
                                             rettype->str.semantics);
                member = member->next;
            }
        } else {
            SemanticError(&program->loc, ERROR_S_PROGRAM_MUST_RETURN_STRUCT,
                          GetAtomString(atable, program->name));
        }
    }

    // Set the output connector variety:

    voutType->str.variety = Cg->theHAL->outcid;

    // Append initial and final assignment statements to beginning and end of main:

    program->details.fun.statements = ConcatStmts(instmts.first, program->details.fun.statements);
    program->details.fun.statements = ConcatStmts(program->details.fun.statements, outstmts.first);

} // BuildSemanticStructs


void BindDefaultSemantic(Symbol *lSymb, int category, int gname)
{
    Binding *lBind;
    float lVal[4];

    switch (category) {
    case TYPE_CATEGORY_SCALAR:
    case TYPE_CATEGORY_ARRAY:
    case TYPE_CATEGORY_STRUCT:
        gname = 0;
        if (lBindUniformVariable(lSymb, gname, 0) && lSymb->details.var.init) {
            lSymb->details.var.init = FoldConstants(lSymb->details.var.init);
            GetVectorConst(lVal, lSymb->details.var.init);
            lBind = NewConstDefaultBinding(0, lSymb->name, 4, 0, 0, lVal);
            lBind->constdef.kind = BK_DEFAULT;
            AddDefaultBinding(lBind);
        }
        break;
    default:
        SemanticError(&lSymb->loc, ERROR_S_NON_STATIC_GLOBAL_TYPE_ERR,
                      GetAtomString(atable, lSymb->name));
    }
    lSymb->properties &= ~SYMB_NEEDS_BINDING;
} // BindDefaultSemantic
