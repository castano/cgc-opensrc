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
// binding.c
//

#include <stdlib.h>
#include <stdio.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// Connector and Parameter Binding Functions: //////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * lInitBinding()
 *
 */

static void lInitBinding(Binding *fBind)
{
    fBind->none.properties = 0;
    fBind->none.gname = 0;
    fBind->none.lname = 0;
    fBind->none.base = 0;
    fBind->none.size = 0;
    fBind->none.kind = BK_NONE;
} // lInitBinding

/*
 * NewBinding()
 *
 */

Binding *NewBinding(int gname, int sname)
{
    Binding *lBind;

    lBind = (Binding *) malloc(sizeof(Binding));
    lInitBinding(lBind);
    lBind->none.gname = gname;
    lBind->none.lname = sname;
    return lBind;
} // NewBinding

/*
 * NewConstDefaultBinding()
 *
 */

Binding *NewConstDefaultBinding(int gname, int sname, int count, int rname, int regno,
                                float *fval)
{
    Binding *lBind;
    int ii;

    lBind = NewBinding(gname, sname);
    lBind->constdef.kind = BK_CONSTANT;
    lBind->constdef.size = count;
    lBind->constdef.rname = rname;
    lBind->constdef.regno = regno;
    for (ii = 0; ii < count; ii++)
        lBind->constdef.val[ii] = fval[ii];
    return lBind;
} // NewConstDefaultBinding

/*
 * NewBindingTree()
 *
 */

BindingTree *NewBindingTree(SourceLoc *loc)
{
    BindingTree *lTree;

    lTree = (BindingTree *) malloc(sizeof(BindingTree));
    lInitBinding(&lTree->binding);
    lTree->nextc = NULL;
    lTree->nextm = NULL;
    lTree->loc = *loc;
    lTree->loc.line = 0;
    return lTree;
} // NewBindingTree

/*
 * NewConnectorBindingTree()
 *
 */

BindingTree *NewConnectorBindingTree(SourceLoc *loc, int cname, int mname, int rname)
{
    BindingTree *lTree;
    BindingConnector *lBind;

    lTree = NewBindingTree(loc);
    lBind = &lTree->binding.conn;
    lBind->gname = cname;
    lBind->lname = mname;
    lBind->kind = BK_CONNECTOR;
    lBind->rname = rname;
    lBind->regno = 0;
    return lTree;
} // NewConnectorBindingTree

/*
 * NewRegArrayBindingTree()
 *
 */

BindingTree *NewRegArrayBindingTree(SourceLoc *loc, int pname, int aname, int rname, int regno,
                                    int count)
{
    BindingTree *lTree;
    BindingRegArray *lBind;

    lTree = NewBindingTree(loc);
    lBind = &lTree->binding.reg;
    lBind->gname = pname;
    lBind->lname = aname;
    lBind->kind = BK_REGARRAY;
    lBind->rname = rname;
    lBind->regno = regno;
    lBind->count = count;
    return lTree;
} // NewRegArrayBindingTree

/*
 * NewTexunitBindingTree()
 *
 */

BindingTree *NewTexunitBindingTree(SourceLoc *loc, int pname, int aname, int unitno)
{
    BindingTree *lTree;
    BindingTexunit *lBind;

    lTree = NewBindingTree(loc);
    lBind = &lTree->binding.texunit;
    lBind->gname = pname;
    lBind->lname = aname;
    lBind->kind = BK_TEXUNIT;
    lBind->unitno = unitno;
    return lTree;
} // NewTexunitBindingTree

/*
 * NewConstDefaultBindingTree()
 *
 */

BindingTree *NewConstDefaultBindingTree(SourceLoc *loc, int kind, int pname, int aname,
                                        int count, float *fval)
{
    BindingTree *lTree;
    BindingConstDefault *lBind;
    int ii;

    if (count > 4)
        count = 4;
    lTree = NewBindingTree(loc);
    lBind = &lTree->binding.constdef;
    lBind->gname = pname;
    lBind->lname = aname;
    lBind->kind = kind;
    lBind->size = count;
    lBind->rname = 0;
    lBind->regno = 0;
    for (ii = 0; ii < count; ii++)
        lBind->val[ii] = fval[ii];
    return lTree;
} // NewConstDefaultBindingTree

/*
 * LookupBinding()
 *
 */

BindingTree *LookupBinding(int gname, int lname)
{
    BindingTree *lTree;

    lTree = Cg->bindings;
    while (lTree) {
        if (lTree->binding.none.gname == gname) {
            do {
                if (lTree->binding.none.lname == lname) {
                    return lTree;
                } else {
                    lTree = lTree->nextm;
                }
            } while (lTree);
            return NULL;
        }
        lTree = lTree->nextc;
    }
    return NULL;
} // LookupBinding

/*
 * AddBinding()
 *
 */

void AddBinding(BindingTree *fTree)
{
    BindingTree *lTree;

    lTree = Cg->bindings;
    while (lTree) {
        if (lTree->binding.none.gname == fTree->binding.none.gname)
            break;
        lTree = lTree->nextc;
    }
    if (lTree) {
        fTree->nextm = lTree->nextm;
        lTree->nextm = fTree;
    } else {
        fTree->nextc = Cg->bindings;
        Cg->bindings = fTree;
    }
} // AddBinding

/*
 * DefineConnectorBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <conn-id> "." <memb-id> "=" <reg-id>
 *
 */

void DefineConnectorBinding(SourceLoc *loc, int cname, int mname, int rname)
{
    BindingTree *lTree;

    lTree = LookupBinding(cname, mname);
    if (lTree) {
        SemanticError(loc, ERROR_SSSD_DUPLICATE_BINDING,
            GetAtomString(atable, cname), GetAtomString(atable, mname),
            GetAtomString(atable, lTree->loc.file), lTree->loc.line);
        return;
    }
    lTree = NewConnectorBindingTree(loc, cname, mname, rname);
    AddBinding(lTree);
} // DefineConnectorBinding

/*
 * DefineRegArrayBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" <reg-id>
 *
 */

void DefineRegArrayBinding(SourceLoc *loc, int pname, int aname, int rname, int index,
                           int count)
{
    BindingTree *lTree;

    lTree = LookupBinding(pname, aname);
    if (lTree) {
        SemanticError(loc, ERROR_SSSD_DUPLICATE_BINDING,
            GetAtomString(atable, pname), GetAtomString(atable, aname),
            GetAtomString(atable, lTree->loc.file), lTree->loc.line);
        return;
    }
    lTree = NewRegArrayBindingTree(loc, pname, aname, rname, index, count);
    AddBinding(lTree);
} // DefineRegArrayBinding

/*
 * DefineTexunitBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" <reg-id> <i-const> [ <i-const> ]
 *
 */

void DefineTexunitBinding(SourceLoc *loc, int pname, int aname, int unitno)
{
    BindingTree *lTree;

    lTree = LookupBinding(pname, aname);
    if (lTree) {
        SemanticError(loc, ERROR_SSSD_DUPLICATE_BINDING,
            GetAtomString(atable, pname), GetAtomString(atable, aname),
            GetAtomString(atable, lTree->loc.file), lTree->loc.line);
        return;
    }
    lTree = NewTexunitBindingTree(loc, pname, aname, unitno);
    AddBinding(lTree);
} // DefineTexunitBinding

/*
 * DefineConstantBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" "constant" <simple-float-expr>+
 *
 */

void DefineConstantBinding(SourceLoc *loc, int pname, int aname, int count, float *fval)
{
    BindingTree *lTree;

    lTree = LookupBinding(pname, aname);
    if (lTree) {
        SemanticError(loc, ERROR_SSSD_DUPLICATE_BINDING,
            GetAtomString(atable, pname), GetAtomString(atable, aname),
            GetAtomString(atable, lTree->loc.file), lTree->loc.line);
        return;
    }
    lTree = NewConstDefaultBindingTree(loc, BK_CONSTANT, pname, aname, count, fval);
    AddBinding(lTree);
} // DefineConstantBinding

/*
 * DefineDefaultBinding() - Define a binding between a member of a connector and a hw reg.
 *
 * #pragma bind <prog-id> "." <arg-id> "=" "default" <simple-float-expr>+
 *
 */

void DefineDefaultBinding(SourceLoc *loc, int pname, int aname, int count, float *fval)
{
    BindingTree *lTree;

    lTree = LookupBinding(pname, aname);
    if (lTree) {
        SemanticError(loc, ERROR_SSSD_DUPLICATE_BINDING,
            GetAtomString(atable, pname), GetAtomString(atable, aname),
            GetAtomString(atable, lTree->loc.file), lTree->loc.line);
        return;
    }
    lTree = NewConstDefaultBindingTree(loc, BK_DEFAULT, pname, aname, count, fval);
    AddBinding(lTree);
} // DefineDefaultBinding

///////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////// Uniform Semantics: /////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * NewUniformSemantic() - Aloocate a new UniformSemantic record.
 *
 */

UniformSemantic *NewUniformSemantic(int gname, int vname, int semantic)
{
    UniformSemantic *lSemantic;

    lSemantic = (UniformSemantic *) malloc(sizeof(UniformSemantic));
    lSemantic->next = NULL;
    lSemantic->gname = gname;
    lSemantic->vname = vname;
    lSemantic->semantic = semantic;

    return lSemantic;
} // NewUniformSemantic

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of binding.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
