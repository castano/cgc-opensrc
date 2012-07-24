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
// generic_hal.c
//

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "slglobals.h"
#include "generic_hal.h"

#define NUMELS(x) (sizeof(x) / sizeof((x)[0]))

// Static functions
static int InitHAL_generic(slHAL *fHAL);
static int FreeHAL_generic(slHAL *fHAL);
static int RegisterNames_generic(slHAL *fHAL);
static int GetConnectorID_generic(int name);
static int GetConnectorAtom_generic(int name);
static int GetConnectorUses_generic(int cid, int pid);
static int GetConnectorRegister_generic(int cid, int ByIndex, int ratom,
                                        Binding *fBind);
static int GetCapsBit_generic(int bitNumber);
static int CheckInternalFunction_generic(Symbol *fSymb, int *group);
static int BindUniformUnbound_generic(SourceLoc *loc, Symbol *fSymb,
                                      Binding *fBind);
static int BindVaryingSemantic_generic(SourceLoc *loc, Symbol *fSymb,
                                       int semantic, Binding *fBind,
                                       int IsOutVal);
static int PrintCodeHeader_generic(FILE *out);
static int GenerateCode_generic(SourceLoc *loc, Scope *fScope, Symbol *program);
// Static data
#define FLT TYPE_BASE_FLOAT

// These define all the input connector registers for this profile
// you can have numltiple names that refer to the same register number
static ConnectorRegisters inputCRegs_generic[] = {
    { "ATTR0",  0, FLT, REG_AP2V_ATTR0,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR1",  0, FLT, REG_AP2V_ATTR1,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR2",  0, FLT, REG_AP2V_ATTR2,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR3",  0, FLT, REG_AP2V_ATTR3,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR4",  0, FLT, REG_AP2V_ATTR4,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR5",  0, FLT, REG_AP2V_ATTR5,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR6",  0, FLT, REG_AP2V_ATTR6,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR7",  0, FLT, REG_AP2V_ATTR7,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR8",  0, FLT, REG_AP2V_ATTR8,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR9",  0, FLT, REG_AP2V_ATTR9,  4, REG_ALLOC | REG_INPUT, },
    { "ATTR10", 0, FLT, REG_AP2V_ATTR10, 4, REG_ALLOC | REG_INPUT, },
    { "ATTR11", 0, FLT, REG_AP2V_ATTR11, 4, REG_ALLOC | REG_INPUT, },
    { "ATTR12", 0, FLT, REG_AP2V_ATTR12, 4, REG_ALLOC | REG_INPUT, },
    { "ATTR13", 0, FLT, REG_AP2V_ATTR13, 4, REG_ALLOC | REG_INPUT, },
    { "ATTR14", 0, FLT, REG_AP2V_ATTR14, 4, REG_ALLOC | REG_INPUT, },
    { "ATTR15", 0, FLT, REG_AP2V_ATTR15, 4, REG_ALLOC | REG_INPUT, },
};

static ConnectorRegisters outputCRegs_generic[] = {
// These are output register names
    { "HPOS",  0, FLT, REG_V2FR_HPOS,  4, REG_RESERVED | REG_OUTPUT | REG_WRITE_REQUIRED, },
    { "COL0",  0, FLT, REG_V2FR_COL0,  4, REG_RESERVED | REG_OUTPUT, },
    { "COL1",  0, FLT, REG_V2FR_COL1,  4, REG_RESERVED | REG_OUTPUT, },
    { "TEX0",  0, FLT, REG_V2FR_TEX0,  4, REG_ALLOC | REG_OUTPUT, },
    { "TEX1",  0, FLT, REG_V2FR_TEX1,  4, REG_ALLOC | REG_OUTPUT, },
    { "TEX2",  0, FLT, REG_V2FR_TEX2,  4, REG_ALLOC | REG_OUTPUT, },
    { "TEX3",  0, FLT, REG_V2FR_TEX3,  4, REG_ALLOC | REG_OUTPUT, },
    { "FOGC",  0, FLT, REG_V2FR_FOGC,  1, REG_RESERVED | REG_OUTPUT, },
    { "PSIZ",  0, FLT, REG_V2FR_PSIZ,  1, REG_RESERVED | REG_OUTPUT, },
};


// Semantics:
enum { AP2V_GROUP = 0, V2FR_GROUP = 1, };

static SemanticsDescriptor Semantics_generic[] = {
// These are semantics that can be attached to varying variables and
// parameters.  They usually correspond to the input and output registers
// defined above, but don't have to.  You can add multiple names for the
// the same thing as aliases
    // Varying input semantics:
    { "ATTRIB",   FLT, 4, REG_AP2V_ATTR0, 16, AP2V_GROUP, SEM_IN | SEM_VARYING, },
    // Varying output semantics:
    { "POSITION", FLT, 4, REG_V2FR_HPOS, 1, V2FR_GROUP, SEM_OUT | SEM_VARYING, },
    { "FOG",      FLT, 1, REG_V2FR_FOGC, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING, },
    { "COLOR",    FLT, 4, REG_V2FR_COL0, 2, V2FR_GROUP, SEM_OUT | SEM_VARYING, },
    { "PSIZE",    FLT, 1, REG_V2FR_PSIZ, 0, V2FR_GROUP, SEM_OUT | SEM_VARYING, },
    { "TEXCOORD", FLT, 4, REG_V2FR_TEX0, 4, V2FR_GROUP, SEM_OUT | SEM_VARYING, },
};


// These are the connector types which refer to the register names above
static ConnectorDescriptor connectors_generic[] = {
    { CID_GENERIC_IN_NAME,  0, CID_GENERIC_IN_ID,  CONNECTOR_IS_INPUT,  NUMELS(inputCRegs_generic),  inputCRegs_generic  },

    { CID_GENERIC_OUT_NAME, 0, CID_GENERIC_OUT_ID, CONNECTOR_IS_OUTPUT, NUMELS(outputCRegs_generic), outputCRegs_generic },
};


///////////////////////////////////////////////////////////////////////////////
/////////////////////////// Generic output Program ////////////////////////////
///////////////////////////////////////////////////////////////////////////////

/*
 * RegisterProfiles_generic() - Register the profiles supported by this module.
 */

int RegisterProfiles_generic(void)
{
    RegisterProfile(InitHAL_generic, PROFILE_GENERIC_NAME, PROFILE_GENERIC_ID);
    return 1;
} // RegisterProfiles_generic


/*
 * InitHAL_generic()
 */

static int InitHAL_generic(slHAL *fHAL)
{
    /* Initialize functions. */
    fHAL->InitHAL = InitHAL_generic;
    fHAL->FreeHAL = FreeHAL_generic;
    fHAL->RegisterNames = RegisterNames_generic;
    fHAL->GetConnectorID = GetConnectorID_generic;
    fHAL->GetConnectorAtom = GetConnectorAtom_generic;
    fHAL->GetConnectorUses = GetConnectorUses_generic;
    fHAL->GetConnectorRegister = GetConnectorRegister_generic;
    fHAL->GetCapsBit = GetCapsBit_generic;
    fHAL->CheckInternalFunction = CheckInternalFunction_generic;
    fHAL->BindUniformUnbound = BindUniformUnbound_generic;
    fHAL->BindVaryingSemantic = BindVaryingSemantic_generic;
    fHAL->PrintCodeHeader = PrintCodeHeader_generic;
    fHAL->GenerateCode = GenerateCode_generic;

    /* Initialize data. */
    fHAL->vendor = VENDOR_STRING_GENERIC;
    fHAL->version = VERSION_STRING_GENERIC;

    fHAL->semantics = Semantics_generic;
    fHAL->numSemantics = NUMELS(Semantics_generic);

    fHAL->incid = CID_GENERIC_IN_ID;
    fHAL->inputCRegs = inputCRegs_generic;
    fHAL->numInputCRegs = NUMELS(inputCRegs_generic);

    fHAL->outcid = CID_GENERIC_OUT_ID;
    fHAL->outputCRegs = outputCRegs_generic;
    fHAL->numOutputCRegs = NUMELS(outputCRegs_generic);

    fHAL->comment = "//";

    return 1;
} // InitHAL_generic


/*
 * FreeHAL_generic()
 */

static int FreeHAL_generic(slHAL *fHAL)
{
    return 1;
} // FreeHAL_generic


/*
 * RegisterNames_generic()
 */

static int RegisterNames_generic(slHAL *fHAL)
{
    int i, j;

    // Add atoms for connectors and connector registers.
    for (i = 0; i < NUMELS(connectors_generic); i++) {
        ConnectorDescriptor * conn = &connectors_generic[i];
        conn->name = AddAtom(atable, conn->sname);
        for (j = 0; j < conn->numregs; j++)
            conn->registers[j].name = AddAtom(atable, conn->registers[j].sname);
    }

    return 1;
}


/*
 * GetConnectorID_generic()
 */

static int GetConnectorID_generic(int name)
{
    int i;

    for (i = 0; i < NUMELS(connectors_generic); i++)
        if (name == connectors_generic[i].name)
            return connectors_generic[i].cid;

    return 0;
}


/*
 * GetConnectorAtom_generic()
 */

static int GetConnectorAtom_generic(int name)
{
    const ConnectorDescriptor *conn
        = LookupConnectorHAL(connectors_generic, name, NUMELS(connectors_generic));
    return conn ? conn->name : 0;
} // GetConnectorAtom_generic

/*
 * GetConnectorUses_generic()
 */

static int GetConnectorUses_generic(int cid, int pid)
{
    const ConnectorDescriptor *conn
        = LookupConnectorHAL(connectors_generic, cid, NUMELS(connectors_generic));
    return conn ? conn->properties : 0;
} //GetConnectorUses_generic

/*
 * GetConnectorRegister_generic()
 */

static int GetConnectorRegister_generic(int cid, int ByIndex, int ratom, Binding *fBind)
{
    int i;
    ConnectorDescriptor *conn;
    ConnectorRegisters *regs;

    conn = LookupConnectorHAL(connectors_generic, cid, NUMELS(connectors_generic));
    if (! conn)
        return 0;

    regs = conn->registers;

    if (! regs)
        return 0;
    
    for (i = 0; i < conn->numregs; i++) {
        if (ratom == regs[i].name) {
            SetSymbolConnectorBindingHAL(fBind, &regs[i]);
            return 1;
        }
    }

    return 0;
} // GetConnectorRegister_generic

/*
 * GetCapsBit_generic() - Return an integer value representing the
 * capabilities of this profile.
 */

static int GetCapsBit_generic(int bitNumber)
{
    switch (bitNumber) {
    case CAPS_INDEXED_ARRAYS:
    case CAPS_DONT_FLATTEN_IF_STATEMENTS:
        return 1;
    default:
        return 0;
    }
} // GetCapsBit_generic


/*
 * CheckInternalFunction_generic() - Check for internally implemented
 * function.
 */

static int CheckInternalFunction_generic(Symbol *fSymb, int *group)
{
    return 1;
} // CheckInternalFunction_generic

/*
 * BindUniformUnbound_generic() - Bind an unbound variable to a free
 * uniform resource.
 *
 */

static int BindUniformUnbound_generic(SourceLoc *loc, Symbol *fSymb,
                                      Binding *fBind)
{
    fBind->none.properties |= BIND_IS_BOUND;
    return 1;
} // BindUniformUnbound_generic

/*
 * BindVaryingSemantic_generic()
 */

static int BindVaryingSemantic_generic(SourceLoc *loc, Symbol *fSymb,
                                       int semantic, Binding *fBind,
                                       int IsOutVal)
{
    int ii, index, len, HasSuffix, base, IsFloating;
    SemanticsDescriptor *semantics;
    char root[128];
    const char *pname, *match;
    Type *lType;

    pname = GetAtomString(atable, semantic);
    HasSuffix = HasNumericSuffix(pname, root, 128, &index);
    semantics = Cg->theHAL->semantics;
    for (ii = 0; ii < Cg->theHAL->numSemantics; ii++, semantics++) {
        match = semantics->numregs > 0 ? root : pname;
        if (!strcmp(match, semantics->sname)) {
            if (semantics->numregs > 0) {
                if (index >= semantics->numregs) {
                    SemanticError(loc, ERROR_S_SEMANTICS_INDEX_TOO_BIG, pname);
                    return 0;
                }
            } else {
                index = 0;
            }

            // Found a match.  See if the type is compatible:

            lType = fSymb->type;
            if (IsScalar(lType)) {
                len = 1;
            } else if (IsVector(lType, &len)) {
            } else {
                SemanticError(loc, ERROR_S_SEM_VAR_NOT_SCALAR_VECTOR,
                              GetAtomString(atable, fSymb->name));
                return 0;
            }
            base = GetBase(lType);
            IsFloating = (base == TYPE_BASE_FLOAT);
            if (!IsFloating)
                return 0;

            if (semantics->properties & SEM_VARYING) {
                fBind->none.kind = BK_CONNECTOR;
                fBind->conn.rname = semantic;
                fBind->conn.regno = semantics->regno + index;
                fSymb->properties |= SYMB_IS_CONNECTOR_REGISTER;
            } else {
                fBind->none.kind = BK_SEMANTIC;
                fBind->sem.sname = semantic;
                fBind->sem.sregno = 0;
            }
            fBind->none.properties |= BIND_IS_BOUND | BIND_VARYING;
            if (semantics->properties & SEM_HIDDEN)
                fBind->conn.properties |= BIND_HIDDEN;
            fSymb->properties |= SYMB_CONNECTOR_CAN_READ; // Obsolete
            fSymb->properties |= SYMB_CONNECTOR_CAN_WRITE; // Obsolete
            if (semantics->properties & SEM_IN)
                fBind->none.properties |= BIND_INPUT;
            if (semantics->properties & SEM_OUT)
                fBind->none.properties |= BIND_OUTPUT;
            if (semantics->properties & SEM_REQUIRED)
                fBind->none.properties |= BIND_WRITE_REQUIRED;
            // fBind->none,gname set elsewhere
            // fBind->none.lname set elsewhere
            fBind->none.base = semantics->base;
            fBind->none.size = semantics->size;
            return 1;
        }
    }
    return 0;
} // BindVaryingSemantic_generic

/*
 * PrintCodeHeader_generic()
 */

static int PrintCodeHeader_generic(FILE *out)
{
    fprintf(out, "# Generic output by Cg compiler\n");
    return 1;
} // PrintCodeHeader_generic


static void PrintFunctions(Symbol *symb)
{
    Symbol *fSymb;
    if (symb) {
        PrintFunctions(symb->left);
        if (IsFunction(symb)) {
            fSymb = symb;
            while (fSymb) {
                PrintFunction(fSymb);
                BPrintFunction(fSymb);
                fSymb = fSymb->details.fun.overload;
            }
        }
        PrintFunctions(symb->right);
    }
}

/*
 * GenerateCode_generic() - Generates human-readable generic output form of
 * source code.
 */

static int GenerateCode_generic(SourceLoc *loc, Scope *fScope, Symbol *program)
{
    PrintFunctions(fScope->symbols);
    return 1;
} // GenerateCode_generic

///////////////////////////////////////////////////////////////////////////////
//////////////////////// End of generic_hal.c /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
