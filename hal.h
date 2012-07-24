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
// hal.h
//

#if !defined(__HAL_H)
#define __HAL_H 1

// Typedefs for things defined here in "hal.h":

typedef struct slHAL_Rec slHAL;
typedef struct slProfile_Rec slProfile;

// Profile and connector IDs for non-programs and non-connectors:

#define PID_NONE_ID    0
#define CID_NONE_ID    0
#define CID_INVALID_ID 1 // Marks connector as invalid to prevent multiple errors

// Connector capabilities bits returned by GetConnectorUses:

#define CONNECTOR_IS_USELESS  0x0000
#define CONNECTOR_IS_INPUT    0x0001
#define CONNECTOR_IS_OUTPUT   0x0002

#define REG_NONE            0x0000
#define REG_ALLOC           0x0001
#define REG_RESERVED        0x0002
#define REG_HIDDEN          0x0004
#define REG_WRITE_REQUIRED  0x0008
#define REG_INPUT           0x0010
#define REG_OUTPUT          0x0020

#define CAPS_INLINE_ALL_FUNCTIONS       1
#define CAPS_GLOBAL_DEBUG_VEC4          2
#define CAPS_RESTRICT_RETURNS           3
#define CAPS_DECONSTRUCT_MATRICES       4
#define CAPS_LATE_BINDINGS              5
#define CAPS_INDEXED_ARRAYS             6
#define CAPS_DONT_FLATTEN_IF_STATEMENTS 7

struct slProfile_Rec {
    slProfile *next;
    int (*InitHAL)(slHAL *);
    int name;
    int id;
};

// Hal version of connector register description:

typedef struct ConnectorRegisters_Rec {
    const char *sname;
    int name;    // atom
    int base;
    int regno;
    int size;
    int properties;
} ConnectorRegisters;

typedef struct ConnectorDescriptor_Rec {
    const char *sname;
    int name;   // atom
    int cid;
    int properties;
    int numregs;
    ConnectorRegisters *registers;
} ConnectorDescriptor;

// Hal version of "semantics"  descriptions:

enum SemanticProperties {
    SEM_IN = 1, SEM_OUT = 2, SEM_UNIFORM = 4, SEM_VARYING = 8, SEM_HIDDEN = 16,
    SEM_EXCLUSIVE = 32, SEM_REQUIRED = 64
};

typedef struct SemanticsDescriptor_Rec {
    const char *sname;
    int base;
    int size;
    int regno;
    int numregs;
    int reggroup;
    int properties;
} SemanticsDescriptor;


struct slHAL_Rec {

    // Function members:

    int (*InitHAL)(slHAL *);
    int (*FreeHAL)(slHAL *);
    int (*RegisterNames)(slHAL *);
    int (*GetCapsBit)(int bitNumber);
    int (*GetConnectorID)(int);
    int (*GetConnectorAtom)(int);
    int (*GetConnectorUses)(int, int);
    int (*GetConnectorRegister)(int cid, int ByIndex, int ratom, Binding *fBind);
    int (*GetFloatSuffixBase)(SourceLoc *loc, int suffix);
    int (*GetSizeof)(Type *fType);
    int (*GetAlignment)(Type *fType);
    int (*CheckDeclarators)(SourceLoc *loc, const dtype *fDtype);
    int (*CheckDefinition)(SourceLoc *loc, int name, const Type *fType);
    int (*CheckStatement)(SourceLoc *loc, stmt *fstmt);
    int (*CheckInternalFunction)(Symbol *fSymb, int *group);
    int (*IsNumericBase)(int fBase);
    int (*IsIntegralBase)(int fBase);
    int (*IsTexobjBase)(int fBase);
    int (*IsValidRuntimeBase)(int fBase);
    int (*IsValidScalarCast)(int toBase, int fromBase, int Explicit);
    int (*IsValidOperator)(SourceLoc *loc, int name, int op, int subop);
    int (*GetBinOpBase)(int lop, int lbase, int rbase, int llen, int rlen);
    int (*ConvertConstant)(const scalar_constant *fval, int fbase, int tbase, expr **fexpr);
    int (*BindUniformUnbound)(SourceLoc *loc, Symbol *fSymb, Binding *lBind);
    int (*BindUniformPragma)(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                        const Binding *fBind);
    int (*BindVaryingSemantic)(SourceLoc *loc, Symbol *fSymb, int semantic,
                        Binding *fBind, int IsOutVal);
    int (*BindVaryingPragma)(SourceLoc *loc, Symbol *fSymb, Binding *lBind,
                        const Binding *fBind, int IsOutVal);
    int (*BindVaryingUnbound)(SourceLoc *loc, Symbol *fSymb, int name, int semantic,
                        Binding *fBind, int IsOutVal);
    int (*PrintCodeHeader)(FILE *out);
    int (*GenerateCode)(SourceLoc *loc, Scope *fScope, Symbol *program);

    // Profile specific data members:

    const char *vendor;
    const char *version;

    int profileName;
    int pid;
    int entryName;

    SemanticsDescriptor *semantics;
    int numSemantics;

    int incid;
    ConnectorRegisters *inputCRegs;
    int numInputCRegs;

    int nextUnboundVinReg;
    int lastUnboundVinReg;

    int outcid;
    ConnectorRegisters *outputCRegs;
    int numOutputCRegs;

    int nextUnboundVoutReg;
    int lastUnboundVoutReg;

    // Program specific data:

    Scope *globalScope;
    Symbol *varyingIn;
    Symbol *varyingOut;
    SymbolList *uniformParam;
    SymbolList *uniformGlobal;
    UniformSemantic *uniforms;

    BindingList *constantBindings;
    BindingList *defaultBindings;

    // Misc data:

    const char *comment;
    void *localData;            // Pointer to profile specific data
};

slProfile *RegisterProfile(int (*InitHAL)(slHAL *), const char *name, int id);
slProfile *EnumerateProfiles(int index);

int InitHAL(const char *profileName, const char *entryName);
ConnectorDescriptor *LookupConnectorHAL(ConnectorDescriptor *connectors, int cid, int num);
void SetSymbolConnectorBindingHAL(Binding *fBind, ConnectorRegisters *fConn);

void AddConstantBinding(Binding *fBind);
void AddDefaultBinding(Binding *fBind);

#endif // !defined(__HAL_H)
