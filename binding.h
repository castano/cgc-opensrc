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
// binding.h
//

#if !defined(__BINDING_H)
#define __BINDING_H 1

typedef enum BindingKinds_Enum {
    BK_NONE, BK_CONNECTOR, BK_TEXUNIT, BK_REGARRAY, BK_CONSTANT, BK_DEFAULT, BK_SEMANTIC, 
} BindingKinds;

typedef union Binding_Rec Binding;
typedef struct BindingList_Rec BindingList;
typedef struct BindingTree_Rec BindingTree;

// Properties bits:

#define BIND_IS_BOUND           0x0001
#define BIND_HIDDEN             0x0002
#define BIND_UNIFORM            0x0004
#define BIND_VARYING            0x0008
#define BIND_INPUT              0x0010
#define BIND_OUTPUT             0x0020
#define BIND_WRITE_REQUIRED     0x0040
#define BIND_WAS_WRITTEN        0x0080

typedef struct BindingNone_Rec {
    int properties;     // Properties
    int gname;          // Global name
    int lname;          // Local name
    int base;           // type base
    int size;           // num of elements
    int kind;
} BindingNone;

typedef struct BindingConnector_Rec {
    int properties;     // Properties
    int gname;          // Connector name
    int lname;          // Member name
    int base;           // type base
    int size;           // num of elements
    int kind;
    // Private members:
    int rname;          // HW Register name
    int regno;          // Profile specific HW register number
} BindingConnector;

typedef struct BindingRegArray_Rec {
    int properties;     // Properties
    int gname;          // Program name
    int lname;          // Argument name
    int base;           // type base
    int size;           // num of elements
    int kind;
    // Private members:
    int rname;          // HW Register name
    int regno;          // Profile specific HW register number
    int count;          // Number of registers allocated
} BindingRegArray;

typedef struct BindingTexunit_Rec {
    int properties;     // Properties
    int gname;          // Program name
    int lname;          // Argument name
    int base;           // type base
    int size;           // num of elements
    int kind;
    // Private members:
    int unitno;         // Texture unit number
} BindingTexunit;

typedef struct BindingConstDefault_Rec {
    int properties;     // Properties
    int gname;          // Program name
    int lname;          // Argument name
    int base;           // type base
    int size;           // num of elements
    int kind;
    // Private members:
    int rname;          // HW Register name
    int regno;          // Profile specific HW register number
    float val[4];       // Values
} BindingConstDefault;

typedef struct BindingSemantic_Rec {
    int properties;     // Properties
    int gname;          // Global name
    int lname;          // Local name
    int base;           // type base
    int size;           // num of elements
    int kind;
    // Private members:
    int sname;          // Semantic name or variable name if no semantic
    int sregno;         // Profile specific HW register number
} BindingSemantic;

union Binding_Rec {
    BindingNone none;
    BindingConnector conn;
    BindingRegArray reg;
    BindingTexunit texunit;
    BindingConstDefault constdef;
    BindingSemantic sem;
};

struct BindingList_Rec {
    BindingList *next;
    Binding *binding;
};

struct BindingTree_Rec {
    BindingTree *nextc; // Next connector name in a list
    BindingTree *nextm; // Next member in this connector
    SourceLoc loc;      // Source location of #pragma bind
    Binding binding;    // Binding info.
};

typedef struct UniformSemantic_Rec {
    struct UniformSemantic_Rec *next;
    int gname;
    int vname;
    int semantic;
} UniformSemantic;

Binding *NewBinding(int gname, int sname);
Binding *NewConstDefaultBinding(int gname, int sname, int count, int rname, int regno,
                                float *fval);

BindingTree *LookupBinding(int gname, int lname);

UniformSemantic *NewUniformSemantic(int gname, int vname, int semantic);

void DefineConnectorBinding(SourceLoc *loc, int cname, int mname, int rname);
void DefineRegArrayBinding(SourceLoc *loc, int pname, int aname, int rname, int regno,
                           int count);
void DefineTexunitBinding(SourceLoc *loc, int pname, int aname, int unitno);
void DefineConstantBinding(SourceLoc *loc, int pname, int aname, int count, float *fval);
void DefineDefaultBinding(SourceLoc *loc, int pname, int aname, int count, float *fval);

#endif // !defined(__BINDING_H)

