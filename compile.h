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
// compile.h
//

#if !defined(__COMPILE_H)
#define __COMPILE_H 1

int InitCgStruct(void);

typedef struct Options_Rec{
    const char *profileString;
    const char *entryName;
    const char *sourceFileName;
    const char *outputFileName;
    const char *listFileName;
    FILE *outfd;
    FILE *listfd;
    int OutputFileOpen;
    int ListFileOpen;
    int DebugMode;
    int ErrorMode;
    int NoCodeGen;
    int NoWarnings;
    int Quiet;
    int PrintVersion;
    int NoStdlib;
    int AllowLongPrograms;
    int PositionInvariant;
    // Tools:
    int Tokenize;
    // Debug The Compiler options:
    int DumpAtomTable;
    int TraceScanner;
    int DumpParseTree;
    int DumpFinalTree;
    int DumpNodeTree;
    int TrapOnError;
    int Comments;
} Options;

struct CgStruct_Rec {
    // Public members
    SourceLoc *pLastSourceLoc;  // Set at the start of each statement by the tree walkers
    int DebugLevel;             // Code generator debug level
    Options options;            // Compile options and parameters
    BindingTree *bindings;      // #pragma bind bindings
    slProfile *allProfiles;     // List of supported profiles
    slHAL *theHAL;              // Current profile's HAL

    // Private members
    SourceLoc lastSourceLoc;

    // Scanner data:

    SourceLoc *tokenLoc;        // Source location of most recent token seen by the scanner
    int mostRecentToken;        // Most recent token seen by the scanner
    InputSrc *currentInput;


    // Private members:
    SourceLoc ltokenLoc;
    int errorCount;
    int warningCount;
    int lineCount;
    int AllowSemanticParseErrors;// Allow exactly one after each parse error.

};

int OpenOutputFile(void);
int OpenListFile(void);
void PrintOptions(int argc, char **argv);
int CloseOutputFiles(const char *mess);

int CompileProgram(CgStruct *Cg, SourceLoc *loc, Scope *fScope);
void SetSymbolFlagsList(Scope *fScope, int fVal);
void SetSymbolFlags(Symbol *fSymb, int fVal);
void OutputBindings(FILE *out, slHAL *fHAL, Symbol *program);

int HasNumericSuffix(const char *fStr, char *root, int size, int *suffix);

int GetNumberedAtom(const char *root, int number, int digits, char ch);
stmt *ConcatStmts(stmt *first, stmt *last);
void AppendStatements(StmtList *fStatements, stmt *fStmt);
expr *GenSymb(Symbol *fSymb);
expr *GenMember(Symbol *fSymb);
expr *GenMemberSelector(expr *sexpr, expr *mExpr);
expr *GenMemberReference(expr *sexpr, Symbol *mSymb);
expr *GenVecIndex(expr *vexpr, expr *xexpr, int base, int len, int len2);
expr *GenMatIndex(expr *mExpr, expr *xexpr, int base, int len, int len2);
expr *GenBoolConst(int fval);
expr *GenIntConst(int fval);
expr *GenFConstV(float *fval, int len, int base);
expr *GenConvertVectorLength(expr *fExpr, int base, int len, int newlen);
expr *DupExpr(expr *fExpr);

#endif // !defined(__COMPILE_H)
