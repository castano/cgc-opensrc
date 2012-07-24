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
// tokenize.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "slglobals.h"

int main(int ac, char **av) {
    InitCgStruct();
    Cg->options.Quiet = 1;
    if (!InitAtomTable(atable, 0)) {
        printf(OPENSL_TAG ": atom table initialization failed.\n");
        return 1;
    }
    if (!InitScanner(Cg))
        return 1;
    if (ac != 2 || !SetInputFile(av[1])) {
        fprintf(stderr, "usage: %s filename\n", av[0]);
        return 1;
    }
    Cg->options.sourceFileName = av[1];
    TokenizeInput();
    return 0;
}

/* Various stuff that isn't used, but is needed to make things link */
YYSTYPE yylval;
int InitCPP() { return 1; }
int FinalCPP() { return 1; }
int CloseOutputFiles(const char *mess) { assert(0); return 0; }
int IsTypedef(const Symbol *s) { assert(0); return 0; }
Symbol *LookUpSymbol(Scope *scope, int atom) { assert(0); return 0; }
int MacroExpand(int atom) { assert(0); return 0; }
void readCPPline() { assert(0); }
void MarkErrorPosHit(SourceLoc *loc) { assert(0); }
Scope *CurrentScope = 0;
void *mem_Alloc(MemoryPool *pool, size_t size) { assert(0); return 0; }
