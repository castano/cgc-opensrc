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
// scanner.h
//

#if !defined(__SCANNER_H)
#define __SCANNER_H 1

#define MAX_SYMBOL_NAME_LEN 128
#define MAX_STRING_LEN 512

// Not really atom table stuff but needed first...

typedef struct SourceLoc_Rec {
    unsigned short file, line;
} SourceLoc;

int yyparse (void);

void yyerror(const char *s);
int yylex(void);

typedef struct InputSrc {
    struct InputSrc	*prev;
    int			(*scan)(struct InputSrc *);
    int			(*getch)(struct InputSrc *);
    void		(*ungetch)(struct InputSrc *, int);
    int			name;  /* atom */
    int			line;
} InputSrc;

int InitScanner(CgStruct *Cg);
int FreeScanner(CgStruct *Cg);
int SetInputFile(const char *fname);
int ScanFromString(char *);
int scan_include_name(void);

void SemanticParseError(SourceLoc *loc, int num, const char *mess, ...);
void SemanticError(SourceLoc *loc, int num, const char *mess, ...);
void InternalError(SourceLoc *loc, int num, const char *mess, ...);
void SemanticWarning(SourceLoc *loc, int num, const char *mess, ...);
void InformationalNotice(SourceLoc *loc, int num, const char *mess, ...);
void FatalError(const char *mess, ...);

int GetErrorCount(void);

#endif // !(defined(__SCANNER_H)

