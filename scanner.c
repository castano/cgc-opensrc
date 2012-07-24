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
// scanner.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if 0
#include <ieeefp.h>
#else
#define isinff(x) (((*(long *)&(x) & 0x7f800000L)==0x7f800000L) && \
                   ((*(long *)&(x) & 0x007fffffL)==0000000000L))
#endif

#include "slglobals.h"

const char *Build_Date = __DATE__;
const char *Build_Time = __TIME__;

typedef struct FileInputSrc {
    InputSrc            base;
    FILE                *fd;
    char                save_cnt;
    char                save[3];
} FileInputSrc;

typedef struct StringInputSrc {
    InputSrc base;
    char *p;
} StringInputSrc;

static int eof_scan(InputSrc *is)
{
    return EOF;
} // eof_scan

static void noop(InputSrc *in, int ch) {}

static InputSrc eof_inputsrc = { 0, &eof_scan, &eof_scan, &noop };

static int byte_scan(InputSrc *);
static int nextchar(FileInputSrc *);
static void ungetchar(FileInputSrc *, int);

#define EOL_SY '\n'

#if defined(_WIN32)
#define DBG_BREAKPOINT() __asm int 3
#else
#define DBG_BREAKPOINT()
#endif

#if defined(_WIN32)
__int64 RDTSC ( void ) {

    __int64 v;

    __asm __emit 0x0f
    __asm __emit 0x31
    __asm mov dword ptr v, eax
    __asm mov dword ptr v+4, edx

    return v;
}
#endif

int InitScanner(CgStruct *Cg)
{
    // Add various atoms needed by the CPP line scanner:
    if (!InitCPP())
        return 0;

    Cg->mostRecentToken = 0;
    Cg->tokenLoc = &Cg->ltokenLoc;

    Cg->ltokenLoc.file = 0;
    Cg->ltokenLoc.line = 0;
    Cg->errorCount = 0;
    Cg->warningCount = 0;
    Cg->lineCount = 0;
    Cg->AllowSemanticParseErrors = 0;

    Cg->currentInput = &eof_inputsrc;

    return 1;
} // InitScanner

int SetInputFile(const char *fname)
{
    FileInputSrc *in = malloc(sizeof(FileInputSrc));
    memset(in, 0, sizeof(FileInputSrc));
    if (fname) {
        if (!Cg->options.Quiet)
            printf("%s\n", fname);
        in->base.name = LookUpAddString(atable, fname);
        in->fd = fopen(fname, "r");
        if (!in->fd) {
            printf(OPENSL_TAG ": cannot open input file \"%s\"\n", fname);
            free(in);
            return 0;
        }
    } else {
        in->fd = stdin;
        in->base.name = LookUpAddString(atable, "<stdin>");
    }
    in->base.line = 1;
    in->base.scan = byte_scan;
    in->base.getch = (int (*)(InputSrc *)) nextchar;
    in->base.ungetch = (void (*)(InputSrc *, int)) ungetchar;
    in->base.prev = Cg->currentInput;
    Cg->currentInput = &in->base;
#if 0 && !defined(_DEBUG)
    if (Cg->options.TraceScanner) {
        __int64 s,e;
        s = RDTSC();
        while (Cg->currentInput->scan(Cg->currentInput) > 0)
            /* empty statement */ ;
        e = RDTSC();
        printf("%d cycles\n", (int)(e-s));
        return 0;
    }
#endif
    return 1;
} // SetInputFile

static int str_getch(StringInputSrc *in)
{
    if (*in->p)
        return *in->p++;
    Cg->currentInput = in->base.prev;
    free(in);
    return ' ';
} // str_getch

static void str_ungetch(StringInputSrc *in, int ch) {
    if (in->p[-1] == ch) in->p--;
} // str_ungetch

int ScanFromString(char *s)
{
    StringInputSrc *in = malloc(sizeof(StringInputSrc));
    memset(in, 0, sizeof(StringInputSrc));
    in->p = s;
    in->base.line = 1;
    in->base.scan = byte_scan;
    in->base.getch = (int (*)(InputSrc *))str_getch;
    in->base.ungetch = (void (*)(InputSrc *, int))str_ungetch;
    in->base.prev = Cg->currentInput;
    Cg->currentInput = &in->base;
    return 1;
} // ScanFromString;

int FreeScanner(CgStruct *Cg)
{
    FinalCPP();
    if (Cg->warningCount || Cg->errorCount || !Cg->options.Quiet) {
        fprintf(Cg->options.listfd, "%d lines", Cg->lineCount);
        if (Cg->warningCount)
            fprintf(Cg->options.listfd, ", %d warnings", Cg->warningCount);
        fprintf(Cg->options.listfd, ", %d errors.\n", Cg->errorCount);
    }
    return Cg->errorCount;
} // FreeScanner

int GetErrorCount(void)
{
    return Cg->errorCount;
} // GetErrorCount

/*
 * bumpErrorCount() - Useful for setting breakpoints when debugging.
 *
 */

void bumpErrorCount(void)
{
    Cg->errorCount++;
    if (Cg->options.TrapOnError) {
        DBG_BREAKPOINT();
    }
} // bumbErrorCount

/*
 * bumpWarningCount() - Useful for setting breakpoints when debugging.
 *
 */

void bumpWarningCount(void)
{
    Cg->warningCount++;
    if (Cg->options.TrapOnError) {
        DBG_BREAKPOINT();
    }
} // bumpWarningCount

// Called by yyparse on an error:

void yyerror(const char *s)
{
    if (!Cg->options.ErrorMode) {
        if (Cg->ltokenLoc.file) {
            fprintf(Cg->options.listfd, "%s(%d) : error C0000: ",
                    GetAtomString(atable, Cg->ltokenLoc.file), Cg->ltokenLoc.line);
        } else {
            fprintf(Cg->options.listfd, "(%d) : error C0000: ", Cg->currentInput->line);
        }
        fprintf(Cg->options.listfd, "%s at token \"%s\"\n", s,
                GetAtomString(atable, Cg->mostRecentToken));
        Cg->AllowSemanticParseErrors = 1;
    }
    bumpErrorCount();
} // yyerror

/*
 * SemanticParseError() - Compiler generated semantic error inside an error rule.
 *
 */

void SemanticParseError(SourceLoc *loc, int num, const char *mess, ...)
{
    va_list args;

    if (Cg->AllowSemanticParseErrors) {
        if (!Cg->options.ErrorMode) {
            if (loc->file) {
                fprintf(Cg->options.listfd, "%s(%d) : error C%04d: ",
                        GetAtomString(atable, loc->file), loc->line, num);
            } else {
                fprintf(Cg->options.listfd, "(%d) : error C%04d: ", loc->line, num);
            }
            va_start(args, mess);
            vfprintf(Cg->options.listfd, mess, args);
            va_end(args);
            fprintf(Cg->options.listfd, "\n");
            bumpErrorCount();
        } else {
            MarkErrorPosHit(loc);
        }

        Cg->AllowSemanticParseErrors = 0;
    }
} // SemanticParseError

/*
 * SemanticError() - Compiler generated semantic error.
 *
 */

void SemanticError(SourceLoc *loc, int num, const char *mess, ...)
{
    va_list args;

    if (!Cg->options.ErrorMode) {
        if (loc->file) {
            fprintf(Cg->options.listfd, "%s(%d) : error C%04d: ",
                    GetAtomString(atable, loc->file), loc->line, num);
        } else {
            fprintf(Cg->options.listfd, "(%d) : error C%04d: ", loc->line, num);
        }
        va_start(args, mess);
        vfprintf(Cg->options.listfd, mess, args);
        va_end(args);
        fprintf(Cg->options.listfd, "\n");
        bumpErrorCount();
    } else {
        MarkErrorPosHit(loc);
    }
} // SemanticError

/*
 * InternalError() - Internal compiler error.
 *
 */

void InternalError(SourceLoc *loc, int num, const char *mess, ...)
{
    va_list args;

    if (loc->file) {
        fprintf(Cg->options.listfd, "%s(%d) : error C%04d: ",
                GetAtomString(atable, loc->file), loc->line, num);
    } else {
        fprintf(Cg->options.listfd, "(%d) : error C%04d: ", loc->line, num);
    }
    va_start(args, mess);
    vfprintf(Cg->options.listfd, mess, args);
    va_end(args);
    fprintf(Cg->options.listfd, "\n");
    bumpErrorCount();
} // InternalError

/*
 * FatalError() - Fatal internal compiler error.
 *
 */

void FatalError(const char *mess, ...)
{
    va_list args;

    fprintf(Cg->options.listfd, "(%d) : fatal error C9999: ", Cg->lineCount);
    va_start(args, mess);
    vfprintf(Cg->options.listfd, mess, args);
    va_end(args);
    fprintf(Cg->options.listfd, "\n");
    bumpErrorCount();
    CloseOutputFiles("Compilation terminated due to fatal error");
    exit(9999);
} // InternalError

/*
 * SemanticWarning() - Compiler generated semantic warning.
 *
 */

void SemanticWarning(SourceLoc *loc, int num, const char *mess, ...)
{
    va_list args;

    if (!Cg->options.NoWarnings) {
        if (!Cg->options.ErrorMode) {
            if (loc->file) {
                fprintf(Cg->options.listfd, "%s(%d) : warning C%04d: ",
                        GetAtomString(atable, loc->file), loc->line, num);
            } else {
                fprintf(Cg->options.listfd, "(%d) : warning C%04d: ", loc->line, num);
            }
            va_start(args, mess);
            vfprintf(Cg->options.listfd, mess, args);
            va_end(args);
            fprintf(Cg->options.listfd, "\n");
        }
        bumpWarningCount();
    }
} // SemanticWarning

/*
 * InformationalNotice() - Print a message from the compiler.
 *
 */

void InformationalNotice(SourceLoc *loc, int num, const char *mess, ...)
{
    va_list args;

    if (!Cg->options.NoWarnings) {
        if (!Cg->options.ErrorMode) {
            if (loc->file) {
                fprintf(Cg->options.listfd, "%s(%d) : notice C%04d: ",
                        GetAtomString(atable, loc->file), loc->line, num);
            } else {
                fprintf(Cg->options.listfd, "(%d) : notice C%04d: ", loc->line, num);
            }
            va_start(args, mess);
            vfprintf(Cg->options.listfd, mess, args);
            va_end(args);
            fprintf(Cg->options.listfd, "\n");
        }
    }
} // InformationalNotice

// The scanner:

static int nextchar(FileInputSrc *in)
{
    int ch;

    if (in->save_cnt) {
        ch = in->save[--in->save_cnt];
    } else {
        ch = getc(in->fd);
        if (ch == EOF) {
            Cg->currentInput = in->base.prev;
            fclose(in->fd);
            free(in);
            return '\n';
        }
    }
    if (ch == '\n') {
        Cg->lineCount++;
        in->base.line++;
    }

    return ch;
} // nextchar

static void ungetchar(FileInputSrc *in, int ch)
{
    if (&in->base == Cg->currentInput) {
        if (in->save_cnt < sizeof(in->save))
            in->save[in->save_cnt++] = ch;
        if (ch == '\n') {
            in->base.line--;
            Cg->lineCount--;
        }
    }

} // ungetchar

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// Floating point constants: /////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * lBuildFloatValue() - Quick and dirty conversion to floating point.  Since all
 *         we need is single precision this should be quite precise.
 */

static float lBuildFloatValue(const char *str, int len, int exp)
{
    double val, expval, ten;
    int ii, llen, absexp;
    float rv;

    val = 0.0;
    llen = len;
    for (ii = 0; ii < len; ii++)
        val = val*10.0 + (str[ii] - '0');
    if (exp != 0) {
        absexp = exp > 0 ? exp : -exp;
        expval = 1.0f;
        ten = 10.0;
        while (absexp) {
            if (absexp & 1)
                expval *= ten;
            ten *= ten;
            absexp >>= 1;
        }
        if (exp >= 0) {
            val *= expval;
        } else {
            val /= expval;
        }
    }
    rv = (float)val;
    if (isinff(rv)) {
        SemanticError(Cg->tokenLoc, ERROR___FP_CONST_OVERFLOW);
    }
    return rv;
} // lBuildFloatValue

/*
 * lFloatConst() - Scan a floating point constant.  Assumes that the scanner
 *         has seen at least one digit, followed by either a decimal '.' or the
 *         letter 'e'.
 */

static int lFloatConst(char *str, int len, int ch)
{
    int HasDecimal, declen, exp, ExpSign;
    float lval;

    HasDecimal = 0;
    declen = 0;
    exp = 0;
    if (ch == '.') {
        HasDecimal = 1;
        ch = Cg->currentInput->getch(Cg->currentInput);
        while (ch >= '0' && ch <= '9') {
            if (len < MAX_SYMBOL_NAME_LEN) {
                declen++;
                if (len > 0 || ch != '0') {
                    str[len] = ch;
                    len++;
                }
                ch = Cg->currentInput->getch(Cg->currentInput);
            } else {
                SemanticError(Cg->tokenLoc, ERROR___FP_CONST_TOO_LONG);
                len = 1;
            }
        }
    }

    // Exponent:

    if (ch == 'e' || ch == 'E') {
        ExpSign = 1;
        ch = Cg->currentInput->getch(Cg->currentInput);
        if (ch == '+') {
            ch = Cg->currentInput->getch(Cg->currentInput);
        } else if (ch == '-') {
            ExpSign = -1;
            ch = Cg->currentInput->getch(Cg->currentInput);
        }
        if (ch >= '0' && ch <= '9') {
            while (ch >= '0' && ch <= '9') {
                exp = exp*10 + ch - '0';
                ch = Cg->currentInput->getch(Cg->currentInput);
            }
        } else {
            SemanticError(Cg->tokenLoc, ERROR___ERROR_IN_EXPONENT);
        }
        exp *= ExpSign;
    }

    if (len == 0) {
        lval = 0.0f;
    } else {
        lval = lBuildFloatValue(str, len, exp - declen);
    }

    // Suffix:

    yylval.sc_fval = lval;
    if (ch == 'h') {
        return FLOATHCONST_SY;
    } else {
        if (ch == 'x') {
            return FLOATXCONST_SY;
        } else {
            if (ch == 'f') {
                return FLOATCONST_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return CFLOATCONST_SY;
            }
        }
    }
} // lFloatConst

///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////// Normal Scanner //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

static int byte_scan(InputSrc *in)
{
    char symbol_name[MAX_SYMBOL_NAME_LEN + 1];
    char string_val[MAX_STRING_LEN + 1];
    int AlreadyComplained;
    int len, ch, ii, ival;

    for (;;) {
        yylval.sc_int = 0;
        ch = Cg->currentInput->getch(Cg->currentInput);
        while (ch == ' ' || ch == '\t' || ch == '\r') {
            yylval.sc_int = 1;
            ch = Cg->currentInput->getch(Cg->currentInput);
        }
        Cg->ltokenLoc.file = Cg->currentInput->name;
        Cg->ltokenLoc.line = Cg->currentInput->line;
        switch (ch) {
        default:
            return ch; // Single character token
        case EOF:
            return 0;
        case 'A': case 'B': case 'C': case 'D': case 'E':
        case 'F': case 'G': case 'H': case 'I': case 'J':
        case 'K': case 'L': case 'M': case 'N': case 'O':
        case 'P': case 'Q': case 'R': case 'S': case 'T':
        case 'U': case 'V': case 'W': case 'X': case 'Y':
        case 'Z': case '_':
        case 'a': case 'b': case 'c': case 'd': case 'e':
        case 'f': case 'g': case 'h': case 'i': case 'j':
        case 'k': case 'l': case 'm': case 'n': case 'o':
        case 'p': case 'q': case 'r': case 's': case 't':
        case 'u': case 'v': case 'w': case 'x': case 'y':
        case 'z':
            len = 0;
            do {
                if (len < MAX_SYMBOL_NAME_LEN) {
                    symbol_name[len] = ch;
                    len++;
                    ch = Cg->currentInput->getch(Cg->currentInput);
                }
            } while ((ch >= 'a' && ch <= 'z') ||
                     (ch >= 'A' && ch <= 'Z') ||
                     (ch >= '0' && ch <= '9') ||
                     ch == '_');
            symbol_name[len] = '\0';
            Cg->currentInput->ungetch(Cg->currentInput, ch);
            yylval.sc_ident = LookUpAddString(atable, symbol_name);
            return IDENT_SY;
            break;
        case '0':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == 'x' || ch == 'X') {
                ch = Cg->currentInput->getch(Cg->currentInput);
                if ((ch >= '0' && ch <= '9') ||
                    (ch >= 'A' && ch <= 'F') ||
                    (ch >= 'a' && ch <= 'f'))
                {
                    AlreadyComplained = 0;
                    ival = 0;
                    do {
                        if (ival <= 0x0fffffff) {
                            if (ch >= '0' && ch <= '9') {
                                ii = ch - '0';
                            } else if (ch >= 'A' && ch <= 'F') {
                                ii = ch - 'A' + 10;
                            } else {
                                ii = ch - 'a' + 10;
                            }
                            ival = (ival << 4) | ii;
                        } else {
                            if (!AlreadyComplained)
                                SemanticError(Cg->tokenLoc, ERROR___HEX_CONST_OVERFLOW);
                            AlreadyComplained = 1;
                        }
                        ch = Cg->currentInput->getch(Cg->currentInput);
                    } while ((ch >= '0' && ch <= '9') ||
                             (ch >= 'A' && ch <= 'F') ||
                             (ch >= 'a' && ch <= 'f'));
                } else {
                    SemanticError(Cg->tokenLoc, ERROR___ERROR_IN_HEX_CONSTANT);
                }
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                yylval.sc_int = ival;
                return INTCONST_SY;
            } else if (ch >= '0' && ch <= '7') { // octal integer constants
                AlreadyComplained = 0;
                ival = 0;
                do {
                    if (ival <= 0x1fffffff) {
                        ii = ch - '0';
                        ival = (ival << 3) | ii;
                    } else {
                        if (!AlreadyComplained)
                            SemanticError(Cg->tokenLoc, ERROR___OCT_CONST_OVERFLOW);
                        AlreadyComplained = 1;
                    }
                    ch = Cg->currentInput->getch(Cg->currentInput);
                } while (ch >= '0' && ch <= '7');
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                yylval.sc_int = ival;
                return INTCONST_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                ch = '0';
            }
            // Fall through...
        case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
            len = 0;
            do {
                if (len < MAX_SYMBOL_NAME_LEN) {
                    if (len > 0 || ch != '0') {
                        symbol_name[len] = ch;
                        len++;
                    }
                    ch = Cg->currentInput->getch(Cg->currentInput);
                }
            } while (ch >= '0' && ch <= '9');
            if (ch == '.' || ch == 'e' || ch == 'f' || ch == 'h' || ch == 'x') {
                return lFloatConst(symbol_name, len, ch);
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                ival = 0;
                AlreadyComplained = 0;
                for (ii = 0; ii < len; ii++) {
                    ch = symbol_name[ii] - '0';
                    if (ival > 214748364 || ival == 214748364 && ch >= 8) {
                        if (!AlreadyComplained)
                            SemanticError(Cg->tokenLoc, ERROR___INTEGER_CONST_OVERFLOW);
                        AlreadyComplained = 1;
                    }
                    ival = ival*10 + ch;
                }
                yylval.sc_int = ival;
                return INTCONST_SY;
            }
            break;
        case '-':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '-') {
                return MINUSMINUS_SY;
            } else if (ch == '=') {
                return ASSIGNMINUS_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '-';
            }
        case '+':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '+') {
                return PLUSPLUS_SY;
            } else if (ch == '=') {
                return ASSIGNPLUS_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '+';
            }
        case '*':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '=') {
                return ASSIGNSTAR_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '*';
            }
        case '%':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '=') {
                return ASSIGNMOD_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '%';
            }
        case ':':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == ':') {
                return COLONCOLON_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return ':';
            }
        case '=':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '=') {
                return EQ_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '=';
            }
        case '!':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '=') {
                return NE_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '!';
            }
        case '|':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '|') {
                return OR_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '|';
            }
        case '&':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '&') {
                return AND_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '&';
            }
        case '<':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '<') {
                return LL_SY;
            } else {
                if (ch == '=') {
                    return LE_SY;
                } else {
                    Cg->currentInput->ungetch(Cg->currentInput, ch);
                    return '<';
                }
            }
        case '>':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '>') {
                return GG_SY;
            } else {
                if (ch == '=') {
                    return GE_SY;
                } else {
                    Cg->currentInput->ungetch(Cg->currentInput, ch);
                    return '>';
                }
            }
        case '.':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch >= '0' && ch <= '9') {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return lFloatConst(symbol_name, 0, '.');
            } else {
                if (ch == '.') {
                    return -1; // Special EOF hack
                } else {
                    Cg->currentInput->ungetch(Cg->currentInput, ch);
                    return '.';
                }
            }
        case '/':
            ch = Cg->currentInput->getch(Cg->currentInput);
            if (ch == '/') {
                do {
                    ch = Cg->currentInput->getch(Cg->currentInput);
                } while (ch != '\n' && ch != EOF);
                if (ch == EOF)
                    return -1;
                return '\n';
            } else if (ch == '*') {
                int nlcount = 0;
                ch = Cg->currentInput->getch(Cg->currentInput);
                do {
                    while (ch != '*') {
                        if (ch == '\n') nlcount++;
                        if (ch == EOF) {
                            SemanticError(Cg->tokenLoc, ERROR___EOF_IN_COMMENT);
                            return -1;
                        }
                        ch = Cg->currentInput->getch(Cg->currentInput);
                    }
                    ch = Cg->currentInput->getch(Cg->currentInput);
                    if (ch == EOF) {
                        SemanticError(Cg->tokenLoc, ERROR___EOF_IN_COMMENT);
                        return -1;
                    }
                } while (ch != '/');
                if (nlcount) {
                    return '\n';
                }
                // Go try it again...
            } else if (ch == '=') {
                return ASSIGNSLASH_SY;
            } else {
                Cg->currentInput->ungetch(Cg->currentInput, ch);
                return '/';
            }
            break;
        case '"':
            len = 0;
            ch = Cg->currentInput->getch(Cg->currentInput);
            while (ch != '"' && ch != '\n' && ch != EOF) {
                if (ch == '\\') {
                    ch = Cg->currentInput->getch(Cg->currentInput);
                    if (ch == '\n' || ch == EOF) {
                        break;
                    }
                }
                if (len < MAX_STRING_LEN) {
                    string_val[len] = ch;
                    len++;
                    ch = Cg->currentInput->getch(Cg->currentInput);
                }
            };
            string_val[len] = '\0';
            if (ch == '"') {
                yylval.sc_ident = LookUpAddString(atable, string_val);
                return STRCONST_SY;
            } else {
                SemanticError(Cg->tokenLoc, ERROR___CPP_EOL_IN_STRING);
                return ERROR_SY;
            }
        }
    }
} // byte_scan

int scan_include_name()
{
    char buf[MAX_STRING_LEN + 1];
    int len, ch;

    if (!Cg->currentInput->getch) return 0;
    len = 0;
    while ((ch = Cg->currentInput->getch(Cg->currentInput)) > 0 &&
           ch != '\n' && ch != '>'
    ) {
        if (len < MAX_STRING_LEN)
            buf[len++] = ch;
    }
    buf[len] = 0;
    if (ch == '\n') Cg->currentInput->ungetch(Cg->currentInput, ch);
    return LookUpAddString(atable, buf);
} // scan_include_name;

int yylex(void)
{
    static int last_token = '\n';
    int token;

    for(;;) {
        token = Cg->currentInput->scan(Cg->currentInput);

        if (token == '#' && last_token == '\n') {
            readCPPline();
            continue;
        }
        last_token = token;

        // expand macros
        if (token == IDENT_SY && MacroExpand(yylval.sc_ident))
            continue;

        // convert IDENTs to reserved words or TYPEIDENT as appropriate
        if (token == IDENT_SY) {
            Cg->mostRecentToken = yylval.sc_ident;
            if (yylval.sc_ident >= FIRST_USER_TOKEN_SY) {
                Symbol *pSymb = LookUpSymbol(NULL, yylval.sc_ident);
                if (pSymb && IsTypedef(pSymb))
                    token = TYPEIDENT_SY;
            } else {
                token = yylval.sc_ident;
            }
        } else {
            Cg->mostRecentToken = token;
        }

        if (Cg->options.TraceScanner) {
            if (token >= 127)
                printf("token = %s", GetAtomString(atable, token));
            else if (token >= 32)
                printf("token = <%c>", token);
            else if (token == '\n')
                printf("token = <\\n>");
            else if (token > 0)
                printf("token = <\\0%o>", token);
            else
                printf("token = <EOF>");
            switch (token) {
            case IDENT_SY:
            case TYPEIDENT_SY:
            case STRCONST_SY:
                printf(" = \"%s\"", GetAtomString(atable, yylval.sc_ident));
                break;
            case CFLOATCONST_SY:
            case FLOATCONST_SY:
            case FLOATHCONST_SY:
            case FLOATXCONST_SY:
                printf(" = %9.6g", yylval.sc_fval);
                break;
            case INTCONST_SY:
                printf(" = %d", yylval.sc_int);
                break;
            }
            printf("\n");
        }

        if (token == '\n') {
            continue;
        }

        return token;
    }
} // yylex

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of scanner.c //////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

