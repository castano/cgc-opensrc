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
// cpp.c
//

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "slglobals.h"

/* Don't use memory.c's replacements, as we clean up properly here */
#undef malloc
#undef free

static int bindAtom = 0;
static int constAtom = 0;
static int defaultAtom = 0;
static int defineAtom = 0;
static int definedAtom = 0;
static int elseAtom = 0;
static int elifAtom = 0;
static int endifAtom = 0;
static int ifAtom = 0;
static int ifdefAtom = 0;
static int ifndefAtom = 0;
static int includeAtom = 0;
static int lineAtom = 0;
static int pragmaAtom = 0;
static int texunitAtom = 0;
static int undefAtom = 0;
static int __LINE__Atom = 0;
static int __FILE__Atom = 0;


static Scope *macros = 0;
#define MAX_MACRO_ARGS  64

static int ifdepth = 0; /* depth of #if nesting -- used to detect invalid
                         * #else/#endif */
static SourceLoc ifloc; /* outermost #if */

int InitCPP(void)
{
    char        buffer[64], *t;
    const char  *f;
    // Add various atoms needed by the CPP line scanner:
    bindAtom = LookUpAddString(atable, "bind");
    constAtom = LookUpAddString(atable, "const");
    defaultAtom = LookUpAddString(atable, "default");
    defineAtom = LookUpAddString(atable, "define");
    definedAtom = LookUpAddString(atable, "defined");
    elifAtom = LookUpAddString(atable, "elif");
    elseAtom = LookUpAddString(atable, "else");
    endifAtom = LookUpAddString(atable, "endif");
    ifAtom = LookUpAddString(atable, "if");
    ifdefAtom = LookUpAddString(atable, "ifdef");
    ifndefAtom = LookUpAddString(atable, "ifndef");
    includeAtom = LookUpAddString(atable, "include");
    lineAtom = LookUpAddString(atable, "line");
    pragmaAtom = LookUpAddString(atable, "pragma");
    texunitAtom = LookUpAddString(atable, "texunit");
    undefAtom = LookUpAddString(atable, "undef");
    __LINE__Atom = LookUpAddString(atable, "__LINE__");
    __FILE__Atom = LookUpAddString(atable, "__FILE__");
    macros = NewScopeInPool(mem_CreatePool(0, 0));
    strcpy(buffer, "PROFILE_");
    t = buffer + strlen(buffer);
    f = Cg->options.profileString;
    while ((isalnum(*f) || *f == '_') && t < buffer + sizeof(buffer) - 1)
        *t++ = toupper(*f++);
    *t = 0;
    PredefineMacro(buffer);
    return 1;
} // InitCPP

int FinalCPP(void)
{
    if (ifdepth)
        SemanticWarning(&ifloc, WARNING___CPP_IF_MISMATCH, "if");
    return 1;
}

static int CPPdefine()
{
    int token, name, args[MAX_MACRO_ARGS], argc;
    MacroSymbol mac;
    Symbol *symb;
    SourceLoc dummyLoc;

    memset(&mac, 0, sizeof(mac));
    token = Cg->currentInput->scan(Cg->currentInput);
    if (token != IDENT_SY) {
        SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
        return token;
    }
    name = yylval.sc_ident;
    token = Cg->currentInput->scan(Cg->currentInput);
    if (token == '(' && !yylval.sc_int) {
        // gather arguments
        argc = 0;
        do {
            token = Cg->currentInput->scan(Cg->currentInput);
            if (argc == 0 && token == ')') break;
            if (token != IDENT_SY) {
                SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
                return token;
            }
            if (argc < MAX_MACRO_ARGS)
                args[argc++] = yylval.sc_ident;
            token = Cg->currentInput->scan(Cg->currentInput);
        } while (token == ',');
        if (token != ')') {
            SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "define");
            return token;
        }
        mac.argc = argc;
        mac.args = mem_Alloc(macros->pool, argc * sizeof(int));
        memcpy(mac.args, args, argc * sizeof(int));
        token = Cg->currentInput->scan(Cg->currentInput);
    }
    mac.body = NewTokenStream(GetAtomString(atable, name));
    while (token != '\n') {
        while (token == '\\') {
            token = Cg->currentInput->scan(Cg->currentInput);
            if (token == '\n')
                token = Cg->currentInput->scan(Cg->currentInput);
            else
                RecordToken(mac.body, '\\');
        }
        RecordToken(mac.body, token);
        token = Cg->currentInput->scan(Cg->currentInput);
    };

    symb = LookUpSymbol(macros, name);
    if (symb) {
        if (!symb->details.mac.undef) {
            // already defined -- need to make sure they are identical
            if (symb->details.mac.argc != mac.argc) goto error;
            for (argc=0; argc < mac.argc; argc++)
                if (symb->details.mac.args[argc] != mac.args[argc])
                    goto error;
            RewindTokenStream(symb->details.mac.body);
            RewindTokenStream(mac.body);
            do {
                int old_lval, old_token;
                old_token = ReadToken(symb->details.mac.body);
                old_lval = yylval.sc_int;
                token = ReadToken(mac.body);
                if (token != old_token || yylval.sc_int != old_lval) {
                error:
                    SemanticWarning(Cg->tokenLoc, WARNING___CPP_MACRO_REDEFINED,
                                    GetAtomString(atable, name));
                    break; }
            } while (token > 0);
        }
        FreeMacro(&symb->details.mac);
    } else {
        dummyLoc.file = 0;
        dummyLoc.line = 0;
        symb = AddSymbol(&dummyLoc, macros, name, 0, MACRO_S);
    }
    symb->details.mac = mac;
    return '\n';
} // CPPdefine

static int CPPundef()
{
    int token = Cg->currentInput->scan(Cg->currentInput);
    Symbol *symb;

    if (token != IDENT_SY) goto error;
    symb = LookUpSymbol(macros, yylval.sc_ident);
    if (symb) {
        symb->details.mac.undef = 1;
    }
    token = Cg->currentInput->scan(Cg->currentInput);
    if (token != '\n') {
    error:
        SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "undef");
    }
    return token;
} // CPPundef

static int CPPif();

/* CPPelse -- skip forward to appropriate spot.  This is actually used
** to skip to and #endif after seeing an #else, AND to skip to a #else,
** #elif, or #endif after a #if/#ifdef/#ifndef/#elif test was false
*/

static int CPPelse(int matchelse)
{
    int atom, depth = 0;
    int token = Cg->currentInput->scan(Cg->currentInput);
    while (token > 0) {
        while (token != '\n')
            token = Cg->currentInput->scan(Cg->currentInput);
        if ((token = Cg->currentInput->scan(Cg->currentInput)) != '#')
            continue;
        if ((token = Cg->currentInput->scan(Cg->currentInput)) != IDENT_SY)
            continue;
        atom = yylval.sc_ident;
        if (atom == ifAtom || atom == ifdefAtom || atom == ifndefAtom)
            depth++;
        else if (atom == endifAtom) {
            if (--depth < 0) {
                if (ifdepth) ifdepth--;
                break;
            }
        }
        else if (matchelse && depth == 0) {
            if (atom == elseAtom)
                break;
            else if (atom == elifAtom) {
                /* we decrement ifdepth here, because CPPif will increment
                 * it and we really want to leave it alone */
                if (ifdepth) ifdepth--;
                return CPPif();
            }
        }
    };
    return token;
}

enum eval_prec {
    MIN_PREC,
    COND, LOGOR, LOGAND, OR, XOR, AND, EQUAL, RELATION, SHIFT, ADD, MUL, UNARY,
    MAX_PREC
};

static int op_logor(int a, int b) { return a || b; }
static int op_logand(int a, int b) { return a && b; }
static int op_or(int a, int b) { return a | b; }
static int op_xor(int a, int b) { return a ^ b; }
static int op_and(int a, int b) { return a & b; }
static int op_eq(int a, int b) { return a == b; }
static int op_ne(int a, int b) { return a != b; }
static int op_ge(int a, int b) { return a >= b; }
static int op_le(int a, int b) { return a <= b; }
static int op_gt(int a, int b) { return a > b; }
static int op_lt(int a, int b) { return a < b; }
static int op_shl(int a, int b) { return a << b; }
static int op_shr(int a, int b) { return a >> b; }
static int op_add(int a, int b) { return a + b; }
static int op_sub(int a, int b) { return a - b; }
static int op_mul(int a, int b) { return a * b; }
static int op_div(int a, int b) { return a / b; }
static int op_mod(int a, int b) { return a % b; }
static int op_pos(int a) { return a; }
static int op_neg(int a) { return -a; }
static int op_cmpl(int a) { return ~a; }
static int op_not(int a) { return !a; }

struct {
    int token, prec, (*op)(int, int);
} binop[] = {
    { OR_SY, LOGOR, op_logor },
    { AND_SY, LOGAND, op_logand },
    { '|', OR, op_or },
    { '^', XOR, op_xor },
    { '&', AND, op_and },
    { EQ_SY, EQUAL, op_eq },
    { NE_SY, EQUAL, op_ne },
    { '>', RELATION, op_gt },
    { GE_SY, RELATION, op_ge },
    { '<', RELATION, op_lt },
    { LE_SY, RELATION, op_le },
    { LL_SY, SHIFT, op_shl },
    { GG_SY, SHIFT, op_shr },
    { '+', ADD, op_add },
    { '-', ADD, op_sub },
    { '*', MUL, op_mul },
    { '/', MUL, op_div },
    { '%', MUL, op_mod },
};

struct {
    int token, (*op)(int);
} unop[] = {
    { '+', op_pos },
    { '-', op_neg },
    { '~', op_cmpl },
    { '!', op_not },
};

#define ALEN(A) (sizeof(A)/sizeof(A[0]))

int eval(int token, int prec, int *res, int *err)
{
    int         i, val;
    Symbol      *s;

    if (token == IDENT_SY) {
        if (yylval.sc_ident == definedAtom) {
            int needclose = 0;

            token = Cg->currentInput->scan(Cg->currentInput);
            if (token == '(') {
                needclose = 1;
                token = Cg->currentInput->scan(Cg->currentInput);
            }
            if (token != IDENT_SY)
                goto error;
            *res = (s = LookUpSymbol(macros, yylval.sc_ident))
                        ? !s->details.mac.undef : 0;
            token = Cg->currentInput->scan(Cg->currentInput);
            if (needclose) {
                if (token != ')')
                    goto error;
                token = Cg->currentInput->scan(Cg->currentInput);
            }
        } else if (MacroExpand(yylval.sc_ident)) {
            token = Cg->currentInput->scan(Cg->currentInput);
            return eval(token, prec, res, err);
        } else {
            goto error;
        }
    } else if (token == INTCONST_SY) {
        *res = yylval.sc_int;
        token = Cg->currentInput->scan(Cg->currentInput);
    } else if (token == '(') {
        token = Cg->currentInput->scan(Cg->currentInput);
        token = eval(token, MIN_PREC, res, err);
        if (!*err) {
            if (token != ')')
                goto error;
            token = Cg->currentInput->scan(Cg->currentInput);
        }
    } else {
        for (i = ALEN(unop) - 1; i >= 0; i--) {
            if (unop[i].token == token)
                break;
        }
        if (i >= 0) {
            token = Cg->currentInput->scan(Cg->currentInput);
            token = eval(token, UNARY, res, err);
            *res = unop[i].op(*res);
        } else {
            goto error;
        }
    }
    while (!*err) {
        if (token == ')' || token == '\n') break;
        for (i = ALEN(binop) - 1; i >= 0; i--) {
            if (binop[i].token == token)
                break;
        }
        if (i < 0 || binop[i].prec <= prec)
            break;
        val = *res;
        token = Cg->currentInput->scan(Cg->currentInput);
        token = eval(token, binop[i].prec, res, err);
        *res = binop[i].op(val, *res);
    }
    return token;
error:
    SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "if");
    *err = 1;
    *res = 0;
    return token;
} // eval

static int CPPif() {
    int token = Cg->currentInput->scan(Cg->currentInput);
    int res = 0, err = 0;

    if (!ifdepth++)
        ifloc = *Cg->tokenLoc;
    token = eval(token, MIN_PREC, &res, &err);
    if (token != '\n') {
        SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "if");
    } else if (!res && !err) {
        token = CPPelse(1);
    }
    return token;
} // CPPif

static int CPPifdef(int defined)
{
    int token = Cg->currentInput->scan(Cg->currentInput);
    int name = yylval.sc_ident;
    ifdepth++;
    if (token != IDENT_SY) {
        SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX,
                      defined ? "ifdef" : "ifndef");
    } else {
        Symbol *s = LookUpSymbol(macros, name);
        if (((s && !s->details.mac.undef) ? 1 : 0) != defined)
            token = CPPelse(1);
    }
    return token;
} // CPPifdef

static int CPPinclude()
{
    int tok = Cg->currentInput->scan(Cg->currentInput);
    int file = 0;
    if (tok == STRCONST_SY) {
        file = yylval.sc_ident;
    } else if (tok == '<') {
        file = scan_include_name();
    } else {
        SemanticError(Cg->tokenLoc, ERROR___CPP_SYNTAX, "include");
    }
    while (tok != '\n')
        tok = Cg->currentInput->scan(Cg->currentInput);
    SetInputFile(GetAtomString(atable, file));
    return '\n';
} // CPPinclude

static int CPPline(int token) {
    if (token == INTCONST_SY) {
        int line = yylval.sc_int;
        token = Cg->currentInput->scan(Cg->currentInput);
        if (token == STRCONST_SY) {
            Cg->currentInput->name = yylval.sc_ident;
            Cg->currentInput->line = line - 1; // Will get bumped by one.
            token = Cg->currentInput->scan(Cg->currentInput);
        }
    }
    return token;
}

static int CPPpragma(void)
{
    int identa, identb, identc;
    int ival, HasIval, NegSign, numfvals;
    float fval[4];
    int token;
    int err;
#define NEXTTOKEN       \
        /* get the next token, while expanding macros */        \
        do {                                                    \
            token = Cg->currentInput->scan(Cg->currentInput);         \
        } while (token == IDENT_SY && MacroExpand(yylval.sc_ident))

    NEXTTOKEN;
    if (token == IDENT_SY && yylval.sc_ident == bindAtom) {

        // Parse:  # "pragma" "bind" <conn-id> "." <memb-id> "=" <reg-id>
        //         # "pragma" "bind" <prog-id> "." <parm-id> "=" <reg-id> <i-const>
        //         # "pragma" "bind" <prog-id> "." <parm-id> "=" "texunit" <i-const>
        //         # "pragma" "bind" <prog-id> "." <memb-id> "=" "const" <number>+
        //         # "pragma" "bind" <prog-id> "." <memb-id> "=" "default" <number>+
        //
        //  <number> ::= [ "-" ] ( <i-const> | <f-const> )

        err = 0;
        NEXTTOKEN;
        if (token == IDENT_SY) {
            identa = yylval.sc_ident;
            NEXTTOKEN;
            if (token == '.') {
                NEXTTOKEN;
            } else {
                err = 1;
            }
            if (token == IDENT_SY) {
                identb = yylval.sc_ident;
                NEXTTOKEN;
                if (token == '=') {
                    NEXTTOKEN;
                } else {
                    err = 1;
                }
                if (token == IDENT_SY) {
                    identc = yylval.sc_ident;
                    NEXTTOKEN;
                } else {
                    err = 1;
                }
                numfvals = 0;
                HasIval = 0;
                while (token == INTCONST_SY ||
                       token == CFLOATCONST_SY ||
                       token == '-'
                ) {
                    if (token == '-') {
                        NegSign = 1;
                        NEXTTOKEN;
                    } else {
                        NegSign = 0;
                    }
                    if (token == INTCONST_SY) {
                        if (numfvals == 0 && !NegSign) {
                            ival = yylval.sc_int;
                            HasIval = 1;
                        }
                        if (NegSign)
                            yylval.sc_int = -yylval.sc_int;
                        if (numfvals < 4) {
                            fval[numfvals] = (float) yylval.sc_int;
                            numfvals++;
                        } else {
                            err = 1;
                            break;
                        }
                        NEXTTOKEN;
                    } else if (token == CFLOATCONST_SY) {
                        if (NegSign)
                            yylval.sc_fval = -yylval.sc_fval;
                        if (numfvals < 4) {
                            fval[numfvals] = yylval.sc_fval;
                            numfvals++;
                        } else {
                            err = 1;
                            break;
                        }
                        NEXTTOKEN;
                    } else {
                        err = 1;
                        break;
                    }
                }
                if (!err) {
                    if (identc == texunitAtom) {
                        if (HasIval && numfvals == 1) {
                            DefineTexunitBinding(Cg->tokenLoc, identa, identb, ival);
                        } else {
                            err = 1;
                        }
                    } else if (identc == constAtom) {
                        if (numfvals > 0) {
                            DefineConstantBinding(Cg->tokenLoc, identa, identb, numfvals, fval);
                        } else {
                            err = 1;
                        }
                    } else if (identc == defaultAtom) {
                        if (numfvals > 0) {
                            DefineDefaultBinding(Cg->tokenLoc, identa, identb, numfvals, fval);
                        } else {
                           err = 1;
                        }
                    } else if (HasIval) {
                        DefineRegArrayBinding(Cg->tokenLoc, identa, identb, identc, ival, 0);
                    } else {
                        DefineConnectorBinding(Cg->tokenLoc, identa, identb, identc);
                    }
                }
            } else {
                err = 1;
            }
        } else {
            err = 1;
        }
        if (err == 1) {
            SemanticError(Cg->tokenLoc, ERROR___CPP_BIND_PRAGMA_ERROR);
        }
    }
    return token;
} // CPPpragma

void readCPPline(void)
{
    int token = Cg->currentInput->scan(Cg->currentInput);
    if (token == IDENT_SY) {
        if (yylval.sc_ident == defineAtom) {
            token = CPPdefine();
        } else if (yylval.sc_ident == elseAtom) {
            if (!ifdepth)
                SemanticWarning(Cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "else");
            token = CPPelse(0);
        } else if (yylval.sc_ident == elifAtom) {
            if (!ifdepth)
                SemanticWarning(Cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "elif");
            token = CPPelse(0);
        } else if (yylval.sc_ident == endifAtom) {
            if (!ifdepth)
                SemanticWarning(Cg->tokenLoc, WARNING___CPP_IF_MISMATCH, "endif");
            else
                ifdepth--;
        } else if (yylval.sc_ident == ifAtom) {
            token = CPPif();
        } else if (yylval.sc_ident == ifdefAtom) {
            token = CPPifdef(1);
        } else if (yylval.sc_ident == ifndefAtom) {
            token = CPPifdef(0);
        } else if (yylval.sc_ident == includeAtom) {
            token = CPPinclude();
        } else if (yylval.sc_ident == lineAtom) {
            token = CPPline(Cg->currentInput->scan(Cg->currentInput));
        } else if (yylval.sc_ident == pragmaAtom) {
            token = CPPpragma();
        } else if (yylval.sc_ident == undefAtom) {
            token = CPPundef();
        } else {
            SemanticError(Cg->tokenLoc, ERROR___CPP_UNKNOWN_DIRECTIVE,
                          GetAtomString(atable, yylval.sc_ident));
        }
    } else if (token == INTCONST_SY) {
        token = CPPline(token);
    }
    while (token != '\n' && token != 0 /* EOF */) {
        token = Cg->currentInput->scan(Cg->currentInput);
    }
} // readCPPline

void FreeMacro(MacroSymbol *s) {
    DeleteTokenStream(s->body);
}

static int eof_scan(InputSrc *in) { return -1; }
static void noop(InputSrc *in, int ch) { }

static void PushEofSrc() {
    InputSrc *in = malloc(sizeof(InputSrc));
    memset(in, 0, sizeof(InputSrc));
    in->scan = eof_scan;
    in->getch = eof_scan;
    in->ungetch = noop;
    in->prev = Cg->currentInput;
    Cg->currentInput = in;
}

static void PopEofSrc() {
    if (Cg->currentInput->scan == eof_scan) {
        InputSrc *in = Cg->currentInput;
        Cg->currentInput = in->prev;
        free(in);
    }
}

static TokenStream *PrescanMacroArg(TokenStream *a) {
    int token;
    TokenStream *n;
    RewindTokenStream(a);
    do {
        token = ReadToken(a);
        if (token == IDENT_SY && LookUpSymbol(macros, yylval.sc_ident))
            break;
    } while (token > 0);
    if (token <= 0) return a;
    n = NewTokenStream("macro arg");
    PushEofSrc();
    ReadFromTokenStream(a, 0, 0);
    while ((token = Cg->currentInput->scan(Cg->currentInput)) > 0) {
        if (token == IDENT_SY && MacroExpand(yylval.sc_ident))
            continue;
        RecordToken(n, token);
    }
    PopEofSrc();
    DeleteTokenStream(a);
    return n;
} // PrescanMacroArg

typedef struct MacroInputSrc {
    InputSrc    base;
    MacroSymbol *mac;
    TokenStream **args;
} MacroInputSrc;

/* macro_scan ---
** return the next token for a macro expanion, handling macro args 
*/
static int macro_scan(MacroInputSrc *in) {
    int i;
    int token = ReadToken(in->mac->body);
    if (token == IDENT_SY) {
        for (i = in->mac->argc-1; i>=0; i--)
            if (in->mac->args[i] == yylval.sc_ident) break;
        if (i >= 0) {
            ReadFromTokenStream(in->args[i], yylval.sc_ident, 0);
            return Cg->currentInput->scan(Cg->currentInput);
        }
    }
    if (token > 0) return token;
    in->mac->busy = 0;
    Cg->currentInput = in->base.prev;
    if (in->args) {
        for (i=in->mac->argc-1; i>=0; i--)
            DeleteTokenStream(in->args[i]);
        free(in->args);
    }
    free(in);
    return Cg->currentInput->scan(Cg->currentInput);
} // macro_scan

/* MacroExpand
** check an identifier (atom) to see if it a macro that should be expanded.
** If it is, push an InputSrc that will produce the appropriate expandsion
** and return TRUE.  If not, return FALSE.
*/

int MacroExpand(int atom)
{
    Symbol              *sym = LookUpSymbol(macros, atom);
    MacroInputSrc       *in;
    SourceLoc           loc = *Cg->tokenLoc;
    int                 i, token, depth;

    if (atom == __LINE__Atom) {
        yylval.sc_int = Cg->currentInput->line;
        UngetToken(INTCONST_SY);
        return 1;
    }
    if (atom == __FILE__Atom) {
        yylval.sc_ident = Cg->currentInput->name;
        UngetToken(STRCONST_SY);
        return 1;
    }
    if (!sym || sym->details.mac.undef) return 0;
    if (sym->details.mac.busy) return 0;        // no recursive expansions
    in = malloc(sizeof(*in));
    memset(in, 0, sizeof(*in));
    in->base.scan = (void *)macro_scan;
    in->base.line = Cg->currentInput->line;
    in->base.name = Cg->currentInput->name;
    in->mac = &sym->details.mac;
    if (sym->details.mac.args) {
        token = Cg->currentInput->scan(Cg->currentInput);
        if (token != '(') {
            UngetToken(token);
            yylval.sc_ident = atom;
            return 0;
        }
        in->args = malloc(in->mac->argc * sizeof(TokenStream *));
        for (i=0; i<in->mac->argc; i++)
            in->args[i] = NewTokenStream("macro arg");
        for (i=0; i<in->mac->argc; i++) {
            depth = 0;
            while(1) {
                token = Cg->currentInput->scan(Cg->currentInput);
                if (token <= 0) {
                    SemanticError(&loc, ERROR___CPP_MACRO_EOF,
                                  GetAtomString(atable, atom));
                    return 1;
                }
                if (depth == 0 && (token == ',' || token == ')')) break;
                if (token == '(') depth++;
                if (token == ')') depth--;
                RecordToken(in->args[i], token);
            }
            if (token == ')') {
                i++;
                break;
            }
        }
        if (i < in->mac->argc) {
            SemanticError(&loc, ERROR___CPP_MACRO_TOOFEW,
                          GetAtomString(atable, atom));
        } else if (token != ')') {
            while (token >= 0 && (depth > 0 || token != ')')) {
                if (token == ')') depth--;
                token = Cg->currentInput->scan(Cg->currentInput);
                if (token == '(') depth++;
            }
            if (token <= 0) {
                SemanticError(&loc, ERROR___CPP_MACRO_EOF,
                              GetAtomString(atable, atom));
                return 1;
            }
            SemanticError(&loc, ERROR___CPP_MACRO_TOOMANY,
                          GetAtomString(atable, atom));
        }
        for (i=0; i<in->mac->argc; i++) {
            in->args[i] = PrescanMacroArg(in->args[i]);
        }
    }
#if 0
    printf("  <%s:%d>found macro %s\n", GetAtomString(atable, loc.file),
           loc.line, GetAtomString(atable, atom));
    for (i=0; i<in->mac->argc; i++) {
        printf("\targ %s = '", GetAtomString(atable, in->mac->args[i]));
        DumpTokenStream(stdout, in->args[i]);
        printf("'\n");
    }
#endif
    in->base.prev = Cg->currentInput;
    sym->details.mac.busy = 1;
    RewindTokenStream(sym->details.mac.body);
    Cg->currentInput = &in->base;
    return 1;
} // MacroExpand

/* PredefineMacro
** define a macro (no args) based on the input string, as from a -D
** argument.  The string is either "NAME=expansion" or just "NAME", which
** is equivalent to "NAME=1".  Return TRUE if everything is ok, or
** FALSE if the string is malformed.
*/
int PredefineMacro(char *def) {
    char *name = def;
    MacroSymbol mac;
    Symbol *symb;
    SourceLoc dummyLoc;

    while (isalnum(*def) || *def == '_') def++;
    if (def != name && *def == '=') {
        *def = 0;
        def++;
    } else if (def == name || *def != 0) {
        return 0;
    } else {
        def = 0;
    }
    memset(&mac, 0, sizeof(mac));
    mac.body = NewTokenStream(name);
    if (def) {
        int     token;
        PushEofSrc();
        ScanFromString(def);
        while ((token = Cg->currentInput->scan(Cg->currentInput)) > 0)
            RecordToken(mac.body, token);
        PopEofSrc();
    } else {
        yylval.sc_int = 1;
        RecordToken(mac.body, INTCONST_SY);
    }
    symb = LookUpSymbol(macros, LookUpAddString(atable, name));
    if (symb) {
        FreeMacro(&symb->details.mac);
    } else {
        dummyLoc.file = 0;
        dummyLoc.line = 0;
        symb = AddSymbol(&dummyLoc, macros, LookUpAddString(atable, name), 0, MACRO_S);
    }
    symb->details.mac = mac;
    if (def) {
        // undo change to arg string
        def[-1] = '=';
    }
    return 1;
} // PredefineMacro
