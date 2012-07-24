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
// tokens.c
//

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "slglobals.h"

///////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////// Preprocessor and Token Recorder and Playback: ////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////

/*
 * idstr()
 * Copy a string to a malloc'ed block and convert it into something suitable
 * for an ID
 *
 */

char *idstr(const char *fstr)
{
    size_t len;
    char *str, *t;
    const char *f;

    len = strlen(fstr);
    str = (char *) malloc(len + 1);
    for (f=fstr, t=str; *f; f++) {
        if (isalnum(*f)) *t++ = *f;
        else if (*f == '.' || *f == '/') *t++ = '_';
    }
    *t = 0;
    return str;
} // idstr

/*
 * lCompressToken() -  Rename a token so that most will fit in one byte.
 *
 */

static int lCompressToken(int token)
{
    return token;
} // lCompressToken

/*
 * lExpandToken() -  Expand a token into what the scanner wants.
 *
 */

static int lExpandToken(int token)
{
    return token;
} // lExpandToken

/*
 * lNewBlock()
 *
 */

static TokenBlock *lNewBlock(TokenStream *fTok)
{
    TokenBlock *lBlock;

    lBlock = (TokenBlock *) malloc(sizeof(TokenBlock) + 256);
    lBlock->count = 0;
    lBlock->current = 0;
    lBlock->data = (unsigned char *) lBlock + sizeof(TokenBlock);
    lBlock->max = 256;
    lBlock->next = NULL;
    if (fTok->head) {
        fTok->current->next = lBlock;
    } else {
        fTok->head = lBlock;
    }
    fTok->current = lBlock;
    return lBlock;
} // lNewBlock

/*
 * lAddByte()
 *
 */

static void lAddByte(TokenStream *fTok, unsigned char fVal)
{
    TokenBlock *lBlock;

    lBlock = fTok->current;
    if (lBlock->count >= lBlock->max)
        lBlock = lNewBlock(fTok);
    lBlock->data[lBlock->count++] = fVal;
} // lAddByte

/*
 * lAdd4Bytes()
 *
 */

static void lAdd4Bytes(TokenStream *fTok, unsigned char *fVal)
{
    TokenBlock *lBlock;
    unsigned char *pc;

    lBlock = fTok->current;
    if (lBlock->count + 4 > lBlock->max)
        lBlock = lNewBlock(fTok);
    pc = &lBlock->data[lBlock->count];
    lBlock->count += 4;
    pc[0] = fVal[0];
    pc[1] = fVal[1];
    pc[2] = fVal[2];
    pc[3] = fVal[3];
} // lAdd4Bytes

/*
 * lReadByte() - Get the next byte from a stream.
 *
 */

static int lReadByte(TokenStream *pTok)
{
    TokenBlock *lBlock;
    int lval = -1;

    lBlock = pTok->current;
    if (lBlock) {
        if (lBlock->current >= lBlock->count) {
            lBlock = lBlock->next;
            if (lBlock)
                lBlock->current = 0;
            pTok->current = lBlock;
        }
        if (lBlock)
            lval = lBlock->data[lBlock->current++];
    }
    return lval;
} // lReadByte

/*
 * lRead4Bytes()
 *
 */

static void lRead4Bytes(TokenStream *fTok, unsigned char *fVal)
{
    fVal[0] = (unsigned char ) lReadByte(fTok);
    fVal[1] = (unsigned char ) lReadByte(fTok);
    fVal[2] = (unsigned char ) lReadByte(fTok);
    fVal[3] = (unsigned char ) lReadByte(fTok);
} // lRead4Bytes

/////////////////////////////////////// Global Functions://////////////////////////////////////

/*
 * InitTokenStreams()
 *
 */

int InitTokenStreams(CgStruct *Cg)
{
    return 1;
} // InitTokenStreams

/*
 * NewTokenStream()
 *
 */

TokenStream *NewTokenStream(const char *name)
{
    TokenStream *pTok;

    pTok = (TokenStream *) malloc(sizeof(TokenStream));
    pTok->next = NULL;
    pTok->name = idstr(name);
    pTok->head = NULL;
    pTok->current = NULL;
    lNewBlock(pTok);
    return pTok;
} // NewTokenStream

/*
 * DeleteTokenStream()
 *
 */

void DeleteTokenStream(TokenStream *pTok)
{
    TokenBlock *pBlock, *nBlock;

    if (pTok) {
        pBlock = pTok->head;
        while (pBlock) {
            nBlock = pBlock->next;
            free(pBlock);
            pBlock = nBlock;
        }
        if (pTok->name)
            free(pTok->name);
        free(pTok);
    }
} // DeleteTokenStream

/*
 * RecordToken() - Add a token to the end of a list for later playback or printout.
 *
 */

void RecordToken(TokenStream *pTok, int token)
{
    const char *s;

    // token = lCompressToken(token);
    if (token > 256)
        lAddByte(pTok, (unsigned char)((token & 0x7f) + 0x80));
    else
        lAddByte(pTok, (unsigned char)(token & 0x7f));
    switch (token) {
    case IDENT_SY:
    case TYPEIDENT_SY:
    case STRCONST_SY:
        s = GetAtomString(atable, yylval.sc_ident);
        while (*s)
            lAddByte(pTok, (unsigned char) *s++);
        lAddByte(pTok, 0);
        break;
    case CFLOATCONST_SY:
    case FLOATCONST_SY:
    case FLOATHCONST_SY:
    case FLOATXCONST_SY:
        lAdd4Bytes(pTok, (unsigned char *) &yylval.sc_fval);
        break;
    case INTCONST_SY:
        lAdd4Bytes(pTok, (unsigned char *) &yylval.sc_int);
        break;
    case '(':
        lAddByte(pTok, (unsigned char)(yylval.sc_int ? 1 : 0));
    default:
        break;
    }
} // RecordToken

/*
 * RewindTokenStream() - Reset a token stream in preperation for reading.
 *
 */

void RewindTokenStream(TokenStream *pTok)
{
    if (pTok->head) {
        pTok->current = pTok->head;
        pTok->current->current = 0;
    }
} // RewindTokenStream

/*
 * ReadToken() - Read the next token from a stream.
 *
 */

int ReadToken(TokenStream *pTok)
{
    char symbol_name[MAX_SYMBOL_NAME_LEN + 1];
    char string_val[MAX_STRING_LEN + 1];
    int ltoken, len;
    char ch;

    ltoken = lReadByte(pTok);
    if (ltoken >= 0) {
        if (ltoken > 127)
            ltoken += 128;
        //ltoken = lExpandToken(ltoken);
        switch (ltoken) {
        case IDENT_SY:
        case TYPEIDENT_SY:
            len = 0;
            ch = lReadByte(pTok);
            while ((ch >= 'a' && ch <= 'z') ||
                     (ch >= 'A' && ch <= 'Z') ||
                     (ch >= '0' && ch <= '9') ||
                     ch == '_')
            {
                if (len < MAX_SYMBOL_NAME_LEN) {
                    symbol_name[len] = ch;
                    len++;
                    ch = lReadByte(pTok);
                }
            }
            symbol_name[len] = '\0';
            assert(ch == '\0');
            yylval.sc_ident = LookUpAddString(atable, symbol_name);
            return IDENT_SY;
            break;
        case STRCONST_SY:
            len = 0;
            while ((ch = lReadByte(pTok)) != 0)
                if (len < MAX_STRING_LEN)
                    string_val[len++] = ch;
            string_val[len] = 0;
            yylval.sc_ident = LookUpAddString(atable, string_val);
            break;
        case CFLOATCONST_SY:
        case FLOATCONST_SY:
        case FLOATHCONST_SY:
        case FLOATXCONST_SY:
            lRead4Bytes(pTok, (unsigned char *) &yylval.sc_fval);
            break;
        case INTCONST_SY:
            lRead4Bytes(pTok, (unsigned char *) &yylval.sc_int);
            break;
        case '(':
            yylval.sc_int = lReadByte(pTok);
            break;
        }
        return ltoken;
    }
    return EOF_SY;
} // ReadToken

typedef struct TokenInputSrc {
    InputSrc            base;
    TokenStream         *tokens;
    int                (*final)(CgStruct *);
} TokenInputSrc;

static int scan_token(TokenInputSrc *in)
{
    int token = ReadToken(in->tokens);
    int (*final)(CgStruct *);
    Cg->tokenLoc->file = Cg->currentInput->name;
    Cg->tokenLoc->line = Cg->currentInput->line;
    if (token == '\n') {
        in->base.line++;
        //printf("    end of line %d\n", tokenloc->line);
        return token;
    }
    if (token > 0) return token;
    Cg->currentInput = in->base.prev;
    final = in->final;
    free(in);
    if (final && !final(Cg)) return -1;
    return Cg->currentInput->scan(Cg->currentInput);
}

int ReadFromTokenStream(TokenStream *ts, int name, int (*final)(CgStruct *))
{
    TokenInputSrc *in = malloc(sizeof(TokenInputSrc));
    memset(in, 0, sizeof(TokenInputSrc));
    in->base.name = name;
    in->base.prev = Cg->currentInput;
    in->base.scan = (int (*)(InputSrc *))scan_token;
    in->base.line = 1;
    in->tokens = ts;
    in->final = final;
    RewindTokenStream(ts);
    Cg->currentInput = &in->base;
    return 1;
}

typedef struct UngotToken {
    InputSrc    base;
    int         token;
    YYSTYPE     lval;
} UngotToken;

static int reget_token(UngotToken *t)
{
    int token = t->token;
    yylval = t->lval;
    Cg->currentInput = t->base.prev;
    free(t);
    return token;
}

void UngetToken(int token) {
    UngotToken *t = malloc(sizeof(UngotToken));
    memset(t, 0, sizeof(UngotToken));
    t->token = token;
    t->lval = yylval;
    t->base.scan = (void *)reget_token;
    t->base.prev = Cg->currentInput;
    t->base.name = Cg->currentInput->name;
    t->base.line = Cg->currentInput->line;
    Cg->currentInput = &t->base;
}

///////////////////////////////////// Tokenize Input File: ////////////////////////////////////

#if defined(CGC_ENABLE_TOOLS)

void TokenizeInput(void)
{
    int ltoken, index, count;

    TokenStream *RecordedTokens = NewTokenStream(Cg->options.sourceFileName);
    while ((ltoken = Cg->currentInput->scan(Cg->currentInput)) > 0 &&
           ltoken != ERROR_SY
    ) {
        RecordToken(RecordedTokens, ltoken);
    }

    // Debug print stuff to screen:

#if 0
    RewindTokenStream(RecordedTokens);
    ltoken = ReadToken(RecordedTokens);
    while (ltoken != EOF_SY && ltoken != ERROR_SY) {
        if (ltoken >= 127)
            printf("token = %s", GetAtomString(atable, ltoken));
        else if (ltoken >= 32) 
            printf("token = <%c>", ltoken);
        else if (ltoken == '\n')
            printf("token = <\\n>");
        else if (ltoken > 0)
            printf("token = <\\0%o>", ltoken);
        else
            printf("token = <EOF>");
        switch (ltoken) {
        case IDENT_SY:
        case TYPEIDENT_SY:
        case STRCONST_SY:
            printf(" = \"%s\"", GetAtomString(atable, yylval.sc_ident));
            break;
        case CFLOATCONST_SY:
        case FLOATCONST_SY:
        case FLOATHCONST_SY:
        case FLOATXCONST_SY:
            printf(" = %g9.6", yylval.sc_fval);
            break;
        case INTCONST_SY:
            printf(" = %d", yylval.sc_int);
            break;
        }
        printf("\n");
        ltoken = ReadToken(RecordedTokens);
    }
#endif

    // Dump file to screen as a C initialization statement:

    printf("// automatically generated from %s -- do no edit\n\n",
           Cg->options.sourceFileName);
    printf("#include <stdio.h>\n");
    printf("#include \"slglobals.h\"\n\n");
    printf("unsigned char %s_tokendata[] = {\n", RecordedTokens->name);
    RewindTokenStream(RecordedTokens);
    ltoken = lReadByte(RecordedTokens);
    index = count = 0;
    while (ltoken != EOF_SY && ltoken != ERROR_SY) {
        if (index == 0)
            printf("    ");
        printf("0x%02x,", ltoken);
        count++;
        index++;
        if (index >= 16) {
            printf("\n");
            index = 0;
        }
        ltoken = lReadByte(RecordedTokens);
    }
    if (index > 0)
        printf("\n");
    printf("};");
    printf("\n");
    printf("TokenBlock %s_blockdata = {\n", RecordedTokens->name);
    printf("    NULL, // next\n");
    printf("    0,    // current\n");
    printf("    %d, // count\n", count);
    printf("    %d, // max\n", count);
    printf("    %s_tokendata // data\n", RecordedTokens->name);
    printf("};\n");

    printf("TokenStream %s_stream = { \n", RecordedTokens->name);
    printf("    NULL, // next\n"
           "    \"%s\", // name\n", Cg->options.sourceFileName);
    printf("    &%s_blockdata, // head\n", RecordedTokens->name);
    printf("    &%s_blockdata // current\n", RecordedTokens->name);
    printf("};\n");

} // TokenizeInput

void DumpTokenStream(FILE *fp, TokenStream *s) {
    int token;

    if (fp == 0) fp = stdout;
    RewindTokenStream(s);
    while ((token = ReadToken(s)) > 0) {
        switch (token) {
        case IDENT_SY:
        case TYPEIDENT_SY:
            printf("%s ", GetAtomString(atable, yylval.sc_ident));
            break;
        case STRCONST_SY:
            printf("\"%s\"", GetAtomString(atable, yylval.sc_ident));
            break;
        case CFLOATCONST_SY:
        case FLOATCONST_SY:
        case FLOATHCONST_SY:
        case FLOATXCONST_SY:
            printf("%g9.6 ", yylval.sc_fval);
            break;
        case INTCONST_SY:
            printf("%d ", yylval.sc_int);
            break;
        default:
            if (token >= 127)
                printf("%s ", GetAtomString(atable, token));
            else
                printf("%c", token);
            break;
        }
    }
}
#endif // defined(CGC_ENABLE_TOOLS)

///////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////// End of tokens.c ///////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
