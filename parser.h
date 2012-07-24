#ifndef BISON_PARSER_H
# define BISON_PARSER_H

#ifndef YYSTYPE
typedef union {
    int    sc_token;
    int    sc_int;
    float  sc_fval;
    int    sc_ident;
    spec   sc_specifiers;
    dtype  sc_type;
    Type   *sc_ptype;
    decl   *sc_decl;
    expr   *sc_expr;
    symb   *sc_symb;
    stmt   *sc_stmt;
    /* Dummy palce holder: */
    int    dummy;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	AND_SY	257
# define	ASM_SY	258
# define	ASSIGNMINUS_SY	259
# define	ASSIGNMOD_SY	260
# define	ASSIGNPLUS_SY	261
# define	ASSIGNSLASH_SY	262
# define	ASSIGNSTAR_SY	263
# define	BOOLEAN_SY	264
# define	BREAK_SY	265
# define	CASE_SY	266
# define	CFLOATCONST_SY	267
# define	COLONCOLON_SY	268
# define	CONST_SY	269
# define	CONTINUE_SY	270
# define	DEFAULT_SY	271
# define	DISCARD_SY	272
# define	DO_SY	273
# define	EQ_SY	274
# define	ELSE_SY	275
# define	ERROR_SY	276
# define	EXTERN_SY	277
# define	FLOAT_SY	278
# define	FLOATCONST_SY	279
# define	FLOATHCONST_SY	280
# define	FLOATXCONST_SY	281
# define	FOR_SY	282
# define	GE_SY	283
# define	GG_SY	284
# define	GOTO_SY	285
# define	IDENT_SY	286
# define	IF_SY	287
# define	IN_SY	288
# define	INLINE_SY	289
# define	INOUT_SY	290
# define	INT_SY	291
# define	INTCONST_SY	292
# define	INTERNAL_SY	293
# define	LE_SY	294
# define	LL_SY	295
# define	MINUSMINUS_SY	296
# define	NE_SY	297
# define	OR_SY	298
# define	OUT_SY	299
# define	PACKED_SY	300
# define	PLUSPLUS_SY	301
# define	RETURN_SY	302
# define	STATIC_SY	303
# define	STRCONST_SY	304
# define	STRUCT_SY	305
# define	SWITCH_SY	306
# define	TEXOBJ_SY	307
# define	THIS_SY	308
# define	TYPEDEF_SY	309
# define	TYPEIDENT_SY	310
# define	UNIFORM_SY	311
# define	VARYING_SY	312
# define	VOID_SY	313
# define	WHILE_SY	314
# define	FIRST_USER_TOKEN_SY	315


extern YYSTYPE yylval;

#endif /* not BISON_PARSER_H */
