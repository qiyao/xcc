/*
   Copyright (C) 2003 Tensilica, Inc.  All Rights Reserved.
   Revised to support Tensilica processors and to improve overall performance
 */

typedef union {long itype; tree ttype; enum tree_code code;
	const char *filename; int lineno; } YYSTYPE;
#define	IDENTIFIER	257
#define	TYPENAME	258
#define	SCSPEC	259
#define	TYPESPEC	260
#define	TYPE_QUAL	261
#define	CONSTANT	262
#define	STRING	263
#define	ELLIPSIS	264
#define	SIZEOF	265
#define	ENUM	266
#define	STRUCT	267
#define	UNION	268
#define	IF	269
#define	ELSE	270
#define	WHILE	271
#define	DO	272
#define	FOR	273
#define	SWITCH	274
#define	CASE	275
#define	DEFAULT	276
#define	BREAK	277
#define	CONTINUE	278
#define	RETURN	279
#define	GOTO	280
#define	ASM_KEYWORD	281
#define	TYPEOF	282
#define	ALIGNOF	283
#define	ATTRIBUTE	284
#define	EXTENSION	285
#define	LABEL	286
#define	REALPART	287
#define	IMAGPART	288
#define	VA_ARG	289
#define	PTR_VALUE	290
#define	PTR_BASE	291
#define	PTR_EXTENT	292
#define	END_OF_LINE	293
#define	ASSIGN	294
#define	OROR	295
#define	ANDAND	296
#define	EQCOMPARE	297
#define	ARITHCOMPARE	298
#define	LSHIFT	299
#define	RSHIFT	300
#define	UNARY	301
#define	PLUSPLUS	302
#define	MINUSMINUS	303
#define	HYPERUNARY	304
#define	POINTSAT	305
#define	INTERFACE	306
#define	IMPLEMENTATION	307
#define	END	308
#define	SELECTOR	309
#define	DEFS	310
#define	ENCODE	311
#define	CLASSNAME	312
#define	PUBLIC	313
#define	PRIVATE	314
#define	PROTECTED	315
#define	PROTOCOL	316
#define	OBJECTNAME	317
#define	CLASS	318
#define	ALIAS	319
#define	OBJC_STRING	320


extern YYSTYPE yylval;
