/* $Id: parser.h,v 1.1 2023/03/08 15:18:43 leavens Exp $ */
// This header file defines the externally-visible entry points to the parser
#ifndef _PARSER_H
#define _PARSER_H
#include "ast.h"

// initialize the parser to work on the given file
extern void parser_open(const char *filename);

// finish using the parser
extern void parser_close();

// <program> ::= <block> .
extern AST *parseProgram();

// <block> ::= <const-decls> <var-decls> <stmt>
extern AST *parseBlock();

// <stmt> ::= <ident> := <expr> | ...
extern AST *parseStmt();

// <condition> ::= <odd-cond> | <bin-rel-cond>
extern AST *parseCondition();

// <expr> ::= <term> { <add-sub-term> }
extern AST *parseExpr();

#endif
