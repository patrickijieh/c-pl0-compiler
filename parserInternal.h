/* $Id: parserInternal.h,v 1.2 2023/03/08 21:18:09 leavens Exp $ */
// This header file makes forward declarations of internal functions
// for the parser. It should not be used by clients.
#ifndef _PARSERINTERNAL_H
#define _PARSERINTERNAL_H
#include "parser.h"

// <const-decls> ::= { <const-decl> }
static AST_list parseConstDecls();

// <const-decl> ::= <const-def> { <comma-const-def> }
static AST *parseConstDecl();

// <const-def> ::= <ident> = <number>
static AST *parseConstDef();

// <var-decls> ::= { <var-decl> }
static AST_list parseVarDecls();

// <var-decl> ::= var <idents> ;
static AST_list parseVarDecl();

// <idents> ::= <ident> { <comma-ident> }
static AST_list parseIdents();

// <proc-decls> ::= { <proc-decl> }
static AST_list parseProcDecls();

// <proc-decl> ::= procedure <ident> ; <block> ;
static AST *parseProcDecl();

// <assignment> ::= <ident> := <expr>
static AST *parseAssignStmt();

// <call-stmt> ::= call <ident>
static AST *parseCallStmt();

// <seq-stmt> ::= begin <stmt> { <semi-stmt> } end
static AST *parseBeginStmt();

// <semi-stmt> ::= ; <stmt>
static AST *parseSemiStmt();

// <if-stmt> ::= if <condition> then <stmt> else <stmt>
static AST *parseIfStmt();

// <while-stmt> ::= while <condition> do <stmt>
static AST *parseWhileStmt();

// <read-stmt> ::= read <ident>
static AST *parseReadStmt();

// <write-stmt> ::= write <expr>
static AST *parseWriteStmt();

// <skip-stmt> ::= skip
static AST *parseSkipStmt();

// <odd-cond> ::= odd <expr>
static AST *parseOddCond();

// <bin-rel-cond> ::= <expr> <rel-op> <expr>
static AST *parseBinRelCond();

// <rel-op> ::= = | <> | < | <= | > | >=
static rel_op parseRelOp();

// <add-sub-term> ::= <add-sub> <term>
static AST *parseAddSubTerm();

// <term> ::= <factor> { <mult-div-factor> }
static AST *parseTerm();

// <mult-div-factor> ::= <mult-div> <factor>
static AST *parseMultDivFactor();

// <factor> ::= <ident> | <paren-expr> | <signed-number>
static AST *parseFactor();

// <paren-expr> ::= ( <expr> )
static AST *parseParenExpr();

// <signed-number> ::= + <number> | - <number> | <number>
static AST *parseSignedNumber();

#endif
