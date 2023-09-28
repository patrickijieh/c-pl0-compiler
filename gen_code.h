/* $Id: gen_code.h,v 1.4 2023/03/23 02:57:55 leavens Exp $ */
#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include "ast.h"
#include "code.h"

// Initialize the code generator
void gen_code_initialize();

// Generate code for the given AST
extern code_seq gen_code_program(AST *prog);

// generate code for blk
extern code_seq gen_code_block(AST *blk);

// generate code for the declarations in cds
extern code_seq gen_code_constDecls(AST_list cds);

// generate code for the const declaration cd
extern code_seq gen_code_constDecl(AST *cd);

// generate code for the declarations in vds
extern code_seq gen_code_varDecls(AST_list vds);

// generate code for the var declaration vd
extern code_seq gen_code_varDecl(AST *vd);

// generate code for the declarations in pds
// and store them for later use
extern void gen_code_procDecls(AST_list pds);

// generate code for the procedure declaration pd
// and store it for later use
extern void gen_code_procDecl(AST *pd);

// generate code for the statement
extern code_seq gen_code_stmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_assignStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_callStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_beginStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_ifStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_whileStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_readStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_writeStmt(AST *stmt);

// generate code for the statement
extern code_seq gen_code_skipStmt(AST *stmt);

// generate code for the condition
extern code_seq gen_code_cond(AST *cond);

// generate code for the condition
extern code_seq gen_code_odd_cond(AST *cond);

// generate code for the condition
extern code_seq gen_code_bin_cond(AST *cond);

// generate code for the expresion
extern code_seq gen_code_expr(AST *exp);

// generate code for the expression (exp)
extern code_seq gen_code_bin_expr(AST *exp);

// generate code for the ident expression (ident)
extern code_seq gen_code_ident_expr(AST *ident);

// generate code for the number expression (num)
extern code_seq gen_code_number_expr(AST *num);

#endif
