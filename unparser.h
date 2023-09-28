/* $Id: unparser.h,v 1.3 2023/03/09 15:44:44 leavens Exp $ */
#ifndef _UNPARSER_H
#define _UNPARSER_H
#include <stdio.h>
#include "ast.h"

// Unparse the given program AST and then print a period and an newline
extern void unparseProgram(FILE *out, AST *ast);

// Unparse the given block, indented by the given level, to out
// adding a semicolon to the end if addSemiToENd is true.
extern void unparseBlock(FILE *out, AST *ast, int indentLevel,
			 bool addSemiToEnd);

// Unparse the list of const-decls given by the AST cds to out
// with the given nesting level
// (note that if cds == NULL, then nothing is printed)
extern void unparseConstDecls(FILE *out, AST_list cds, int level);

// Unparse the list of var-decls given by the AST vds to out
// with the given nesting level
// (note that if vds == NULL, then nothing is printed)
extern void unparseVarDecls(FILE *out, AST_list vds, int level);

// Unparse the list of proc-decls given by the AST pds to out
// with the given nesting level
// (note that if pds == NULL, then nothing is printed)
extern void unparseProcDecls(FILE *out, AST_list pds, int level);

// Unparse the statement given by the AST stmt to out,
// indented for the given level,
// adding a semicolon to the end if addSemiToENd is true.
extern void unparseStmt(FILE *out, AST *stmt, int indentLevel,
			bool addSemiToEnd);

// Unparse the condition given by cond to out
extern void unparseCondition(FILE *out, AST *cond);

// Unparse the given relational operator, relop, to out
extern void unparseRelOp(FILE *out, rel_op relop);

// Unparse the expression given by the AST exp to out
// adding parentheses to indicate the nesting relationships
extern void unparseExpr(FILE *out, AST *exp);

// Unparse the given bin_arith_opo to out
extern void unparseArithOp(FILE *out, bin_arith_op op);

// Unparse the given identifer reference (use) to out
extern void unparseIdent(FILE *out, AST *id);

// Unparse the given number to out in decimal format
extern void unparseNumber(FILE *out, AST *num);

#endif
