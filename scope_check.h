/* $Id: scope_check.h,v 1.3 2023/03/19 00:53:40 leavens Exp $ */
#ifndef _SCOPE_CHECK_H
#define _SCOPE_CHECK_H
#include "ast.h"

// Build the symbol table for the given program AST
// and Check the given program AST for duplicate declarations
// or uses of identifiers that were not declared
extern void scope_check_program(AST *prog);

// build the symbol table and check the declarations in blk
extern void scope_check_block(AST *blk);

// build the symbol table and check the declarations in cds
extern void scope_check_constDecls(AST_list cds);

// check the const declaration cd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
extern void scope_check_constDecl(AST *cd);

// build the symbol table and check the declarations in vds
extern void scope_check_varDecls(AST_list vds);

// check the var declaration vd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
extern void scope_check_varDecl(AST *vd);

// build the symbol table and check the declarations in pds
extern void scope_check_procDecls(AST_list pds);

// check the procedure declaration pd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
extern void scope_check_procDecl(AST *pd);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_stmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_assignStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_callStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_beginStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_ifStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_whileStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_readStmt(AST *stmt);

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_writeStmt(AST *stmt);

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_cond(AST *cond);

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_odd_cond(AST *cond);

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_bin_cond(AST *cond);

// check the expresion to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_expr(AST *exp);

// check that the given constant or variable name has been declared,
// if so, then return an id_use (containing the attributes) for that name,
// otherwise, produce an error using the file_location (floc) given.
extern id_use *scope_check_ident_is_data(file_location floc, const char *name);

// check that the given procedure name has been declared,
// if so, then return an id_use (containing the attributes) for that name,
// otherwise, produce an error using the file_location (floc) given.
extern id_use *scope_check_ident_is_proc_id(file_location floc, const char *name);

// check the expression (exp) to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
extern void scope_check_bin_expr(AST *exp);

#endif
