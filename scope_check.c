/* $Id: scope_check.c,v 1.13 2023/03/23 05:39:43 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scope_check.h"
#include "id_attrs.h"
#include "file_location.h"
#include "ast.h"
#include "utilities.h"
#include "symtab.h"
#include "scope_check.h"

// Build the symbol table for the given program AST
// and Check the given program AST for duplicate declarations
// or uses of identifiers that were not declared
// Also, update the AST with information about the identifier uses
void scope_check_program(AST *prog)
{
    scope_check_block(prog);
}

// build the symbol table and check the declarations in blk
void scope_check_block(AST *blk)
{
    symtab_enter_scope();
    scope_check_constDecls(blk->data.program.cds);
    scope_check_varDecls(blk->data.program.vds);
    scope_check_procDecls(blk->data.program.pds);
    scope_check_stmt(blk->data.program.stmt);
    symtab_leave_scope();
}

// build the symbol table and check the declarations in cds
void scope_check_constDecls(AST_list cds)
{
    while (!ast_list_is_empty(cds)) {
	scope_check_constDecl(ast_list_first(cds));
	cds = ast_list_rest(cds);
    }
}

// check the const declaration cd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
void scope_check_constDecl(AST *cd)
{
    id_attrs *attrs = id_attrs_loc_create(cd->file_loc, constant,
					   symtab_next_loc_offset());
    symtab_insert(cd->data.const_decl.name, attrs);
}

// build the symbol table and check the declarations in vds
void scope_check_varDecls(AST_list vds)
{
    while (!ast_list_is_empty(vds)) {
	scope_check_varDecl(ast_list_first(vds));
	vds = ast_list_rest(vds);
    }
}

// check the var declaration vd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
void scope_check_varDecl(AST *vd)
{
    int ofst = symtab_next_loc_offset();
    id_attrs *attrs = id_attrs_loc_create(vd->file_loc, variable,
					   ofst);
    const char *name = vd->data.var_decl.name;
    symtab_insert(name, attrs);
}

// build the symbol table and check the declarations in pds
void scope_check_procDecls(AST_list pds)
{
    while (!ast_list_is_empty(pds)) {
	scope_check_procDecl(ast_list_first(pds));
	pds = ast_list_rest(pds);
    }
}

// check the procedure declaration pd
// and add it to the current scope's symbol table
// or produce an error if the name has already been declared
void scope_check_procDecl(AST *pd)
{
    label *plabel = pd->data.proc_decl.lab;
    id_attrs *attrs = id_attrs_proc_create(pd->file_loc, plabel);
    symtab_insert(pd->data.proc_decl.name, attrs);
    scope_check_block(pd->data.proc_decl.block);
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_stmt(AST *stmt)
{
    switch (stmt->type_tag) {
    case assign_ast:
	scope_check_assignStmt(stmt);
	break;
    case call_ast:
	scope_check_callStmt(stmt);
	break;
    case begin_ast:
	scope_check_beginStmt(stmt);
	break;
    case if_ast:
	scope_check_ifStmt(stmt);
	break;
    case while_ast:
	scope_check_whileStmt(stmt);
	break;
    case read_ast:
	scope_check_readStmt(stmt);
	break;
    case write_ast:
	scope_check_writeStmt(stmt);
	break;
    case skip_ast:
	// nothing to do, as no identifiers may occur in the statement
	break;
    default:
	bail_with_error("Call to scope_check_stmt with an AST that is not a statement!");
	break;
    }
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_assignStmt(AST *stmt)
{
    const char *name = stmt->data.assign_stmt.ident->data.ident.name;
    id_use *idu = scope_check_ident_is_data(stmt->file_loc, name);
    stmt->data.assign_stmt.ident->data.ident.idu = idu;
    if (stmt->data.assign_stmt.ident->data.ident.idu->attrs->kind != variable) {
	general_error(stmt->file_loc,
		      "identifier \"%s\" is not a variable name",
		      name);
    }
    scope_check_expr(stmt->data.assign_stmt.exp);
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_callStmt(AST *stmt)
{
    id_use *idu // this has the attributes of the procedure declared
	= scope_check_ident_is_proc_id(stmt->file_loc,
				       stmt->data.call_stmt.ident->data.ident.name);
    /*
    fprintf(stderr, "In scope_check_callStmt label address is %p\n",
	    idu->attrs->lab);
    */
    stmt->data.call_stmt.ident->data.ident.idu = idu;
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_beginStmt(AST *stmt)
{
    AST_list stmts = stmt->data.begin_stmt.stmts;
    while (!ast_list_is_empty(stmts)) {
	scope_check_stmt(ast_list_first(stmts));
	stmts = ast_list_rest(stmts);
    }
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_ifStmt(AST *stmt)
{
    scope_check_cond(stmt->data.if_stmt.cond);
    scope_check_stmt(stmt->data.if_stmt.thenstmt);
    scope_check_stmt(stmt->data.if_stmt.elsestmt);
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_whileStmt(AST *stmt)
{
    scope_check_cond(stmt->data.while_stmt.cond);
    scope_check_stmt(stmt->data.while_stmt.stmt);
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_readStmt(AST *stmt)
{
    stmt->data.read_stmt.ident->data.ident.idu
	= scope_check_ident_is_data(stmt->file_loc,
				    stmt->data.read_stmt.ident->data.ident.name);
}

// check the statement to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_writeStmt(AST *stmt)
{
    scope_check_expr(stmt->data.write_stmt.exp);
}

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_cond(AST *cond)
{
    switch (cond->type_tag){
    case odd_cond_ast:
	scope_check_odd_cond(cond);
	break;
    case bin_cond_ast:
	scope_check_bin_cond(cond);
	break;
    default:
	bail_with_error("Unexpected type_tag (%d) in scope_check_cond (for line %d, column %d)!",
			cond->type_tag, cond->file_loc.line, cond->file_loc.column);
	break;
    }
}

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_odd_cond(AST *cond)
{
    scope_check_expr(cond->data.odd_cond.exp);
}

// check the condition to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_bin_cond(AST *cond)
{
    scope_check_expr(cond->data.bin_cond.leftexp);
    scope_check_expr(cond->data.bin_cond.rightexp);
}

// check the expresion to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_expr(AST *exp)
{
    switch (exp->type_tag) {
    case ident_ast:
	{   // must update the id_use in the AST
	    id_use *idu
		= scope_check_ident_is_data(exp->file_loc,
					    exp->data.ident.name);
	    exp->data.ident.idu = idu;
	}
	break;
    case bin_expr_ast:
	scope_check_bin_expr(exp);
	break;
    case number_ast:
	// no identifiers are possible in this case, so just return
	break;
    default:
	bail_with_error("Unexpected type_tag (%d) in scope_check_expr (for line %d, column %d)!",
			exp->type_tag, exp->file_loc.line, exp->file_loc.column);
	break;
    }
}

// check that the given name has been declared,
// if so, then return an id_use (containing the attributes) for that name,
// otherwise, produce an error using the file_location (floc) given.
static id_use *scope_check_ident_declared(file_location floc, const char *name)
{
    id_use *ret = symtab_lookup(name);
    if (ret == NULL) {
	general_error(floc,
		      "identifier \"%s\" is not declared!", name);
    }
    return ret;
}

// check that the given constant or variable name has been declared,
// if so, then return an id_use (containing the attributes) for that name,
// otherwise, produce an error using the file_location (floc) given.
id_use *scope_check_ident_is_data(file_location floc, const char *name)
{
    id_use *ret = scope_check_ident_declared(floc, name);
    if (ret->attrs->kind == procedure) {
	general_error(floc,
		      "identifier \"%s\" is a procedure name, not a constant or variable!", name);
    }
    return ret;
}

// check that the given procedure name has been declared,
// if so, then return an id_use (containing the attributes) for that name,
// otherwise, produce an error using the file_location (floc) given.
id_use *scope_check_ident_is_proc_id(file_location floc, const char *name)
{
    id_use *ret = scope_check_ident_declared(floc, name);
    if (ret->attrs->kind != procedure) {
	general_error(floc,
		      "identifier \"%s\" is not a procedure name", name);
    }
    return ret;
}

// check the expression (exp) to make sure that
// all idenfifiers referenced in it have been declared
// (if not, then produce an error)
void scope_check_bin_expr(AST *exp)
{
    scope_check_expr(exp->data.bin_expr.leftexp);
    scope_check_expr(exp->data.bin_expr.rightexp);
}
