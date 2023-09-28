/* $Id: ast.c,v 1.9 2023/03/23 05:36:03 leavens Exp $ */
#include <stdlib.h>
#include "utilities.h"
#include "ast.h"

// Return a (pointer to a) fresh AST
// and fill in its file_location with the given file name (fn),
// line number (ln) and column number (col).
// Also initializes the next pointer to NULL.
// If there is no space to allocate an AST node,
// print an error on stderr and exit with a failure code.
static AST *ast_allocate(const char *fn, unsigned int ln, unsigned int col)
{
    AST *ret = (AST *) malloc(sizeof(AST));
    if (ret == NULL) {
	bail_with_error("No space to create an AST!");
    }
    ret->file_loc.filename = fn;
    ret->file_loc.line = ln;
    ret->file_loc.column = col;
    ret->next = NULL;
    return ret;
}

// Return a (pointer to a) fresh AST for a program or a block,
// whose first token starts in the given file (fn), line (ln), and column (col),
// and which contains the given AST lists for const-decls (cds),
// var-decls (vds), proc-decls (pds), and an AST for the statement (stmt).
AST *ast_program(const char *fn, unsigned int ln, unsigned int col,
		 AST_list cds, AST_list vds, AST_list pds, AST *stmt)
{
    AST *ret = ast_allocate(fn, ln, col);
    ret->type_tag = program_ast;
    ret->data.program.cds = cds;
    ret->data.program.vds = vds;
    ret->data.program.pds = pds;
    ret->data.program.stmt = stmt;
    return ret;
}

// Return a (pointer to a) fresh AST for a const definition
// with name ident and value num
AST *ast_const_def(token t, const char *ident, short int num)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = const_decl_ast;
    ret->data.const_decl.name = ident;
    ret->data.const_decl.num_val = num;
    return ret;
}

// Return a (pointer to a) fresh AST for a var declaration
// with name ident.
AST *ast_var_decl(token t, const char *ident)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = var_decl_ast;
    ret->data.var_decl.name = ident;
    return ret;
}


// Return a (pointer to a) fresh AST for a procedure declaration
// with name ident, and block blck, which starts at the token t
AST *ast_proc_decl(token t, const char *ident, AST *blck)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = proc_decl_ast;
    ret->data.proc_decl.name = ident;
    ret->data.proc_decl.block = blck;
    ret->data.proc_decl.lab = label_create();
    return ret;
}

// Return a (pointer to a) fresh AST for an assignment statement
// with the given name and expression (exp).
AST *ast_assign_stmt(token t, const char *name, AST *exp)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    AST *ident = ast_allocate(t.filename, t.line, t.column);
    ident->type_tag = ident_ast;
    ident->data.ident.name = name;
    id_attrs *attrs = id_attrs_start(token2file_loc(t));
    ident->data.ident.idu = id_use_create(attrs, 0);
    ret->type_tag = assign_ast;
    ret->data.assign_stmt.ident = ident;
    ret->data.assign_stmt.exp = exp;
    return ret;
}

// Return a (pointer to a) fresh AST for a call statement
// to the given procedure name
AST *ast_call_stmt(token t, const char *name)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    AST *ident = ast_allocate(t.filename, t.line, t.column);
    ident->type_tag = ident_ast;
    ident->data.ident.name = name;
    id_attrs *attrs = id_attrs_start(token2file_loc(t));
    ident->data.ident.idu = id_use_create(attrs, 0);
    ret->type_tag = call_ast;
    ret->data.call_stmt.ident = ident;
    return ret;
}

// Return a (pointer to a) fresh AST for a begin-statement
// with statments AST stmts.
AST *ast_begin_stmt(token t, AST *stmts)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = begin_ast;
    ret->data.begin_stmt.stmts = stmts;
    return ret;
}

// Return a (pointer to a) fresh AST for an if-statement
// with condition AST cond, then part thenstmt, and else part elsestmt
AST *ast_if_stmt(token t, AST *cond, AST *thenstmt, AST *elsestmt)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = if_ast;
    ret->data.if_stmt.cond = cond;
    ret->data.if_stmt.thenstmt = thenstmt;
    ret->data.if_stmt.elsestmt = elsestmt;
    return ret;
}

// Return a (pointer to a) fresh AST for a while-statement
// with condition AST cond and body statement AST body.
AST *ast_while_stmt(token t, AST *cond, AST *body)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = while_ast;
    ret->data.while_stmt.cond = cond;
    ret->data.while_stmt.stmt = body;
    return ret;
}

// Return a (pointer to a) fresh AST for a read-statement
// with variable identifier name
AST *ast_read_stmt(token t, const char *name)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    AST *ident = ast_allocate(t.filename, t.line, t.column);
    ident->type_tag = ident_ast;
    ident->data.ident.name = name;
    id_attrs *attrs = id_attrs_start(token2file_loc(t));
    ident->data.ident.idu = id_use_create(attrs, 0);
    ret->type_tag = read_ast;
    ret->data.read_stmt.ident = ident;
    return ret;
}

// Return a (pointer to a) fresh AST for a write-statement
// with expression AST exp
AST *ast_write_stmt(token t, AST *exp)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = write_ast;
    ret->data.write_stmt.exp = exp;
    return ret;
}

// Return a (pointer to a) fresh AST for a skip statement
AST *ast_skip_stmt(token t)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = skip_ast;
    return ret;
}

// Return a (pointer to a) fresh AST for an odd condition
// with expression AST exp
AST *ast_odd_cond(token t, AST *exp)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = odd_cond_ast;
    ret->data.odd_cond.exp = exp;
    return ret;
}

// Return a (pointer to a) fresh AST for a binary condition
// with left expression AST e1, relational operator relop,
// and right expression e2
AST *ast_bin_cond(token t, AST *e1, rel_op relop, AST *e2)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = bin_cond_ast;
    ret->data.bin_cond.leftexp = e1;
    ret->data.bin_cond.relop = relop;
    ret->data.bin_cond.rightexp = e2;
    return ret;
}

// Return a (pointer to a) fresh AST for a pair of a binary operator, op,
// and a (right) expression e2
AST *ast_op_expr(token t, bin_arith_op op, AST *e2)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->data.op_expr.arith_op = op;
    ret->data.op_expr.exp = e2;
    return ret;
}

// Return a (pointer to a) fresh AST for a binary expression
// with left expresion AST e1, binary artihmetic operator arith_op,
// and right expression AST e2.
AST *ast_bin_expr(token t, AST *e1, bin_arith_op arith_op, AST *e2)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = bin_expr_ast;
    ret->data.bin_expr.leftexp = e1;
    ret->data.bin_expr.arith_op = arith_op;
    ret->data.bin_expr.rightexp = e2;
    return ret;
}

// Return a (pointer to a) fresh AST for an identref expression
// with the given name.
AST *ast_ident(token t, const char *name)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = ident_ast;
    ret->data.ident.name = name;
    id_attrs *attrs = id_attrs_start(token2file_loc(t));
    ret->data.ident.idu = id_use_create(attrs, 0);
    return ret;
}

// Return a (pointer to a) fresh AST for an (signed) number expression
// with the given value
AST *ast_number(token t, short int value)
{
    AST *ret = ast_allocate(t.filename, t.line, t.column);
    ret->type_tag = number_ast;
    ret->data.number.value = value;
    return ret;
}

// Return an AST list that is empty
AST_list ast_list_empty_list()
{
    return NULL;
}

// Return an AST list consisting of just the given AST node (ast)
AST_list ast_list_singleton(AST *ast)
{
    return ast;
}

// Return true just when lst is an empty list (and false otherwise)
bool ast_list_is_empty(AST_list lst)
{
    return lst == NULL;
}

// Requires lst != NULL
// Return the first element in an AST_list
AST *ast_list_first(AST_list lst)
{
    // each node is an AST itself, so the first element is lst
    return lst;
}

// Requires lst != NULL
// Return the rest of the AST_list (which is null if it is empty)
AST_list ast_list_rest(AST_list lst)
{
    return lst->next;
}


// Return the last element in the AST list lst.
// The result is only NULL if ast_list_is_empty(lst);
AST_list ast_list_last_elem(AST_list lst)
{
    AST *prev = NULL;
    while (!ast_list_is_empty(lst)) {
	prev = lst;
	lst = ast_list_rest(lst);
    }
    // here ast_list_is_empty(lst)
    return prev;
}

// Requires: !ast_list_is_empty(lst) and ast_list_is_empty(ast_list_rest(lst))
// Make newtail the tail of the AST_list starting at lst
void ast_list_splice(AST_list lst, AST_list newtail)
{
    lst->next = newtail;
}

// Return the number of elements in the AST list lst.
extern int ast_list_size(AST_list lst)
{
    int ret = 0;
    while (!ast_list_is_empty(lst)) {
	ret++;
	lst = ast_list_rest(lst);
    }
    return ret;
}
