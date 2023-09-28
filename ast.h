/* $Id: ast.h,v 1.9 2023/03/23 05:23:01 leavens Exp $ */
#ifndef _AST_H
#define _AST_H
#include <stdbool.h>
#include "token.h"
#include "file_location.h"
#include "id_use.h"
#include "label.h"

// types of ASTs (type tags)
typedef enum {
    program_ast, const_decl_ast, var_decl_ast, proc_decl_ast,
    assign_ast, call_ast, begin_ast,
    if_ast, while_ast, read_ast, write_ast, skip_ast,
    odd_cond_ast, bin_cond_ast, op_expr_ast, bin_expr_ast, 
    ident_ast, number_ast
} AST_type;

// forward declaration, so can use the type AST* below
typedef struct AST_s AST;
// lists of ASTs
typedef AST *AST_list;

// The following types for structs named N_t
// are used in the declaration of the AST_s struct below.
// The struct N_t is the type of information kept in the AST
// that is related to the nonterminal N in the abstract syntax.
// In addition there are two enum types declared before AST_s,
// one for relational operators (rel_op, which is used in the type cond_t,
// which is the struct related to the ASTs for <condition>)
// and one for binary arithmetic operators (bin_arith_op, which is used in
// the types op_expr_t and bin_exp_t, the latter being
// the struct related to the ASTs for <expr>).

// The AST type for both programs and blocks
// B ::= { CD } { VD } { PD} S
typedef struct {
    AST_list cds;
    AST_list vds;
    AST_list pds;
    AST *stmt;
} program_t;

// CD ::= const x n
typedef struct {
    const char *name;
    short int num_val;
} const_decl_t;

// VD ::= var x
typedef struct {
    const char *name;
} var_decl_t;

// PD ::= procedure x B
typedef struct {
    const char *name;
    AST *block;
    label *lab; // needed for code generation
} proc_decl_t;

// S ::= assign x E
typedef struct {
    AST *ident;
    AST *exp;
} assign_t;

// S ::= call x
typedef struct {
    AST *ident;
} call_t;

// S ::= begin { S }
typedef struct {
    AST_list stmts;
} begin_t;

// S ::= if C S1 S2
typedef struct {
    AST *cond;
    AST *thenstmt;
    AST *elsestmt;
} if_t;

// S ::= while C S
typedef struct {
    AST *cond;
    AST *stmt;
} while_t;

// S ::= read x
typedef struct {
    AST *ident;
} read_t;

// S ::= write E
typedef struct {
    AST *exp;
} write_t;

typedef struct {
} skip_t;

// r ::= = | <> | < | <= | > | >=
typedef enum {eqop, neqop, ltop, leqop, gtop, geqop} rel_op;

// C ::= odd E
typedef struct {
    AST *exp;
} odd_cond_t;

// C ::= E1 r E2
typedef struct {
    AST *leftexp;
    rel_op relop;
    AST *rightexp;
} bin_cond_t;

// o ::= + | - | * | /
typedef enum {addop, subop, multop, divop} bin_arith_op;

// E ::= o E
// The following is used for pairs of operator and an expression
// (when parsing terms and factors),
// but it is converted into some other kind of AST eventually
typedef struct {
    bin_arith_op arith_op;
    AST *exp;
} op_expr_t;

// E ::= E o E
typedef struct {
    AST *leftexp;
    bin_arith_op arith_op;
    AST *rightexp;
} bin_expr_t;

// E ::= x
// The ident_t struct holds intermediate representation information
// as well as the syntactic information. That is, parts of the
// idu field will be filled in during static analysis.
typedef struct {
    // name of a constant or variable
    const char *name;
    // idu will be set during static analysis, includes info for lexical addr
    id_use *idu;
} ident_t;

// E ::= n
typedef struct {
    short int value;
} number_t;

// The actual AST definition:
typedef struct AST_s {
    file_location file_loc;
    AST_list next;  // for lists
    AST_type type_tag;
    union AST_u {
	program_t program;
	const_decl_t const_decl;
	var_decl_t var_decl;
	proc_decl_t proc_decl;
	assign_t assign_stmt;
	call_t call_stmt;
	begin_t begin_stmt;
	if_t if_stmt;
	while_t while_stmt;
	read_t read_stmt;
	write_t write_stmt;
	skip_t skip_stmt;
	odd_cond_t odd_cond;
	bin_cond_t bin_cond;
	op_expr_t op_expr;
	bin_expr_t bin_expr;
	ident_t ident;
	number_t number;
    } data;
} AST;

// Return a (pointer to a) fresh AST for a program or a block,
// whose first token starts in the given file (fn), line (ln), and column (col),
// and which contains the given AST lists for const-decls (cds),
// var-decls (vds), proc-decls (pds), and an AST for the statement (stmt).
extern AST *ast_program(const char *fn, unsigned int ln, unsigned int col,
			AST_list cds, AST_list vds, AST_list pds, AST *stmt);

// Return a (pointer to a) fresh AST for a const definition
// with name ident and value num, which starts at the token t
extern AST *ast_const_def(token t, const char *ident, short int num);

// Return a (pointer to a) fresh AST for a var declaration
// with name ident, which starts at the token t
extern AST *ast_var_decl(token t, const char *ident);

// Return a (pointer to a) fresh AST for a procedure declaration
// with name ident, and block blck, which starts at the token t
extern AST *ast_proc_decl(token t, const char *ident, AST *blck);

// Return a (pointer to a) fresh AST for an assignment statement
// with name name and expression AST exp.
extern AST *ast_assign_stmt(token t, const char *name, AST *exp);

// Return a (pointer to a) fresh AST for a call statement
// with procedure name ident
extern AST *ast_call_stmt(token t, const char *name);

// Return a (pointer to a) fresh AST for a begin-statement
// with statments AST stmts.
extern AST *ast_begin_stmt(token t, AST_list stmts);

// Return a (pointer to a) fresh AST for an if-statement
// with condition AST cond, then part thenstmt, and else part elsestmt
extern AST *ast_if_stmt(token t, AST *cond, AST *thenstmt, AST *elsestmt);

// Return a (pointer to a) fresh AST for a while-statement
// with condition AST cond and body statement AST body.
extern AST *ast_while_stmt(token t, AST *cond, AST *body);

// Return a (pointer to a) fresh AST for a read-statement
// with variable identifier name
extern AST *ast_read_stmt(token t, const char *name);

// Return a (pointer to a) fresh AST for a write-statement
// with expression AST exp
extern AST *ast_write_stmt(token t, AST *exp);

// Return a (pointer to a) fresh AST for a skip statement
extern AST *ast_skip_stmt(token t);

// Return a (pointer to a) fresh AST for an odd condition
// with expression AST exp
extern AST *ast_odd_cond(token t, AST *exp);

// Return a (pointer to a) fresh AST for a binary condition
// with left expression AST e1, relational operator relop,
// and right expression e2
extern AST *ast_bin_cond(token t, AST *e1, rel_op relop, AST *e2);

// Return a (pointer to a) fresh AST for a pair of a binary operator, op,
// and a (right) expression e2
extern AST *ast_op_expr(token t, bin_arith_op op, AST *e2);

// Return a (pointer to a) fresh AST for a binary expression
// with left expresion AST e1, binary artihmetic operator arith_op,
// and right expression AST e2.
extern AST *ast_bin_expr(token t, AST *e1, bin_arith_op arith_op, AST *e2);

// Return a (pointer to a) fresh AST for an ident expression
// with the given name.
extern AST *ast_ident(token t, const char *name);

// Return a (pointer to a) fresh AST for an (signed) number expression
// with the given value
extern AST *ast_number(token t, short int value);

// Return an AST list that is empty
extern AST_list ast_list_empty_list();

// Return an AST list consisting of just the given AST node (ast)
extern AST_list ast_list_singleton(AST *ast);

// Return true just when lst is an empty list (and false otherwise)
extern bool ast_list_is_empty(AST_list lst);

// Requires: !ast_list_is_empty(lst)
// Return the first element in an AST_list
extern AST *ast_list_first(AST_list lst);

// Requires: !ast_list_is_empty(lst)
// Return the rest of the AST_list (which is null if it is empty)
extern AST_list ast_list_rest(AST_list lst);

// Requires: !ast_list_is_empty(lst) and ast_list_is_empty(ast_list_rest(lst))
// Make newtail the tail of the AST_list starting at lst
extern void ast_list_splice(AST_list lst, AST_list newtail);

// Return the last element in the AST list lst.
// The result is only NULL if ast_list_is_empty(lst);
extern AST_list ast_list_last_elem(AST_list lst);

// Return the number of elements in the AST list lst.
extern int ast_list_size(AST_list lst);

#endif
