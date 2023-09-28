/* $Id: unparser.c,v 1.8 2023/03/23 05:39:43 leavens Exp $ */
#include <stdio.h>
#include "ast.h"
#include "utilities.h"
#include "unparserInternal.h"

// Amount of spaces to indent per nesting level
#define SPACES_PER_LEVEL 2

// Print SPACES_PER_LEVEL * level spaces to out
static void indent(FILE *out, int level)
{
    int num = SPACES_PER_LEVEL * level;
    for (int i = 0; i < num; i++) {
	fprintf(out, " ");
    }
}

// Unparse the given program AST and then print a period and an newline
void unparseProgram(FILE *out, AST *ast)
{
    unparseBlock(out, ast, 0, false);
    fprintf(out, ".\n");
}

// Unparse the given block, indented by the given level, to out
// adding a semicolon to the end if addSemiToENd is true.
void unparseBlock(FILE *out, AST *ast, int level,
		  bool addSemiToEnd)
{
    AST_list cds = ast->data.program.cds;
    AST_list vds = ast->data.program.vds;
    AST_list pds = ast->data.program.pds;
    AST *stmt = ast->data.program.stmt;
    unparseConstDecls(out, cds, level);
    unparseVarDecls(out, vds, level);
    unparseProcDecls(out, pds, level);
    unparseStmt(out, stmt, level, addSemiToEnd);
}

// Unparse the list of const-decls given by the AST cds to out
// with the given nesting level
// (note that if cds == NULL, then nothing is printed)
void unparseConstDecls(FILE *out, AST_list cds, int level)
{
    while (!ast_list_is_empty(cds)) {
	unparseConstDecl(out, ast_list_first(cds), level);
	cds = ast_list_rest(cds);
    }
}

// Unparse a single const-def given by the AST cd to out,
// indented for the given nesting level
static void unparseConstDecl(FILE *out, AST *cd, int level)
{
    indent(out, level);
    fprintf(out, "const ");
    fprintf(out, "%s = ", cd->data.const_decl.name);
    fprintf(out, "%d;\n", cd->data.const_decl.num_val);
}

// Unparse the list of vart-decls given by the AST vds to out
// with the given nesting level
// (note that if vds == NULL, then nothing is printed)
void unparseVarDecls(FILE *out, AST_list vds, int level)
{
    while (!ast_list_is_empty(vds)) {
	unparseVarDecl(out, ast_list_first(vds), level);
	vds = ast_list_rest(vds);
    }
}

// Unparse a single var-decl given by the AST vd to out,
// indented for the given nesting level
static void unparseVarDecl(FILE *out, AST *vd, int level)
{
    indent(out, level);
    fprintf(out, "var ");
    fprintf(out, "%s;\n", vd->data.var_decl.name);
}

// Unparse the list of proc-decls given by the AST pds to out
// with the given nesting level
// (note that if pds == NULL, then nothing is printed)
void unparseProcDecls(FILE *out, AST_list pds, int level)
{
    while (!ast_list_is_empty(pds)) {
	unparseProcDecl(out, ast_list_first(pds), level);
	pds = ast_list_rest(pds);
    }
}

// Unparse a single proc-decl given by the AST pd to out,
// indented for the given nesting level
static void unparseProcDecl(FILE *out, AST *pd, int level)
{
    indent(out, level);
    fprintf(out, "procedure ");
    fprintf(out, "%s;\n", pd->data.proc_decl.name);
    unparseBlock(out, pd->data.proc_decl.block, level+1, true);
}


// Print (to out) a semicolon, but only if addSemiToEnd is true,
// and then print a newline.
static void newlineAndOptionalSemi(FILE *out, bool addSemiToEnd)
{
    fprintf(out, "%s\n", (addSemiToEnd ? ";" : ""));
}

// Unparse the statement given by the AST stmt to out,
// indented for the given level,
// adding a semicolon to the end if addSemiToENd is true.
void unparseStmt(FILE *out, AST *stmt, int indentLevel, bool addSemiToEnd)
{
    switch (stmt->type_tag) {
    case assign_ast:
	unparseAssignStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case call_ast:
	unparseCallStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case begin_ast:
	unparseBeginStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case if_ast:
	unparseIfStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case while_ast:
	unparseWhileStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case read_ast:
	unparseReadStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case write_ast:
	unparseWriteStmt(out, stmt, indentLevel, addSemiToEnd);
	break;
    case skip_ast:
	unparseSkipStmt(out, indentLevel, addSemiToEnd);
	break;
    default:
	bail_with_error("Call to unparseStmt with an AST that is not a statement!");
	break;
    }
}

// Unparse the assignment statment given by stmt to out
// with indentation level given by level,
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseAssignStmt(FILE *out, AST *stmt, int level,
			      bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "%s := ", stmt->data.assign_stmt.ident->data.ident.name);
    unparseExpr(out, stmt->data.assign_stmt.exp);
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the call statment given by stmt to out
// with indentation level given by level,
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseCallStmt(FILE *out, AST *stmt, int level,
			      bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "call %s", stmt->data.call_stmt.ident->data.ident.name);
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the sequential statment given by stmt to out
// with indentation level given by level (indenting the body one more level)
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseBeginStmt(FILE *out, AST *stmt, int level,
			     bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "begin\n");
    AST *stmt1 = ast_list_first(stmt->data.begin_stmt.stmts);
    AST_list rest = ast_list_rest(stmt->data.begin_stmt.stmts);
    unparseStmtList(out, stmt1, rest, level+1, false);
    indent(out, level);
    fprintf(out, "end");
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the list of statments given by stmt to out
// with indentation level given by level,
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseStmtList(FILE *out, AST *stmt1, AST_list rest,
			   int level, bool addSemiToEnd)
{
    unparseStmt(out, stmt1, level, !ast_list_is_empty(rest));
    while (!ast_list_is_empty(rest)) {
	AST_list nxt = ast_list_rest(rest);
	unparseStmt(out, rest, level, !ast_list_is_empty(nxt));
	rest = nxt;
    }
}

// Unparse the if-statment given by stmt to out
// with indentation level given by level (and each body indented one more),
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseIfStmt(FILE *out, AST *stmt, int level, bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "if ");
    unparseCondition(out, stmt->data.if_stmt.cond);
    fprintf(out, "\n");
    indent(out, level);
    fprintf(out, "then\n");
    unparseStmt(out, stmt->data.if_stmt.thenstmt, level+1, false);
    indent(out, level);
    fprintf(out, "else\n");
    unparseStmt(out, stmt->data.if_stmt.elsestmt, level+1, addSemiToEnd);
}

// Unparse the while-statment given by stmt to out
// with indentation level given by level (and the body indented one more),
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseWhileStmt(FILE *out, AST* stmt, int level, bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "while ");
    unparseCondition(out, stmt->data.while_stmt.cond);
    fprintf(out, "\n");
    indent(out, level);
    fprintf(out, "do\n");
    unparseStmt(out, stmt->data.while_stmt.stmt, level+1, addSemiToEnd);
}

// Unparse the read statment given by stmt to out
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseReadStmt(FILE *out, AST *stmt, int level, bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "read %s", stmt->data.read_stmt.ident->data.ident.name);
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the write statment given by stmt to out
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseWriteStmt(FILE *out, AST *stmt, int level, bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "write ");
    unparseExpr(out, stmt->data.write_stmt.exp);
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the write statment given by stmt to out
// and add a semicolon at the end if addSemiToEnd is true.
static void unparseSkipStmt(FILE *out, int level, bool addSemiToEnd)
{
    indent(out, level);
    fprintf(out, "skip");
    newlineAndOptionalSemi(out, addSemiToEnd);
}

// Unparse the condition given by cond to out
void unparseCondition(FILE *out, AST *cond)
{
    switch (cond->type_tag) {
    case odd_cond_ast:
	unparseOddCond(out, cond);
	break;
    case bin_cond_ast:
	unparseBinRelCond(out, cond);
	break;
    default:
	bail_with_error("Unexpected type tag %d in unparseCondition!",
			cond->type_tag);
	break;
    }
}

// Unparse the odd condition given by cond to out
static void unparseOddCond(FILE *out, AST *cond)
{
    fprintf(out, "odd ");
    unparseExpr(out, cond->data.odd_cond.exp);
}

// Unparse the binary relation condition given by cond to out
static void unparseBinRelCond(FILE *out, AST *cond)
{
    unparseExpr(out, cond->data.bin_cond.leftexp);
    fprintf(out, " ");
    unparseRelOp(out, cond->data.bin_cond.relop);
    fprintf(out, " ");
    unparseExpr(out, cond->data.bin_cond.rightexp);
}

// Unparse the given relational operator, relop, to out
void unparseRelOp(FILE *out, rel_op relop)
{
    switch (relop) {
    case eqop:
	fprintf(out, "=");
	break;
    case neqop:
	fprintf(out, "<>");
	break;
    case ltop:
	fprintf(out, "<");
	break;
    case leqop:
	fprintf(out, "<=");
	break;
    case gtop:
	fprintf(out, ">");
	break;
    case geqop:
	fprintf(out, ">=");
	break;
    default:
	bail_with_error("Unknown rel_op %d", relop);
	break;
    }
}

// Unparse the expression given by the AST exp to out
// adding parentheses to indicate the nesting relationships
void unparseExpr(FILE *out, AST *exp)
{
    switch (exp->type_tag) {
    case bin_expr_ast:
	unparseBinExpr(out, exp);
	break;
    case ident_ast:
	unparseIdent(out, exp);
	break;
    case number_ast:
	unparseNumber(out, exp);
	break;
    default:
	bail_with_error("Unexpected type_tag %d in unparseExpr", exp->type_tag);
	break;
    }
}

// Unparse the expression given by the AST exp to out
// adding parentheses (whether needed or not)
static void unparseBinExpr(FILE *out, AST *exp)
{
    fprintf(out, "(");
    unparseExpr(out, exp->data.bin_expr.leftexp);
    fprintf(out, " ");
    unparseArithOp(out, exp->data.bin_expr.arith_op);
    fprintf(out, " ");
    unparseExpr(out, exp->data.bin_expr.rightexp);
    fprintf(out, ")");
}

// Unparse the given bin_arith_opo to out
void unparseArithOp(FILE *out, bin_arith_op op)
{
    switch (op) {
    case addop:
	fprintf(out, "+");
	break;
    case subop:
	fprintf(out, "-");
	break;
    case multop:
	fprintf(out, "*");
	break;
    case divop:
	fprintf(out, "/");
	break;
    default:
	bail_with_error("Unexpected bin_arith_op %d in unparseArithOp", op);
	break;
    }
}

// Unparse the given identifier reference (use) to out
void unparseIdent(FILE *out, AST *id)
{
    fprintf(out, "%s", id->data.ident.name);
}

// Unparse the given number to out in decimal format
void unparseNumber(FILE *out, AST *num)
{
    fprintf(out, "%d", num->data.number.value);
}
