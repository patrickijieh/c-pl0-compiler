/* $Id: parser.c,v 1.4 2023/03/19 01:17:21 leavens Exp $ */
#include <stdio.h>
#include "lexer.h"
#include "token.h"
#include "id_attrs.h"
#include "scope.h"
#include "utilities.h"
#include "parserInternal.h"

static token tok;

// initialize the parser to work on the given file
void parser_open(const char *filename)
{
    lexer_open(filename);
    tok = lexer_next();
}

// finish using the parser
void parser_close()
{
    lexer_close();
}

// Advance to the next token
// (if the lexer is not done,
//  otherwise do nothing)
static void advance()
{
    if (!lexer_done()) {
	tok = lexer_next();
    }
}

// Check that the current token is of type tt
// and advance to the next token if so;
// otherwise, produce a syntax error.
static void eat(token_type tt) {
    if (tok.typ == tt) {
	// debug_print("Eating a %s token\n", ttyp2str(tt));
        advance();
    } else {
	token_type expected[1] = {tt};
	parse_error_unexpected(expected, 1, tok);
    }
}

// <program> ::= <block> .
AST *parseProgram()
{
    AST * ast = parseBlock();
    eat(periodsym);
    eat(eofsym);
    return ast;
}

// <block> ::= <const-decls> <var-decls> <stmt>
AST *parseBlock()
{
    AST_list cds = parseConstDecls();
    AST_list vds = parseVarDecls();
    AST_list pds = parseProcDecls();
    AST *stmt = parseStmt();
    file_location floc;
    if (!ast_list_is_empty(cds)) {
	if (ast_list_first(cds)->type_tag == const_decl_ast) {
	    floc = ast_list_first(cds)->file_loc;
	} else {
	    bail_with_error("Bad AST for const declarations");
	}
    } else if (!ast_list_is_empty(vds)) {
	if (ast_list_first(vds)->type_tag == var_decl_ast) {
	    floc = ast_list_first(vds)->file_loc;
	} else {
	    bail_with_error("Bad AST for var declarations");
	}
    } else if (!ast_list_is_empty(pds)) {
	if (ast_list_first(pds)->type_tag == proc_decl_ast) {
	    floc = ast_list_first(pds)->file_loc;
	} else {
	    bail_with_error("Bad AST for proc declarations");
	}
    } else {
	floc = stmt->file_loc;
    }
    return ast_program(floc.filename, floc.line, floc.column,
		       cds, vds, pds, stmt);
}

// Requires: !ast_list_is_empty(*head) ==> !ast_list_is_empty(*last).
// Requires: when called head points to the first element of an AST_list
// and last points to the last element in that list.
// Modifies *head, *last;
// Splice the list starting at lst into the AST list starting at *head,
// and make *last point to the last element in the resulting list.
static void add_AST_to_end(AST_list *head, AST_list *last, AST_list lst)
{
    if (ast_list_is_empty(*head)) {
	*head = lst;
	*last = ast_list_last_elem(lst);
    } else {
	// assert(*last != NULL);
	ast_list_splice(*last, lst);
	*last = ast_list_last_elem(lst);
    }
}

// <const-decls> ::= { <const-decl> }
static AST_list parseConstDecls()
{
    AST_list ret = ast_list_empty_list();
    AST_list last = ast_list_empty_list();
    // invariant: !ast_list_is_empty(ret) ==> !ast_list_is_empty(last)
    while (tok.typ == constsym) {
	AST *cdast = parseConstDecl();
	add_AST_to_end(&ret, &last, ast_list_singleton(cdast));
    }
    return ret;
}

// <const-decl> ::= const <const-def> { <comma-const-def> }
static AST *parseConstDecl()
{
    eat(constsym);
    AST *cd = parseConstDef();
    AST *ret = cd;
    AST *last = cd;
    while (tok.typ == commasym) {
	eat(commasym);
	AST *nxt_cd = parseConstDef();
	add_AST_to_end(&ret, &last, ast_list_singleton(nxt_cd));
    }
    eat(semisym);
    return ret;
}

// <const-def> ::= <ident> = <number>
static AST *parseConstDef()
{
    token ident = tok;
    eat(identsym);
    eat(eqsym);
    short int numval = tok.value;
    eat(numbersym);
    AST *ret = ast_const_def(ident, ident.text, numval);
    return ret;
}

// <var-decls> ::= { <var-decl> }
static AST_list parseVarDecls()
{
    AST_list ret = ast_list_empty_list();
    AST_list last = ast_list_empty_list();
    // invariant: !ast_list_is_empty(ret) ==> !ast_list_is_empty(last)
    while (tok.typ == varsym) {
	AST_list vdasts = parseVarDecl();
	add_AST_to_end(&ret, &last, vdasts);
    }
    return ret;
}

// <var-decl> ::= var <idents> ;
static AST_list parseVarDecl()
{
    eat(varsym);
    AST_list vds = parseIdents();
    // not setting vds->file_loc to the location of the varsym
    eat(semisym);
    return vds;
}

// <idents> ::= <ident> { <comma-ident> }
static AST_list parseIdents()
{
    token idtok = tok;
    eat(identsym);
    AST_list ret = ast_list_singleton(ast_var_decl(idtok, idtok.text));
    AST_list last = ret;
    while (tok.typ == commasym) {
	eat(commasym);
	token idtok = tok;
	eat(identsym);
	AST *vd = ast_var_decl(idtok, idtok.text);
	add_AST_to_end(&ret, &last, ast_list_singleton(vd));
    }
    return ret;
}

// <proc-decls> ::= { <proc-decl> }
static AST_list parseProcDecls()
{
    AST_list ret = ast_list_empty_list();
    AST_list last = ast_list_empty_list();
    // invariant: !ast_list_is_empty(ret) ==> !ast_list_is_empty(last)
    while (tok.typ == procsym) {
	AST *pdast = parseProcDecl();
	add_AST_to_end(&ret, &last, ast_list_singleton(pdast));
    }
    return ret;
}

// <proc-decl> ::= procedure <ident> ; <block> ;
static AST *parseProcDecl()
{
    token ptok = tok;
    eat(procsym);
    const char *name = tok.text;
    eat(identsym);
    eat(semisym);
    AST *blck = parseBlock();
    eat(semisym);
    return ast_proc_decl(ptok, name, blck);
}

// <stmt> ::= <ident> := <expr> | ...
AST *parseStmt()
{
    AST *ret = NULL;
    switch (tok.typ) {
    case identsym:
	ret = parseAssignStmt();
	break;
    case callsym:
	ret = parseCallStmt();
	break;
    case beginsym:
	ret = parseBeginStmt();
	break;
    case ifsym:
	ret = parseIfStmt();
	break;
    case whilesym:
	ret = parseWhileStmt();
	break;
    case readsym:
	ret = parseReadStmt();
	break;
    case writesym:
	ret = parseWriteStmt();
	break;
    case skipsym:
	ret = parseSkipStmt();
	break;
    default:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token_type expected[8] =
	    {identsym, callsym, beginsym, ifsym, whilesym, readsym, writesym, skipsym};
	parse_error_unexpected(expected, 8, tok);
    }
    return ret;
}

// <assignment> ::= <ident> := <expr>
static AST *parseAssignStmt()
{
    token idtok = tok;
    eat(identsym);
    eat(becomessym);
    AST *exp = parseExpr();
    return ast_assign_stmt(idtok, idtok.text, exp);
}

// <call-stmt> ::= call <ident>
static AST *parseCallStmt()
{
    token ctok = tok;
    eat(callsym);
    const char *name = tok.text;
    eat(identsym);
    return ast_call_stmt(ctok, name);
}

// <begin-stmt> ::= begin <stmt> { <semi-stmt> } end
static AST_list parseBeginStmt()
{
    token btok = tok;
    eat(beginsym);
    AST_list stmts = ast_list_singleton(parseStmt());
    AST_list last = stmts;
    while (tok.typ == semisym) {
	AST *stmt = parseSemiStmt();
	add_AST_to_end(&stmts, &last, ast_list_singleton(stmt));
    }
    eat(endsym);
    AST *ret = ast_begin_stmt(btok, stmts);
    return ret;
}

// <semi-stmt> ::= ; <stmt>
static AST *parseSemiStmt()
{
    eat(semisym);
    return parseStmt();
}

// <if-stmt> ::= if <condition> then <stmt> else <stmt>
static AST *parseIfStmt()
{
    token ift = tok;
    eat(ifsym);
    AST *cond = parseCondition();
    eat(thensym);
    AST *s1 = parseStmt();
    eat(elsesym);
    AST *s2 = parseStmt();
    return ast_if_stmt(ift, cond, s1, s2);
}

// <while-stmt> ::= while <condition> do <stmt>
static AST *parseWhileStmt()
{
    token wt = tok;
    eat(whilesym);
    AST *cond = parseCondition();
    eat(dosym);
    AST *stmt = parseStmt();
    return ast_while_stmt(wt, cond, stmt);
}

// <read-stmt> ::= read <ident>
static AST *parseReadStmt()
{
    token rt = tok;
    eat(readsym);
    const char *name = tok.text;
    eat(identsym);
    return ast_read_stmt(rt, name);
}

// <write-stmt> ::= write <expr>
static AST *parseWriteStmt()
{
    token wt = tok;
    eat(writesym);
    AST * exp = parseExpr();
    return ast_write_stmt(wt, exp);
}

// <skip-stmt> ::= skip
static AST *parseSkipStmt()
{
    token skt = tok;
    eat(skipsym);
    return ast_skip_stmt(skt);
}

// <condition> ::= <odd-cond> | <bin-rel-cond>
AST *parseCondition()
{
    AST *ret;
    switch (tok.typ) {
    case oddsym:
	ret = parseOddCond();
	break;
    default:
	ret = parseBinRelCond();
	break;
    }
    return ret;
}

// <odd-cond> ::= odd <expr>
static AST *parseOddCond()
{
    token ot = tok;
    eat(oddsym);
    AST *exp = parseExpr();
    return ast_odd_cond(ot, exp);
}

// <bin-rel-cond> ::= <expr> <rel-op> <expr>
static AST *parseBinRelCond()
{
    token fst = tok;
    AST *e1 = parseExpr();
    rel_op r = parseRelOp();
    AST *e2 = parseExpr();
    AST *ret = ast_bin_cond(tok, e1, r, e2);
    ret->file_loc = token2file_loc(fst);
    return ret;
}

// <rel-op> ::= = | <> | < | <= | > | >=
static rel_op parseRelOp()
{
    switch (tok.typ) {
    case eqsym:
	eat(eqsym);
	return eqop;
	break;
    case neqsym:
	eat(neqsym);
	return neqop;
	break;
    case lessym:
	eat(lessym);
	return ltop;
	break;
    case leqsym:
	eat(leqsym);
	return leqop;
	break;
    case gtrsym:
	eat(gtrsym);
	return gtop;
	break;
    case geqsym:
	eat(geqsym);
	return geqop;
	break;
    default:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token_type expected[6] = {eqsym, neqsym, lessym, leqsym, gtrsym, geqsym};
	parse_error_unexpected(expected, 6, tok);
	break;
    }
    // The follow should never execute
    return (rel_op) 0;
}

// <expr> ::= <term> { <add-sub-term> }
// Return an AST with the operators (if any) associating to the left
AST *parseExpr()
{
    token fst = tok;
    AST *trm = parseTerm();
    AST *exp = trm;
    while (tok.typ == plussym || tok.typ == minussym) {
	AST *rght = parseAddSubTerm();
	exp = ast_bin_expr(fst, exp, rght->data.op_expr.arith_op,
			   rght->data.op_expr.exp);
    }
    return exp;
}
// <add-sub-term> ::= <add-sub> <term>
static AST *parseAddSubTerm()
{
    token opt = tok;
    switch (tok.typ) {
    case plussym:
	eat(plussym);
	AST *exp = parseTerm();
	return ast_op_expr(opt, addop, exp);
	break;
    case minussym:
	eat(minussym);
	AST *e = parseTerm();
	return ast_op_expr(opt, subop, e);
	break;
    default:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token_type expected[2] = {plussym, minussym};
	parse_error_unexpected(expected, 2, tok);
	break;
    }
    // The following should never execute
    return (AST *) NULL;
}

// <term> ::= <factor> { <mult-div-factor> }
// Return an AST with the operators (if any) associating to the left
static AST *parseTerm()
{
    token fst = tok;
    AST *fac = parseFactor();
    AST *exp = fac;
    while (tok.typ == multsym || tok.typ == divsym) {
	AST *rght = parseMultDivFactor();
	exp = ast_bin_expr(fst, exp, rght->data.op_expr.arith_op,
			   rght->data.op_expr.exp);
    }
    return exp;
}

// <mult-div-factor> ::= <mult-div> <factor>
static AST *parseMultDivFactor()
{
    token opt = tok;
    switch (tok.typ) {
    case multsym:
	eat(multsym);
	AST *exp = parseFactor();
	return ast_op_expr(opt, multop, exp);
	break;
    case divsym:
	eat(divsym);
	AST *e = parseFactor();
	return ast_op_expr(opt, divop, e);
	break;
    default:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token_type expected[2] = {multsym, divsym};
	parse_error_unexpected(expected, 2, tok);
	break;
    }
    // The following should never execute
    return (AST *) NULL;
}

// <factor> ::= <ident> | <paren-expr> | <signed-number>
static AST *parseFactor()
{
    switch (tok.typ) {
    case identsym:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token idt = tok;
	eat(identsym);
	return ast_ident(idt, idt.text);
	break;
    case lparensym:
	return parseParenExpr();
	break;
    case plussym: case minussym: case numbersym:
	return parseSignedNumber();
	break;
    default:
	;  // empty statement needed so label isn't immediately
	// followed by a declaration, which is prohibited in C;
	token_type expected[5] =
	    {identsym, lparensym, plussym, minussym, numbersym};
	parse_error_unexpected(expected, 5, tok);
	break;
    }
    // The following should never execute
    return (AST *)NULL;
}

// <paren-expr> ::= ( <expr> )
static AST *parseParenExpr()
{
    token lpt = tok;
    eat(lparensym);
    AST *ret = parseExpr();
    eat(rparensym);
    ret->file_loc = token2file_loc(lpt);
    return ret;
}

// <signed-number> ::= + <number> | - <number> | <number>
static AST *parseSignedNumber()
{
    bool negate = false;
    token fst = tok;
    switch (tok.typ) {
    case plussym:
	eat(plussym);
	break;
    case minussym:
	eat(minussym);
	negate=true;
	break;
    default:
	break;
    }
    token numt = tok;
    eat(numbersym);
    short int val = numt.value;
    if (negate) {
	val = - val;
    }
    return ast_number(fst, val);
}
