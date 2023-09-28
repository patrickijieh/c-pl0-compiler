#ifndef _UNPARSERINTERNAL_H
#define _UNPARSERINTERNAL_H
#include "unparser.h"

static void unparseConstDecl(FILE *out, AST *cd, int level);

static void unparseVarDecl(FILE *out, AST *vd, int level);

static void unparseProcDecl(FILE *out, AST *pd, int level);

static void unparseAssignStmt(FILE *out, AST *stmt, int level,
			      bool addSemiToEnd);

static void unparseCallStmt(FILE *out, AST *stmt, int level,
			    bool addSemiToEnd);

static void unparseBeginStmt(FILE *out, AST *stmt, int level,
			     bool addSemiToEnd);

static void unparseStmtList(FILE *out, AST *stmt1, AST *rest,
			   int level, bool addSemiToEnd);

static void unparseIfStmt(FILE *out, AST *stmt, int level, bool addSemiToEnd);

static void unparseWhileStmt(FILE *out, AST* stmt, int level,
			     bool addSemiToEnd);

static void unparseReadStmt(FILE *out, AST *stmt, int level,
			    bool addSemiToEnd);

static void unparseWriteStmt(FILE *out, AST *stmt, int level,
			     bool addSemiToEnd);

static void unparseSkipStmt(FILE *out, int level, bool addSemiToEnd);

static void unparseOddCond(FILE *out, AST *cond);

static void unparseBinRelCond(FILE *out, AST *cond);

static void unparseBinExpr(FILE *out, AST *exp);

#endif
