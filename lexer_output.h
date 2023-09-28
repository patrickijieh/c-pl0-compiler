/* $Id: lexer_output.h,v 1.1 2023/03/08 15:18:43 leavens Exp $ */
#ifndef _LEXER_OUTPUT_H
#define _LEXER_OUTPUT_H
#include "lexer.h"

// Requires: the lexer is not done
// Output to stdout a table
// of all the tokens read from the lexer's input file
extern void lexer_output();
#endif
