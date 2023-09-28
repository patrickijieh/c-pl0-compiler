/* $Id: compiler_main.c,v 1.11 2023/03/23 02:57:55 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexer_output.h"
#include "parser.h"
#include "ast.h"
#include "symtab.h"
#include "scope_check.h"
#include "utilities.h"
#include "unparser.h"
#include "code.h"
#include "gen_code.h"

/* Print a usage message on stderr 
   and exit with failure. */
static void usage(const char *cmdname)
{
    fprintf(stderr, "Usage: %s %s\n       %s %s\n       %s %s\n",
	    cmdname, "-l codeFilename.pl0",
	    cmdname, "-u codeFilename.pl0",
	    cmdname, " codeFilename.pl0"
	    );
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    // should the lexer's tokens be shown
    bool lexer_print_output = false;
    bool parser_unparse = false;
    /* bool debug_asm = false; */
    const char *cmdname = argv[0];
    argc--;
    argv++;
    // possible options: -l, -d, and -u
    while (argc > 0 && strlen(argv[0]) >= 2 && argv[0][0] == '-') {
	if (strcmp(argv[0],"-l") == 0) {
	    lexer_print_output = true;
	    argc--;
	    argv++;
        /*} else if (strcmp(argv[0],"-d") == 0) {
	    debug_asm = true;
	    argc--;
	    argv++; */
	} else if (strcmp(argv[0],"-u") == 0) {
	    parser_unparse = true;
	    argc--;
	    argv++;
	} else {
	    // bad option!
	    usage(cmdname);
	}
    }

    // give usage message if -l and other options are used
    if (lexer_print_output && /* (debug_asm || */ parser_unparse/*)*/ ) {
	usage(cmdname);
    }

    /*
    // give usage message if -u and -d are both used
    if (parser_unparse && debug_asm) {
	usage(cmdname);
    }
    */

    // must have a file name
    if (argc <= 0 || (strlen(argv[0]) >= 2 && argv[0][0] == '-')) {
	usage(cmdname);
    }

    // the name of the file
    const char *filename = argv[0];

    if (lexer_print_output) {
	// with the lexer_print_output option, nothing else is done
	lexer_open(filename);
	lexer_output();
	lexer_close();
	return EXIT_SUCCESS;
    }

    // otherwise (if not lexer_print_outout) continue to parse etc.
    parser_open(filename);
    AST * progast = parseProgram();
    parser_close();

    if (parser_unparse) {
	unparseProgram(stdout, progast);
    }

    // build symbol table and...
    symtab_initialize();
    // check for duplicate declarations and proper use of names
    // this modifies progast
    scope_check_program(progast);

    if (parser_unparse) {
	return EXIT_SUCCESS;
    }

    // generate code from the ASTs
    gen_code_initialize();
    code_seq prog_code_seq = gen_code_program(progast);

    /* if (debug_asm) {
    	code_seq_debug_print(stdout, prog_code_seq);
       } else { */
    code_seq_vm_print(stdout, prog_code_seq);
    /* } */

    return EXIT_SUCCESS;
}
