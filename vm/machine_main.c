/* $Id: machine_main.c,v 1.8 2023/03/22 14:16:36 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "instruction.h"
#include "machine.h"

/* Print a usage message on stderr 
   and exit with failure. */
static void usage(const char *cmdname)
{
    fprintf(stderr,
	    "Usage: %s [-n] code-filename\n",
	    cmdname);
    exit(EXIT_FAILURE);
}

int main(int argc, char *argv[])
{
    extern void machine(const char *filename);

    const char *cmdname = argv[0];
    argc--;
    argv++;
    // default is to print the program and do tracing
    tracing = true;
    // possible option: -n
    if (argc > 1 && argv[0][0] == '-') {
	// -n turns off tracing
	if (strcmp(argv[0], "-n") == 0) {
	    tracing = false;
	    argc--;
	    argv++;
	} else {
	    usage(cmdname);
	}
    }
    
    /* process file name argument */
    if (argc != 1 || argv[0][0] == '-') {
	    usage(cmdname);
    }
    machine(argv[0]);
    return EXIT_SUCCESS;
}
