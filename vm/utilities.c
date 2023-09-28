/* $Id: utilities.c,v 1.7 2023/01/25 14:11:04 leavens Exp $ */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "utilities.h"

// Format a string error message and print it followed by a newline on stderr
// using perror (for an OS error, if the errno is not 0)
// then exit with a failure code, so a call to this does not return.
void bail_with_error(const char *fmt, ...)
{
    extern int errno;
    fflush(stdout); // flush so output comes after what has happened already
    va_list(args);
    va_start(args, fmt);
    char buff[2048];
    vsprintf(buff, fmt, args);
    if (errno != 0) {
	perror(buff);
    } else {
	fprintf(stderr, "%s\n", buff);
    }
    exit(EXIT_FAILURE);
}
