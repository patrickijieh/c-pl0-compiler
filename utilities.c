/* $Id: utilities.c,v 1.2 2023/03/19 01:17:21 leavens Exp $ */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include "token.h"
#include "file_location.h"
#include "utilities.h"

// to turn off assertions
// define the symbol NDEBUG (by writing uncommenting the following)
// #define NDEBUG

// otherwise debugging is on, and debug_print is defined as follows...
// (note that assert is a macro defined in <assert.h>
static void vdebug_print(const char *fmt, va_list args);

// If debugging is false, do nothing, otherwise (when debugging)
// flush stderr and stdout, then print the message given on stderr,
// using printf formatting from the format string fmt.
// This function returns normally.
void debug_print(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    vdebug_print(fmt, args);
}

// The variadic version of debug_print
static void vdebug_print(const char *fmt, va_list args)
{
    // flush output streams to synchronize outputs
    fflush(stdout);
    fflush(stderr);
    vfprintf(stderr, fmt, args);
    fflush(stderr);
}

static void vbail_with_error(const char* fmt, va_list args);

// Format a string error message and print it followed by a newline on stderr
// using perror (for an OS error, if the errno is not 0)
// then exit with a failure code, so a call to this does not return.
void bail_with_error(const char *fmt, ...)
{
    fflush(stdout); // flush so output comes after what has happened already
    va_list(args);
    va_start(args, fmt);
    vbail_with_error(fmt, args);
}

// The variadic version of bail_with_error
static void vbail_with_error(const char* fmt, va_list args)
{
    extern int errno;
    char buff[2048];
    vsprintf(buff, fmt, args);
    if (errno != 0) {
	perror(buff);
    } else {
	fprintf(stderr, "%s\n", buff);
    }
    fflush(stderr);
    exit(EXIT_FAILURE);
}

void lexical_error(const char *filename, unsigned int line,
			  unsigned int column, const char *fmt, ...)
{
    fflush(stdout); // flush so output comes after what has happened already
    fprintf(stderr, "%s: line %d, column %d: ", filename, line, column);
    va_list(args);
    va_start(args, fmt);
    vbail_with_error(fmt, args);
}

const char *token2string(token t)
{
    char *buf = (char *)malloc(sizeof(char)*BUFSIZ);
    if (buf == NULL) {
	bail_with_error("No space for buf in token2string!");
    }
    sprintf(buf, "%s", ttyp2str(t.typ));
    int len = strlen(buf);
    if (t.text != NULL) {
	sprintf(buf+len, " (\"%s\")", t.text);
    }
    return buf;
}

// Requires num_expected > 0 and expected has num_expected elements.
// Print a parsing error message on stderr about an unexpected token
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message (adding a newline).
// Then exit with a failure code, so this function does not return.
// The message says that one of the token types in expected
// was expected, but instead the next token (saw) was seen.
void parse_error_unexpected(token_type *expected,
			    unsigned int num_expected,
			    token saw)
{
    fflush(stdout); // flush so output comes after what has happened already
    // print file, line, column information
    fprintf(stderr, "%s: line %d, column %d: syntax error, ",
	    saw.filename, saw.line, saw.column);

    // print what was expected and what was seen, then bail out!
    if (num_expected == 1) {
	bail_with_error("expecting a %s token, but saw a %s token (\"%s\")",
			ttyp2str(expected[0]), ttyp2str(saw.typ),
			(saw.text != NULL ? saw.text : ""));
    } else {
	// num_expected > 1
	fprintf(stderr, "Expecting one of: ");
	for (int i = 0; i < num_expected; i++) {
	    if (0 < i && i < num_expected-1) {
		fprintf(stderr, ", ");
	    } else if (i == num_expected-1) {
		fprintf(stderr, " or ");
	    }
	    fprintf(stderr, "%s", ttyp2str(expected[i]));
	}
	bail_with_error(", but saw a %s token (\"%s\")",
			ttyp2str(saw.typ),
			(saw.text != NULL ? saw.text : ""));
    }
}

// Print a parsing error message on stderr from the parser
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Then exit with a failure code, so this function does not return.
void parse_error_general(token t, const char *fmt, ...)
{
    fflush(stdout); // flush so output comes after what has happened already
    // print file, line, column information
    fprintf(stderr, "%s: line %d, column %d: ",
	    t.filename, t.line, t.column);

    va_list(args);
    va_start(args, fmt);
    vbail_with_error(fmt, args);
}

// Print a compiler error message on stderr
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Then exit with a failure code, so this function does not return.
void general_error(file_location floc, const char *fmt, ...)
{
    fflush(stdout); // flush so output comes after what has happened already
    // print file, line, column information
    fprintf(stderr, "%s: line %d, column %d: ",
	    floc.filename, floc.line, floc.column);

    va_list(args);
    va_start(args, fmt);
    vbail_with_error(fmt, args);
}
