/* $Id: utilities.h,v 1.1 2023/03/08 15:18:43 leavens Exp $ */
#ifndef _UTILITIES_H
#define _UTILITIES_H
#include <stdbool.h>
#include <assert.h>
#include "token.h"
#include "file_location.h"

// If NDEBUG is defined, do nothing, otherwise (when debugging)
// flush stderr and stdout, then print the message given on stderr,
// using printf formatting from the format string fmt.
// This function returns normally.
void debug_print(const char *fmt, ...);

// Format a string error message and print it using perror (for an OS error)
// then exit with a failure code, so a call to this does not return.
extern void bail_with_error(const char *fmt, ...);

// Print a lexical error message to stderr
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Output goes to stderr and then an exit with a failure code,
// so a call to this function does not return.
extern void lexical_error(const char *filename, unsigned int line,
			  unsigned int column, const char *fmt, ...);

// Requires num_expected > 0 and expected has num_expected elements.
// Print a parsing error message on stderr
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Then exit with a failure code, so this function does not return.
// The message says that one of the token types in expected
// was expected, but instead the next token (saw) was seen.
extern void parse_error_unexpected(token_type *expected,
				   unsigned int num_expected,
				   token saw);

// Print a parsing error message on stderr from the parser
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Then exit with a failure code, so this function does not return.
extern void parse_error_general(token t, const char *fmt, ...);

// Print a compiler error message on stderr
// starting with the filename, a colon, the line number, a comma
// the column number, a colon, and then the message.
// Then exit with a failure code, so this function does not return.
extern void general_error(file_location floc, const char *fmt, ...);

#endif
