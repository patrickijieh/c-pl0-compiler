/* $Id: lexer.c,v 1.3 2023/04/06 18:05:23 leavens Exp leavens $ */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include "utilities.h"
#include "lexer.h"
#include "reserved.h"

// The input file
static FILE *input_file = NULL;
// The input file's name
static const char *filename = NULL;
// Is this token stream done (past EOF or error)?
static bool done = true;
// the line of the next token
static unsigned int line;
// the column of the next token
static unsigned int column;

// Check the lexer's invariant
static void lexer_okay()
{
    assert(done == (input_file == NULL));
    assert(done == (filename == NULL));
}

// Initialize the lexer (i.e., its data structures)
static void lexer_initialize()
{
    filename = NULL;
    input_file = NULL;
    done = true;
    line = 1;
    column = 1;
    reserved_initialize();
}

// Requires: fname != NULL
// Requires: fname is the name of a readable file
// Initialize the lexer and start it reading
// from the given file name
void lexer_open(const char *fname)
{
    lexer_initialize();
    input_file = fopen(fname, "r");
    if (input_file == NULL) {
	bail_with_error("Cannot open %s", fname);
    }
    filename = fname;
    done = feof(input_file);
    if (done) {
	input_file = NULL;
	filename = NULL;
    }
    lexer_okay();
}

// Close the file the lexer is working on
// and make this lexer be done
void lexer_close()
{
    lexer_okay();
    if (input_file != NULL) {
	int rc = fclose(input_file);
	if (rc == EOF) {
	    bail_with_error("Cannot close %s!", filename);
	}
    }
    input_file = NULL;
    filename = NULL;
    done = true;
    lexer_okay();
}

// Is the lexer's token stream finished
// (either past EOF or not open)?
bool lexer_done()
{
    return done;
}

// the previous value of column,
// for use in lexer_ungetchar
static unsigned int last_column = 0;

// Requires: input_file is readable
// Return the next char in the input
// updating line and column as appropriate
// update last_column to the old value of column
static char lexer_getchar()
{
    char c = getc(input_file);
    last_column = column;
    if (c == '\n') {
	line++;
	column=1;
    } else {
	column++;
    }
    return c;
}

// Requires: input_file is readable
// Put c back into the input file stream
// to be read again
static void lexer_ungetchar(char c)
{
    column = last_column;
    if (c == '\n') {
	line--;
    }
    if (c != EOF) {
	ungetc(c, input_file);
    }
}

// forward declarations of lexical functions
static void lexer_consume_ignored();
static token lexer_ident(char c, token t);
static token lexer_number(char c, token t);
static token lexer_becomes(char c, token t);
static token lexer_starts_less(char c, token t);
static token lexer_starts_greater(char c, token t);

// Requires: !lexer_done()
// Return the next token in the input file,
// advancing in the input
token lexer_next()
{
    token t;
    t.filename = filename;
    t.typ = eofsym;
    t.text = NULL;
    t.value = 0;

    lexer_consume_ignored();

    t.line = line;
    t.column = column;

    char c = lexer_getchar();
    
    // since we consumed all the whitespace
    // c should not be a kind of space character
    assert(!isspace(c));
    
    if (c == EOF) {
	t.typ = eofsym;
	t.text = NULL;
	filename = NULL;
	input_file = NULL;
	done = true;
	return t;
    }
    if (isalpha(c)) {
	return lexer_ident(c, t);
    } else if (isdigit(c)) {
	return lexer_number(c, t);
    } else {
	// leave room in t.text for 2 chars of text
	t.text = malloc(3 * sizeof(char));
	if (t.text == NULL) {
	    bail_with_error("Cannot allocate space for token text!");
	}
	t.text[0] = c;
	t.text[1] = '\0';
	switch (c) {
	case '.':
	    t.typ = periodsym;
	    break;
	case ';':
	    t.typ = semisym;
	    break;
	case ',':
	    t.typ = commasym;
	    break;
	case ':':
	    return lexer_becomes(c, t);
	case '=':
	    t.typ = eqsym;
	    break;
	case '(':
	    t.typ = lparensym;
	    break;
	case ')':
	    t.typ = rparensym;
	    break;
	case '<':
	    return lexer_starts_less(c, t);
	    break;
	case '>':
	    return lexer_starts_greater(c, t);
	    break;
	case '+':
	    t.typ = plussym;
	    break;
	case '-':
	    t.typ = minussym;
	    break;
	case '*':
	    t.typ = multsym;
	    break;
	case '/':
	    t.typ = divsym;
	    break;
	default:
	    lexical_error(filename, line, column,
			  "Illegal character '%c' (0%o)",
			   c, c);
	    break;
	}
	return t;
    }
}

// Requires: !lexer_done()
// Return the name of the current file
const char *lexer_filename()
{
    if (lexer_done()) {
	bail_with_error("Asking for file name of done lexer!");
    }
    return filename;
}

// Requires: !lexer_done()
// Return the line number of the next token
unsigned int lexer_line()
{
    if (lexer_done()) {
	bail_with_error("Asking for line of done lexer!");
    }
    return line;
}

// Requires: !lexer_done()
// Return the column number of the next token
unsigned int lexer_column()
{
    if (lexer_done()) {
	bail_with_error("Asking for column of done lexer!");
    }
    return column;
}

// Requires: input_file is readable
// Advance input_file to the next newline
static void lexer_consume_comment()
{
    char c = lexer_getchar();
    while (c != '\n' && c != EOF) {
	c = lexer_getchar();
    }
    if (c == EOF) {
	lexical_error(filename, line, column,
		      "File ended while reading comment!");
    }
    // assert(c == '\n');
}

// Requires: input_file is readable
// Advance in the input file until
// the next char is the start of a token
// that is not ignored
// (i.e., not whitespace or a comment)
static void lexer_consume_ignored()
{
    char c = lexer_getchar();
    while (isspace(c) || c == '#') {
	if (isspace(c)) {
	    // ignore the whitespace char
	    c = lexer_getchar();
	} else if (c == '#') {
	    lexer_consume_comment();
	    c = lexer_getchar();
	}
    }
    // assert(!isspace(c) && c != '#');
    lexer_ungetchar(c);
}

// Requires: c is a letter
// Return a token for a reserved word
// or an identifier
static token lexer_ident(char c, token t)
{
    char *text = malloc((MAX_IDENT_LENGTH+1)*sizeof(char));
    if (text == NULL) {
	bail_with_error("Cannot allocate space for identifier");
    }
    text[0] = c;
    int n = 1;
    c = lexer_getchar();
    while (isalpha(c) || isdigit(c)) {
	if (n >= MAX_IDENT_LENGTH) {
	    text[n] = '\0';
	    lexical_error(filename, line, column,
			  "Identifier starting \"%s\" is too long!",
			  text);
	}
	text[n] = c;
	n++;
	c = lexer_getchar();
    }
    // assert(!isalpha(c) && !isdigit(c));
    text[n+1] = '\0';    
    lexer_ungetchar(c);
    t.text = text;
    t.typ = reserved_type(text);
    return t;
}

#define MAX_NUM_LENGTH 5

// Requires: c is a digit
// Return a token for a number
static token lexer_number(char c, token t)
{
    char *text = malloc((MAX_NUM_LENGTH+1)*sizeof(char));
    if (text == NULL) {
	bail_with_error("Cannot allocate space for number");
    }
    text[0] = c;
    int n = 1;
    c = lexer_getchar();
    while (isdigit(c)) {
	if (n >= MAX_NUM_LENGTH) {
	    text[n] = '\0';
	    bail_with_error(filename, line, column,
			    "Number starting \"%s\" is too long!",
			    text);
	}
	text[n] = c;
	n++;
	c = lexer_getchar();
    }
    text[n+1] = '\0';
    lexer_ungetchar(c);
    t.text = text;
    int val;
    sscanf(text, "%d", &val);
    if (val > SHRT_MAX) {
	lexical_error(filename, line, column,
		      "The value of %s is too large for a short!",
		      text);
    }
    t.value = val;
    t.typ = numbersym;
    return t;
}

// Requires: c is a colon character (:)
// Returns the token for a becomessym
static token lexer_becomes(char c, token t)
{
    assert(c == ':');
    c = lexer_getchar();
    if (c != '=') {
	lexical_error(filename, line, column,
		      "Expecting '=' after a colon, not '%c'",
		      c);
    }
    t.text[1] = c;
    t.text[2] = '\0';
    t.typ = becomessym;
    return t;
}

// Requires: c is a less-than character (<)
// Returns the token appropriate for the next char
static token lexer_starts_less(char c, token t)
{
    assert(c == '<');
    c = lexer_getchar();
    t.text[1] = c;
    t.text[2] = '\0';
    switch (c) {
    case '=':
	t.typ = leqsym;
	break;
    case '>':
	t.typ = neqsym;
	break;
    default:
	t.text[1] = '\0';
	lexer_ungetchar(c);
	t.typ = lessym;
	break;
    }
    return t;
}

static token lexer_starts_greater(char c, token t)
{
    assert(c == '>');
    c = lexer_getchar();
    t.text[1] = c;
    t.text[2] = '\0';
    switch (c) {
    case '=':
	t.typ = geqsym;
	break;
    default:
	t.text[1] = '\0';
	lexer_ungetchar(c);
	t.typ = gtrsym;
	break;
    }
    return t;

}
