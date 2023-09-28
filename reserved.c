#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "reserved.h"

static const char *reserved_words[NUM_RESERVED_WORDS]
        = {"const", "var", "procedure",
           "call", "begin", "end",
	   "if", "then", "else", "while", "do",
           "read", "write", "skip", "odd"};

static token_type reserved_types[NUM_RESERVED_WORDS]
         = {constsym, varsym, procsym,
           callsym, beginsym, endsym,
	   ifsym, thensym, elsesym, whilesym, dosym,
	   readsym, writesym, skipsym, oddsym};

// initialize the data structures of the
// reserved module
void reserved_initialize()
{
    // nothing to do!
}

// Requires: text != NULL
// If text is a reserved word,
// then return its token_type,
// else return the token_type identsym
token_type reserved_type(const char *text)
{
    for (int i = 0; i < NUM_RESERVED_WORDS; i++) {
	if (strcmp(text, reserved_words[i]) == 0) {
	    return reserved_types[i];
	}
    }
    return identsym;
}
