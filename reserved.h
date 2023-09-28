#ifndef _RESERVED_H
#define _RESERVED_H
#include "token.h"

#define NUM_RESERVED_WORDS 15

// initialize the data structures of the
// reserved module
extern void reserved_initialize();

// Requires: text != NULL
// If text is a reserved word,
// then return its token_type,
// else return the token_type nosym
extern token_type reserved_type(const char *text);

#endif
