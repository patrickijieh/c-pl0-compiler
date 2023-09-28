/* $Id: lexical_address.h,v 1.2 2023/03/14 21:09:51 leavens Exp $ */
#ifndef _LEXICAL_ADDRESS_H
#define _LEXICAL_ADDRESS_H

#include <stdio.h>
#include <stdlib.h>

typedef struct {
    unsigned int levelsOutward;
    unsigned int offsetInAR;
} lexical_address;

// Allocate and return a (fresh) lexical address
// where the levelsOutwards field is levelsOut
// and the offsetInAR field is offset
extern lexical_address *lexical_address_create(unsigned int levelsOut,
					       unsigned int offset);

// Requires: out is not NULL and is open for writing
// Requires: la is not NULL
extern void lexical_address_print(FILE *out, lexical_address *la);

#endif
