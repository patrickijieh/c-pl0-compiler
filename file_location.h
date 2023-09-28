/* $Id: file_location.h,v 1.1 2023/03/08 15:18:43 leavens Exp $ */
#ifndef _FILE_LOCATION_H
#define _FILE_LOCATION_H
#include "token.h"

// location in a source file (useful for error messages)
typedef struct {
    const char *filename;
    unsigned int line; // of first token
    unsigned int column; // of first token
} file_location;

// Return the file location information from a token
extern file_location token2file_loc(token t);

#endif
