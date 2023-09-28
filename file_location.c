/* $Id: file_location.c,v 1.1 2023/03/08 15:18:43 leavens Exp $ */
#include "file_location.h"

// Return the file location information from a token
file_location token2file_loc(token t)
{
    file_location ret;
    ret.filename = t.filename;
    ret.line = t.line;
    ret.column = t.column;
    return ret;
}
