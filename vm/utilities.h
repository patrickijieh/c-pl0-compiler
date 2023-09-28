/* $Id: utilities.h,v 1.5 2023/01/25 14:11:04 leavens Exp $ */
#ifndef _UTILITIES_H
#define _UTILITIES_H

// Format a string error message and print it using perror (for an OS error)
// then exit with a failure code, so a call to this does not return.
extern void bail_with_error(const char *fmt, ...);
#endif
