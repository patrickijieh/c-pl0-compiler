/* $Id: label.h,v 1.1 2023/03/22 22:07:53 leavens Exp $ */
#ifndef _LABEL_H
#define _LABEL_H
#include <stdbool.h>
#include "machine_types.h"

typedef struct {
    bool is_set;
    address addr;
} label;

// Return a fresh label that is not set
extern label *label_create();

// Set the address in the label
extern void label_set(label *lab, address addr);

// Is lab set?
extern bool label_is_set(label *lab);

// Requires: label_is_set(lab)
// Return the address in lab.
extern address label_read(label *lab);
#endif
