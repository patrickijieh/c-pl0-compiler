/* $Id: label.c,v 1.4 2023/03/23 05:36:03 leavens Exp $ */
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include "utilities.h"
#include "label.h"

// Return a fresh label that is not set
extern label *label_create()
{
    label *ret = (label *)malloc(sizeof(label));
    if (ret == NULL) {
	bail_with_error("No space to allocate label!");
    }
    ret->is_set = false;
    return ret;
}

// Requires: !label_is_set(lab)
// Set the address in the label
extern void label_set(label *lab, address addr)
{
    lab->addr = addr;
    lab->is_set = true;
}

// Is lab set?
extern bool label_is_set(label *lab)
{
    return lab->is_set;
}

// Requires: label_is_set(lab)
// Return the address in lab.
extern address label_read(label *lab)
{
    if (!label_is_set(lab)) {
	bail_with_error("Internal error: label_read on unset label");
    }
    return lab->addr;
}
