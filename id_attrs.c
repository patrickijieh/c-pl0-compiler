/* $Id: id_attrs.c,v 1.6 2023/03/22 22:08:22 leavens Exp $ */
// Attributes of identifiers in the symbol table
#include <stdlib.h>
#include <stddef.h>
#include "utilities.h"
#include "id_attrs.h"

// Return a freshly allocated id_attrs struct
// with the given file location and the rest of the fields uninitialized.
// If there is no space, bail with an error message,
// so this should never return NULL.
id_attrs *id_attrs_start(file_location floc)
{
    id_attrs *ret = (id_attrs *)malloc(sizeof(id_attrs));
    if (ret == NULL) {
	bail_with_error("No space to allocate id_attrs!");
    }
    ret->file_loc = floc;
    return ret;
}

// Return a freshly allocated id_attrs struct
// with token t, kind k, and loc_offset ofst (for constants and variables),
// If there is no space, bail with an error message,
// so this should never return NULL.
id_attrs *id_attrs_loc_create(file_location floc, id_kind k,
				 unsigned int ofst)
{
    id_attrs *ret = id_attrs_start(floc);
    ret->kind = k;
    ret->loc_offset = ofst;
    return ret;
}

// Return a freshly allocated id_attrs struct for a procedure
// with token t, kind k, and label lab
// If there is no space, bail with an error message,
// so this should never return NULL.
id_attrs *id_attrs_proc_create(file_location floc, label *lab)
{
    id_attrs *ret = id_attrs_start(floc);
    ret->kind = procedure;
    ret->lab = lab;
    return ret;
}

// Return a English version of the kind's name as a string
// (i.e. if k == variable, return "variable", else return "constant")
const char *kind2str(id_kind k)
{
    static const char *kind_names[3] = {"constant", "variable", "procedure"};
    return kind_names[k];
}
