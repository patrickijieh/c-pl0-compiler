/* $Id: id_attrs.h,v 1.6 2023/03/22 22:08:22 leavens Exp $ */
#ifndef _ID_ATTRS_H
#define _ID_ATTRS_H
#include "token.h"
#include "file_location.h"
#include "machine_types.h"
#include "label.h"

// kinds of entries in the symbol table
typedef enum {constant, variable, procedure} id_kind;

// forward declaration, so can use the type AST* below
typedef struct AST_s AST;

// attributes of identifiers in the symbol table
typedef struct {
    // file_loc is the source file location of the identifier's declaration
    file_location file_loc;
    id_kind kind;  // kind of identifier
    // for constants and variables, the offset from beginning of an AR
    unsigned int loc_offset; 
    // for a procedure, its label (to use in a call)
    label *lab;
} id_attrs;

// Return a freshly allocated id_attrs struct
// with the given file location and the rest of the fields uninitialized.
// If there is no space, bail with an error message,
// so this should never return NULL.
extern id_attrs *id_attrs_start(file_location floc);

// Return a freshly allocated id_attrs struct
// with token t, kind k, and loc_offset ofst (for constants and variables),
// and code a block AST pointer of NULL (for procedures).
// If there is no space, bail with an error message,
// so this should never return NULL.
extern id_attrs *id_attrs_loc_create(file_location floc, id_kind k,
				     unsigned int ofst);

// Return a freshly allocated id_attrs struct for a procedure
// with token t, kind k, and label lab
// If there is no space, bail with an error message,
// so this should never return NULL.
extern id_attrs *id_attrs_proc_create(file_location floc, label *lab);

// Return a lowercase version of the kind's name as a string
// (i.e. if k == variable, return "variable", else return "constant")
extern const char *kind2str(id_kind k);
#endif
