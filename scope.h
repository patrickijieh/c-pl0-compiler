/* $Id: scope.h,v 1.2 2023/03/10 21:28:11 leavens Exp $ */
#ifndef _SCOPE_H
#define _SCOPE_H

#include <stdbool.h>
#include "token.h"
#include "ast.h"
#include "id_attrs.h"

// Maximum number of declarations that can be stored in a scope
#define MAX_SCOPE_SIZE 4096

typedef struct {
    const char *id;
    id_attrs *attrs;
} scope_assoc_t;

// Invariant: 0 <= size < MAX_SCOPE_SIZE;
typedef struct scope_s {
    unsigned int size;
    unsigned int next_loc_offset;
    scope_assoc_t *entries[MAX_SCOPE_SIZE];
} scope_t;

// Allocate a fresh scope symbol table and return (a pointer to) it.
// Issues an error message (on stderr) if there is no space
// and exits with a failure error code in that case.
extern scope_t *scope_create();

// Return the current scope's next offset to use for allocation,
// which is the number of constants and variables allocated in this scope.
extern unsigned int scope_next_loc_offset(scope_t *s);

// Return the current scope's next offset to use for allocation,
// which is the size of the current scope (number of declared ids).
extern unsigned int scope_size(scope_t *s);

// Is the current scope full?
extern bool scope_full(scope_t *s);

// Is the given name associated with some attributes in the current scope?
extern bool scope_defined(scope_t *s, const char *name);

// Requires: !scope_defined(name) && attrs != NULL;
// Modify the current scope symbol table to
// add an association from the given name to the given id_attrs attrs,
// and if attrs->id_kind != procedure, 
// then this stores the next_loc_offset value into attrs->loc_offset
// and then increases the next_loc_offset for this scope by 1.
extern void scope_insert(scope_t *s, const char *name, id_attrs *attrs);

// Return (a pointer to) the attributes of the given name in the current scope
// or NULL if there is no association for name.
extern id_attrs *scope_lookup(scope_t *s, const char *name);

#endif
