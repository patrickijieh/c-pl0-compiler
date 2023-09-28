/* $Id: scope.c,v 1.4 2023/04/06 18:05:23 leavens Exp leavens $ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "scope.h"
#include "utilities.h"

// Allocate a fresh scope symbol table and return (a pointer to) it.
// Issues an error message (on stderr) if there is no space
// and exits with a failure error code in that case.
scope_t *scope_create()
{
    scope_t *new_s
	= (scope_t *) malloc(sizeof(scope_t));
    if (new_s == NULL) {
	bail_with_error("No space for new scope_t!");
    }
    new_s->size = 0;
    new_s->next_loc_offset = 0;
    for (int j = 0; j < MAX_SCOPE_SIZE; j++) {
	new_s->entries[j] = NULL;
    }
    return new_s;
}

// Return the current scope's next offset to use for allocation,
// which is the number of constants and variables allocated in this scope.
extern unsigned int scope_next_loc_offset(scope_t *s)
{
    return s->next_loc_offset;
}

// Return the current scope's size (number of declared ids).
unsigned int scope_size(scope_t *s)
{
    return s->size;
}

// Is the current scope full?
bool scope_full(scope_t *s)
{
    return scope_size(s) >= MAX_SCOPE_SIZE;
}

// Requires: assoc != NULL && !scope_full() && !scope_defined(assoc->id);
// Add an association from the given name to the given id attributes
// in the current scope.
// If assoc->attrs->kind != procedure, 
// then this stores the next_loc_offset value into assoc->attrs->loc_offset
// and then increases next_loc_offset by 1.
static void scope_add(scope_t *s, scope_assoc_t *assoc)
{
    // assert(assoc != NULL);
    // assert(!scope_full());
    // assert(!scope_defined(assoc->id));
    if (assoc->attrs->kind != procedure) {
	(assoc->attrs->loc_offset) = (s->next_loc_offset)++;
    }
    s->entries[(s->size)++] = assoc;
    // fprintf(stderr, "assoc->attrs->loc_offset is %d\n",
    //         assoc->attrs->loc_offset);
}

// Requires: !scope_defined(name) && attrs != NULL;
// Modify the current scope symbol table to
// add an association from the given name to the given id_attrs attrs,
// and if attrs->kind != procedure, 
// then this stores the next_loc_offset value into attrs->loc_offset
// and then increases the next_loc_offset for this scope by 1.
void scope_insert(scope_t *s, const char *name, id_attrs *attrs)
{
    // assert(!scope_defined(name));
    // assert(attrs != NULL);
    // debug_print("Running scope_insert for name "%s\"\n", name);
    scope_assoc_t *new_assoc = malloc(sizeof(scope_assoc_t));
    if (new_assoc == NULL) {
	bail_with_error("No space for association!");
    }
    new_assoc->id = name;
    new_assoc->attrs = attrs;
    scope_add(s, new_assoc);
}

// Requires: name != NULL;
// Is the given name associated with some attributes in the current scope?
bool scope_defined(scope_t *s, const char *name)
{
    // assert(s != NULL);
    // assert(name != NULL);
    // debug_print("entering scope_defined for \"%s\"\n", name);
    return scope_lookup(s, name) != NULL;
}

// Requires: name != NULL and scope_initialize() has been called previously.
// Return (a pointer to) the attributes of the given name in the current scope
// or NULL if there is no association for name.
id_attrs *scope_lookup(scope_t *s, const char *name)
{
    int i;
    // assert(name != NULL);
    // assert(s != NULL);
    // debug_print("Entering scope_lookup for \"%s\"\n", name);
    for (i = 0; i < s->size; i++) {
	// assert(s != NULL);
	// assert(s->entries != NULL);
	// assert(0 <= i && i < s->size);
	// assert(s->entries[i] != NULL);
	// assert(s->entries[i]->id != NULL);
	// debug_print("Past asserts in scope_lookup loop\n");
	// debug_print("Length of name is %d\n", strlen(name));
	if (strcmp(s->entries[i]->id, name) == 0) {
	    // debug_print("scope_lookup(\"%s\") returning attributes\n",
	    //		name);
	    return s->entries[i]->attrs;
	}
    }
    // assert(i == s->size);
    // debug_print("The scope_lookup call on \"%s\" returns NULL\n", name);
    return NULL;
}
