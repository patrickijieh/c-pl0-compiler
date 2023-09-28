/* $Id: code.h,v 1.11 2023/03/27 15:01:19 leavens Exp leavens $ */
#ifndef _CODE_SEQ_H
#define _CODE_SEQ_H
#include <stdbool.h>
#include "machine_types.h"
#include "label.h"
#include "instruction.h"
#include "lexical_address.h"

// number of words used by call instruction
#define LINKS_SIZE 3

typedef struct code_s code;
// code sequences
typedef code *code_seq;

// machine code instructions (that can be in linked lists)
typedef struct code_s {
    code_seq next;
    instruction instr;
    // labels are used for call instructions that have their target
    // filled in after the code is created.
    label *lab; 
} code;

// Code creation functions below

// no-op
extern code *code_nop();

// literal push
extern code *code_lit(word n);

// return from a subroutine
extern code *code_rtn();

// call the procedure at the code index given by lab
extern code *code_cal(label *lab);

// pop the stack
extern code *code_pop();

// push the element whose address is on the top of the stack
extern code *code_psi();

// push the parameter at address BP-o on top of the stack
extern code *code_lod(int o);

// store stack[SP-2] into the address at the top of the stack+o, pop twice
extern code *code_sto(int o);

// allocate m locals on the stack
extern code *code_inc(unsigned int m);

// jump relative to the current instruction's address plus offset
extern code *code_jmp(int offset);

// jump conditionally, relative to current instruction's address by
// the given offset if top of the stack is not 0
extern code *code_jpc(int offset);

// output char on top of the stack
extern code *code_cho();

// read char from stdin and push it onto the stack
extern code *code_chi();

// halt execution
extern code *code_hlt();

// stop printing debugging output
extern code *code_ndb();

// negate value at top of stack
extern code *code_neg();

// add top two elements in the stack
extern code *code_add();

// subtract, top gets stack[SP-2] - stack[SP-1]
extern code *code_sub();

// multiply the top two elements in the stack
extern code *code_mul();

// divide, top gets stack[SP-2] / stack[SP-1]
extern code *code_div();

// modulo, top gets stack[SP-2] mod stack[SP-1]
extern code *code_mod();

// equal test
extern code *code_eql();

// not equal test
extern code *code_neq();

// strictly less than test
extern code *code_lss();

// less than or equal to test
extern code *code_leq();

// strictly greater than test
extern code *code_gtr();

// greater than or equal to test
extern code *code_geq();

// push SP on top of the stack
extern code *code_psp();

// push BP on top of the stack
extern code *code_pbp();

// push PC on top of the stack
extern code *code_ppc();

// jump to the address on top of the stack
extern code *code_jmi();



// Sequence manipulation functions below

// Return an empty code_seq
extern code_seq code_seq_empty();

// Return a code_seq containing just the given code
extern code_seq code_seq_singleton(code *c);

// Is seq empty?
extern bool code_seq_is_empty(code_seq seq);

// Requires: !code_seq_is_empty(seq)
// Return the first element of the given code sequence, seq
extern code *code_seq_first(code_seq seq);

// Requires: !code_seq_is_empty(seq)
// Return the rest of the given sequence, seq
extern code_seq code_seq_rest(code_seq seq);

// Return the size (number of instructions/words) in seq
extern unsigned int code_seq_size(code_seq seq);

// Requires: !code_seq_is_empty(seq)
// Return the last element in the given sequence
extern code *code_seq_last_elem(code_seq seq);

// Requires: c != NULL
// Add the given code *c to the end of the seq
// and return the seq, which has been modified if it was not empty
extern code_seq code_seq_add_to_end(code_seq seq, code *c);

// Concatenate the given code sequences in order first s1 then s2
extern code_seq code_seq_concat(code_seq s1, code_seq s2);

// Requires: for all code containing a CAL instruction, either
// the address (m) field is set or the label is set.
// Modifies cs so that each CAL instruction whose target was not set
// has the target address set from the code's label.
extern void code_seq_fix_labels(code_seq cs);

// Requires: out is open for writing
// print the instructions in the code_seq to out in debugging format
extern void code_seq_debug_print(FILE *out, code_seq seq);

// Requires: out is open for writing
// print the instructions in seq, using the VM's input format, to out
extern void code_seq_vm_print(FILE *out, code_seq seq);

// Return a code sequence that will put the address that corresponds to the
// frame pointer for the given number of scopes outward on top of the stack
extern code_seq code_compute_fp(unsigned int levelsOut);

// Requires: la != NULL
// Return a code sequence that will put the value
// of the given lexical address on the top of the stack
extern code_seq code_load_from_lexical_address(lexical_address *la);
    
#endif
