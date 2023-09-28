/* $Id: code.c,v 1.13 2023/04/09 19:14:10 leavens Exp leavens $ */
#include <stdlib.h>
#include "utilities.h"
#include "lexical_address.h"
#include "code.h"

// Code creation functions below

typedef enum {
     NOP, LIT, RTN, CAL, POP, PSI, LOD, STO, INC, JMP,
     JPC, CHO, CHI, HLT, NDB, NEG, ADD, SUB, MUL, DIV,
     MOD, EQL, NEQ, LSS, LEQ, GTR, GEQ, PSP, PBP, PPC,
     JMI
} opcode;

// Return a fresh code struct, with next pointer NULL
// containing the instruction with opcode op and the given m parameter.
// If there is not enough space, bail with an error,
// so this will never return NULL.
static code *code_create(opcode op, int m)
{
    code *ret = (code *)malloc(sizeof(code));
    if (ret == NULL) {
	bail_with_error("Not enough space to allocate a code struct!");
    }
    ret->next = NULL;
    ret->instr.op = op;
    ret->instr.m = m;
    return ret;
}

// no-op
code *code_nop()
{
    return code_create(NOP, 0);
}

// literal push
code *code_lit(word n)
{
    return code_create(LIT, n);
}

// return from a subroutine
code *code_rtn()
{
    return code_create(RTN, 0);
}

// call the procedure at code index p
code *code_cal(label *lab)
{
    code *ret = code_create(CAL, -1);
    ret->lab = lab;
    if (label_is_set(lab)) {
	address p = label_read(lab);
	ret->instr.m = p;
    }
    // if the label is not set yet, then
    // just leave it and fill in the address later
    return ret;
}

// pop the stack
code *code_pop()
{
    return code_create(POP, 0);
}

// push the element whose address is on the top of the stack
code *code_psi()
{
    return code_create(PSI, 0);
}

// load the value at the address given at the top of the stack
// + offset o + LINKS_SIZE
code *code_lod(int o)
{
    return code_create(LOD, LINKS_SIZE+o);
}

// store the top of the stack's value into the address at the
// stack[SP-2]+o+LINK_SIZE, and then pop twice
code *code_sto(int o)
{
    code *ret = code_create(STO, LINKS_SIZE+o);
    return ret;
}

// allocate m locals on the stack
code *code_inc(unsigned int m)
{
    return code_create(INC, m);
}

// jump relative to the current instruction's address plus offset
code *code_jmp(int offset)
{
    return code_create(JMP, offset);
}

// jump conditionally, relative to current instruction's address by
// the given offset if top of the stack is not 0
code *code_jpc(int offset)
{
    return code_create(JPC, offset);
}

// output char on top of the stack
code *code_cho()
{
    return code_create(CHO, 0);
}

// read char from stdin and push it onto the stack
code *code_chi()
{
    return code_create(CHI, 0);
}

// halt execution
code *code_hlt()
{
    return code_create(HLT, 0);
}

// stop printing debugging output
code *code_ndb()
{
    return code_create(NDB, 0);
}

// negate value at top of stack
code *code_neg()
{
    return code_create(NEG, 0);
}

// add top two elements in the stack
code *code_add()
{
    return code_create(ADD, 0);
}

// subtract, top gets stack[SP-2] - stack[SP-1]
code *code_sub()
{
    return code_create(SUB, 0);
}

// multiply the top two elements in the stack
code *code_mul()
{
    return code_create(MUL, 0);
}

// divide, top gets stack[SP-2] / stack[SP-1]
code *code_div()
{
    return code_create(DIV, 0);
}

// modulo, top gets stack[SP-2] mod stack[SP-1]
code *code_mod()
{
    return code_create(MOD, 0);
}

// equal test
code *code_eql()
{
    return code_create(EQL, 0);
}

// not equal test
code *code_neq()
{
    return code_create(NEQ, 0);
}

// strictly less than test
code *code_lss()
{
    return code_create(LSS, 0);
}

// less than or equal to test
code *code_leq()
{
    return code_create(LEQ, 0);
}

// strictly greater than test
code *code_gtr()
{
    return code_create(GTR, 0);
}

// greater than or equal to test
code *code_geq()
{
    return code_create(GEQ, 0);
}

// push SP on top of the stack
code *code_psp()
{
    return code_create(PSP, 0);
}

// push BP on top of the stack
code *code_pbp()
{
    return code_create(PBP, 0);
}

// push PC on top of the stack
code *code_ppc()
{
    return code_create(PPC, 0);
}

// jump to the address on top of stack
code *code_jmi()
{
    return code_create(JMI, 0);
}


// Sequence manipulation functions below

// Return an empty code_seq
code_seq code_seq_empty()
{
    return NULL;
}

// Return a code_seq containing just the given code
code_seq code_seq_singleton(code *c)
{
    return c;
}


// Is seq empty?
bool code_seq_is_empty(code_seq seq)
{
    return seq == NULL;
}

// Requires: !code_seq_is_empty(seq)
// Return the first element of the given code sequence, seq
code *code_seq_first(code_seq seq)
{
    return seq;
}

// Requires: !code_seq_is_empty(seq)
// Return the rest of the given sequence, seq
code_seq code_seq_rest(code_seq seq)
{
    return seq->next;
}

// Return the size (number of instructions/words) in seq
unsigned int code_seq_size(code_seq seq)
{
    unsigned int ret = 0;
    while (!code_seq_is_empty(seq)) {
	ret++;
	seq = code_seq_rest(seq);
    }
    return ret;
}

// Requires: !code_seq_is_empty(seq)
// Return the last element in the given sequence
code *code_seq_last_elem(code_seq seq)
{
    code *ret = seq;
    while (!code_seq_is_empty(seq)) {
	ret = seq;
	seq = code_seq_rest(seq);
    }
    return ret;
}

// Requires: c != NULL
// Add the given code *c to the end of the seq
// and return the seq, which has been modified if it was not empty
code_seq code_seq_add_to_end(code_seq seq, code *c)
{
    if (code_seq_is_empty(seq)) {
	return code_seq_singleton(c);
    }
    // assert(!code_seq_is_empty(seq));
    code *last = code_seq_last_elem(seq);	
    last->next = c;
    c->next = NULL;
    return seq;
}

// Concatenate the given code sequences in order first s1 then s2
// This may modify the sequence s1 if both s1 and s2 are not empty
code_seq code_seq_concat(code_seq s1, code_seq s2)
{
    if (code_seq_is_empty(s1)) {
	return s2;
    } else if (code_seq_is_empty(s2)) {
	return s1;
    } else {
	code *last = code_seq_last_elem(s1);
	last->next = s2;
	return s1;
    }
}

// Requires: for all code containing a CAL instruction, either
// the address (m) field is set or the label is set.
// Modifies cs so that each CAL instruction whose target was not set
// has the target address set from the code's label.
void code_seq_fix_labels(code_seq cs)
{
    while (!code_seq_is_empty(cs)) {
	code *c = code_seq_first(cs);
	if (c->instr.op == CAL && c->instr.m == -1) {
	    if (label_is_set(c->lab)) {
		c->instr.m = label_read(c->lab);
	    } else {
		bail_with_error("Internal error: in code_seq_fix_labels label (%p) is not set!",
				c->lab);
	    }
	}
	cs = code_seq_rest(cs);
    }
}

// Requires: out is open for writing
// print the instructions in the code_seq to out in debugging format
void code_seq_debug_print(FILE *out, code_seq seq)
{
    print_instruction_heading(out);
    int addr = 0;
    while(!code_seq_is_empty(seq)) {
	print_instr_with_addr(out, addr, code_seq_first(seq)->instr);
	seq = code_seq_rest(seq);
	addr++;
    }
}

// Requires: out is open for writing
// print the instructions in seq, using the VM's input format, to out
void code_seq_vm_print(FILE *out, code_seq seq)
{
    while(!code_seq_is_empty(seq)) {
	print_vm_instruction(out, code_seq_first(seq)->instr);
	seq = code_seq_rest(seq);
    }
}

// Return a code sequence that will put the address that corresponds to the
// frame pointer for the given number of scopes outward on top of the stack
code_seq code_compute_fp(unsigned int levelsOut)
{
    code_seq ret = code_seq_singleton(code_pbp()); // bp of current stack frame
    while (levelsOut > 0) {
	// add in code to follow each needed static link
	ret = code_seq_add_to_end(ret, code_psi());
	levelsOut--;
    }
    // now ret holds the code to put the address of the frame for *la
    // on the top of the stack
    return ret;
}

// Requires: la != NULL
// Return a code sequence that will put the value
// of the given lexical address on the top of the stack
code_seq code_load_from_lexical_address(lexical_address *la)
{
    
    code_seq ret = code_compute_fp(la->levelsOutward);
    code_seq_add_to_end(ret,
			code_seq_singleton(code_lod(la->offsetInAR)));
    return ret;
}

