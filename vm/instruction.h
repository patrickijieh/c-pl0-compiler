/* $Id: instruction.h,v 1.7 2023/03/20 17:13:53 leavens Exp $ */
#ifndef _INSTRUCTION_H
#define _INSTRUCTION_H
#include <stdio.h>
#include <stdbool.h>

typedef struct {
    int op; /* opcode */
    int m; /* M */
} instruction;

extern bool stop_reading;
extern instruction read_instruction(FILE *);

// Is the argument a legal op code for the machine?
extern bool legal_op_code(int op);

// Requires: legal_op_code(op);
// Return the ASCII mnemonic for the op code given.
extern const char * mnemonic(int op);

// Requires: out is open and writable
// print the header of the output table to out
extern void print_instruction_heading(FILE *out);

// Requires: out is an open FILE
// print the instruction with mnemonic (on one line)
// followed by a newline character.
void print_instr_debug(FILE *out, instruction instr);

// Requires: out is an open FILE
// print the address on out followed by the instruction (on one line)
// followed by a newline character.
void print_instr_with_addr(FILE *out, int addr, instruction instr);

// Requires: out is an open FILE
// print the instruction in the VM's input format,
// followed by a newline character.
void print_vm_instruction(FILE *out, instruction instr);

#endif
