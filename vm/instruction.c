/* $Id: instruction.c,v 1.17 2023/03/27 14:10:39 leavens Exp leavens $ */
#include <stdio.h>
#include <stdbool.h>
#include "instruction.h"
#include "utilities.h"

// one more than the highest op code, to allow for 0
#define NUM_OPCODES 31

static const char *opcodes[NUM_OPCODES] =
    {"NOP", "LIT", "RTN", "CAL", "POP",
     "PSI", "LOD", "STO", "INC", "JMP",
     "JPC", "CHO", "CHI", "HLT", "NDB",
     "NEG", "ADD", "SUB", "MUL", "DIV",
     "MOD", "EQL", "NEQ", "LSS", "LEQ",
     "GTR", "GEQ", "PSP", "PBP", "PPC",
     "JMI"};

// Is the argument a legal op code for the machine?
bool legal_op_code(int op)
{
    return 0 <= op && op < NUM_OPCODES;
}

// return the mnemonic for the given op code
const char * mnemonic(int op)
{
    if (!legal_op_code(op)) {
	bail_with_error("Illegal opcode: %d", op);
    }
    return opcodes[op];
}

bool stop_reading = false;

// read a single instruction (all on one line) from the file in and return it
// sets stop_reading to true if there is an error or EOF detected
instruction read_instruction(FILE *in)
{
    instruction instr;
    int num_read =
	fscanf(in, "%d %d\n", &instr.op, &instr.m);
    if (!legal_op_code(instr.op) || num_read < 2) {
	stop_reading = true;
    }
    return instr;
}

// Requires: out is open and writable
// print the header of the output table to out
void print_instruction_heading(FILE *out) {
    fprintf(out, "%-5s %-5s %-5s\n", "Addr", "OP", "M");
}

// Requires: out is open for writing
// print to out the given instruction, which is found at the given address
void print_instr_with_addr(FILE *out, int addr, instruction instr)
{
    fprintf(out, "%-5d %-5s %-5d\n", addr, mnemonic(instr.op), instr.m);
}

// Requires: out is an open FILE
// print the instruction in the VM's input format,
// followed by a newline character.
void print_vm_instruction(FILE *out, instruction instr)
{
    if (!legal_op_code(instr.op)) {
	    bail_with_error("Illegal opcode passed to print_vm_instruction %d",
			    instr.op);
    }
    fprintf(out, "%-5d %-5d\n", instr.op, instr.m);
}
