/* $Id: machine.c,v 1.27 2023/03/27 14:10:39 leavens Exp leavens $ */
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdarg.h>
#include "instruction.h"
#include "utilities.h"
#include "stack.h"
#include "machine.h"

extern void initialize();
extern int read_program(FILE *prog);
extern void print_program(FILE *out, int size);

FILE *open_instruction_file(const char *filename)
{
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
	bail_with_error("Cannot open file '%s'", filename);
    }
    return f;
}

void close_instruction_file(FILE *prog)
{
    int fcrv = fclose(prog);
    if (fcrv == EOF) {
	bail_with_error("Cannot close stream for program file");
    }
}

// print tracing output?
bool tracing = true;

// stop the program's execution (false keeps it running)
static bool halt;

static instruction code[MAX_CODE_LENGTH];

// the program counter
static int PC;

void machine(const char *filename)
{
    initialize();
    FILE *prog = open_instruction_file(filename);
    int prog_size = read_program(prog);
    close_instruction_file(prog);
    if (tracing) {
	print_program(stderr, prog_size);
	fprintf(stderr, "Tracing ...\n");
	print_state(stderr);
    }
    while (!halt) {
	trace_execute(stderr, code[PC]);
    }
    return;
}


// read the program from the given filename
int read_program(FILE * prog)
{
    int count = 0;
    while (count < MAX_CODE_LENGTH) {
	code[count] = read_instruction(prog);

	if (stop_reading) {
	    break;
	} else {
	    count++;
	}
    }
    if (count >= MAX_CODE_LENGTH) {
	bail_with_error("Error: too many instructions!");
    }
    return count;
}

// print all the instructions in the program to out
void print_program(FILE *out, int size)
{
    // print the table of output header
    print_instruction_heading(out);
    // print the instructions in order
    for (int i = 0; i < size; i++) {
	print_instr_with_addr(out, i, code[i]);
    }
}

// Requires: out is open for writing
// Print instr, execute instr, then the machine's state (to the file out)
void trace_execute(FILE *out, instruction instr)
{
    if (tracing) {
	fprintf(out, "==> addr: ");
	print_instr_with_addr(out, PC, instr);
    }
    execute(instr);
    if (tracing) {
	print_state(out);
    }
}

// Execute the given instruction, setting halt to true if the machine
// should halt (due to a HLT instruction being executed).
void execute(instruction instr)
{
    PC++;
    halt = false;
    switch (instr.op) {
    case 0: // NOP
	// do nothing
	break;
    case 1: // LIT
	stack_push(instr.m);
	break;
    case 2: // RTN
	stack_return(&PC); // restore old PC
	break;
    case 3: // CAL
	stack_call(PC); // save old PC and set static link
	PC = instr.m;
	break;
    case 4: // POP
	stack_pop();
	break;
    case 5: // PSI
	{
	    int addr = stack_pop();
	    stack_push(stack_fetch(addr));
	}
	break;
    case 6: // LOD
      {
	address loc = stack_pop() + instr.m;
	stack_push(stack_fetch(loc));
      }
	break;
    case 7: // ST0
      {
	word val = stack_pop();
	address dest = stack_pop() + instr.m;
	stack_assign(dest, val);
      }
	break;
    case 8: // INC
	stack_allocate(instr.m);
	break;
    case 9: // JMP
	PC = PC - 1 + instr.m;
	break;
    case 10: // JPC
	{
	    word top_elem = stack_pop();
	    if (top_elem != 0) {
		PC = PC - 1 + instr.m;
	    }
	}
	break;
    case 11: // CHO
	fputc(stack_pop(), stdout);
	break;
    case 12: // CHI
	stack_push(fgetc(stdin));
	break;
    case 13: // HLT
	halt = true;
	break;
    case 14: // NDB
	tracing = false;
	break;
    case 15: // NEG
	stack_push(- stack_pop());
	break;
    case 16: // ADD
	{    // C technically doesn't guarantee ordering inside expressions
	    word topval = stack_pop();
	    word second = stack_pop();
	    stack_push(second+topval);
	}
	break;
    case 17: // SUB
	{   // C technically doesn't guarantee ordering inside expressions
	    int topval = stack_pop();
	    int second = stack_pop();
	    stack_push(second - topval);
	}
	break;
    case 18: // MUL
	{    // C technically doesn't guarantee ordering inside expressions
	    word topval = stack_pop();
	    word second = stack_pop();
	    stack_push(second*topval);
	}
	break;
    case 19: // DIV
	{    // C technically doesn't guarantee ordering inside expressions
	    word topval = stack_pop();
	    word second = stack_pop();
	    if (topval == 0) {
		bail_with_error("Divisor is zero in DIV instruction!");
	    }
	    stack_push(second / topval);
	}
	break;
    case 20: // MOD
	{
	    word topval = stack_pop();
	    word second = stack_pop();
	    if (topval == 0) {
		bail_with_error("Modulus is zero in MOD instruction!");
	    }
	    stack_push(second % topval);
	}
	break;
    case 21: // EQL
	stack_push(stack_pop() == stack_pop());
	break;
    case 22: // NEQ
	stack_push(stack_pop() != stack_pop());
	break;
    case 23: // LSS
	{   // C technically doesn't guarantee ordering inside expressions
	    int topval = stack_pop();
	    int second = stack_pop();
	    stack_push(second < topval);
	}
	break;
    case 24: // LEQ
	{   // C technically doesn't guarantee ordering inside expressions
	    int topval = stack_pop();
	    int second = stack_pop();
	    stack_push(second <= topval);
	}
	break;
    case 25: // GTR
	{   // C technically doesn't guarantee ordering inside expressions
	    int topval = stack_pop();
	    int second = stack_pop();
	    stack_push(second > topval);
	}
	break;
    case 26: // GEQ
	{   // C technically doesn't guarantee ordering inside expressions
	    int topval = stack_pop();
	    int second = stack_pop();
	    stack_push(second >= topval);
	}
	break;
    case 27: // PSP
	stack_push(stack_size());
	break;
    case 28: // PBP
	stack_push(stack_AR_base());
	break;
    case 29: // PPC
	stack_push(PC);
	break;
    case 30: // JMI
	PC = stack_pop();
	break;
    default:
	bail_with_error("Undefined opcode: %d", instr.op);
	break;
    }
}

// invariant test for the VM (for debugging purposes)
void okay()
{
    assert(0 <= PC && PC < MAX_CODE_LENGTH);
}

// Initialization of the VM
void initialize()
{
    stack_initialize();
    stop_reading = false;
    halt = false;
    tracing = true;
    instruction zero_instruction = {0,0};
    // initialize the code array
    for (int i = 0; i < MAX_CODE_LENGTH; i++) {
	code[i] = zero_instruction;
    }
    PC = 0;
}

void print_state(FILE *out)
{
    // control registers
    fprintf(out, "PC: %d BP: %d SP: %d\n", PC, stack_AR_base(), stack_size());
    // stack between BP and SP
    fprintf(out, "stack: ");
    stack_print(out);
}
