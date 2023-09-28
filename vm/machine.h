/* $Id: machine.h,v 1.8 2023/03/22 14:28:25 leavens Exp leavens $ */
#ifndef _MACHINE_H
#define _MACHINE_H
#include <stdio.h>
#include <stdbool.h>

// maximum height of the runtime stack
#define MAX_STACK_HEIGHT 2048
// maximum number of instructions in a program
#define MAX_CODE_LENGTH 512

// print tracing output?
extern bool tracing;

// print the state of the machine (named registers)
extern void print_state(FILE *out);

// Requires: out is open for writing
// Print instr, execute instr, then the machine's state (to the file out)
extern void trace_execute(FILE *out, instruction instr);

// Execute the given instruction, setting halt to true if the machine
// should halt (due to a HLT instruction being executed).
extern void execute(instruction instr);
#endif
