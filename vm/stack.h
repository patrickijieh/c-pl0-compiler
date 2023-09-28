/* $Id: stack.h,v 1.11 2023/03/20 17:13:53 leavens Exp $ */
#ifndef _STACK_H
#define _STACK_H
#include <stdbool.h>
#include <stdio.h>
#include "machine_types.h"

#define MAX_STACK_HEIGHT 2048

// Initialize the stack data structure
extern void stack_initialize();

// Return the stack's num. of elements (SP value)
extern address stack_size();

// Return the address of the base
// of the current AR (BP value)
extern address stack_AR_base();

// Is the stack empty?
extern bool stack_empty();

// Is the stack full?
extern bool stack_full();

// push a word on the stack
extern void stack_push(word val);

// Increase the size of the stack by n
void stack_allocate(unsigned int n);

// pop the stack and return the top elem
extern word stack_pop();

// return the top element without popping
extern word stack_top();

// fetch the value from the given address
extern word stack_fetch(address addr);

// assign val to the given address, addr, on the stack
extern void stack_assign(address addr, word val);

// call a subroutine
// without any static link
void stack_call_no_stat_lnk(int ret_addr);

// call a subroutine
extern void stack_call(int ret_addr);

// return (with no result) from subroutine
// this assumes there is no static link
void stack_return_no_stat_lnk(int *PC);

// return (with no result) from subroutine
extern void stack_return(int *PC);

// return given value from a subroutine
extern void stack_return_value(
	        int *PC,
   	        word fun_value);

// print the stack's values in the current AR
// (between stack_base() and stack_size()-1)
extern void stack_print(FILE *out);
#endif
