/* $Id: stack.c,v 1.14 2023/03/20 17:13:53 leavens Exp $ */
#include <stdlib.h>
#include "utilities.h"
#include "stack.h"

// the stack's storage
static word stack[MAX_STACK_HEIGHT];

// index of next free element
static address sp;

// first index of current AR
static address bp;

// Does the stack's invariant hold?
// (Exit with an error message if not)
static void stack_invariant() {
    if (!(0 <= bp)) {
	fprintf(stderr, "VM stack invariant failure: BP (%d) < 0!\n",
		bp);
    } else if (!(bp <= sp)) {
	fprintf(stderr, "VM stack invariant failure: SP (%d) < BP (%d)!\n",
		sp, bp);
    } else if (!(0 <= sp)) {
	fprintf(stderr, "VM stack invariant failure: SP (%d) < 0!\n",
		sp);
    } else if (!(sp < MAX_STACK_HEIGHT)) {
	fprintf(stderr, "VM stack invariant failure: MAX_STACK_HEIGHT (%d) <= SP (%d)!\n",
		MAX_STACK_HEIGHT, sp);
    } else {
	return; // the invariant holds
    }
    exit(EXIT_FAILURE);
}

// Initialize the stack data structure
void stack_initialize()
{
    sp = 0;
    bp = 0;
    for (address i = 0; i < MAX_STACK_HEIGHT; i++) {
	stack[i] = 0;
    }
    stack_invariant();
}

// Is the given address legal for the stack?
static bool legal_stack_index(address addr)
{
    return 0 <= addr && addr < MAX_STACK_HEIGHT;
}

// Return the stack's num. of elements
// (SP value)
address stack_size() { return sp; }

// Return the address of the base
// of the current AR (BP value)
address stack_AR_base() { return bp; }

// Is the stack empty?
bool stack_empty() { return sp == 0; }

// Is the stack full?
bool stack_full() {
    return sp == MAX_STACK_HEIGHT-1;
}

// Requires: !stack_full()
// push a word on the stack
void stack_push(word val) {
    if (stack_full()) {
	bail_with_error(
          "Trying to push on a full stack!\n");
    }
    stack[sp] = val;  // or stack[sp++] = val;
    sp += 1;
    stack_invariant();
}

// Requires: stack_size() + n
//                  < MAX_STACK_HEIGHT
// Increase the size of the stack by n
void stack_allocate(unsigned int n)
{
    int new_sp = sp + n;
    if (!legal_stack_index(new_sp)) {
	bail_with_error("Can't increase stack size to %d in stack_allocate",
			new_sp);
    }
    sp = new_sp;
    stack_invariant();
}

// Requires: !stack_empty()
// pop the stack and return the top elem
word stack_pop() {
    if (stack_empty()) {
	bail_with_error("Trying to pop an empty stack!");
	exit(2);
    } else {    // whole block could be: return stack[sp--];
	sp = sp-1;
	return stack[sp];
    }
    stack_invariant();
}

// return the top element without popping
word stack_top() {
    if (stack_empty()) {
	bail_with_error("Trying to pop an empty stack!");
	exit(2);
    } else {
	return stack[sp-1]; // don't use -- here!
    }
    stack_invariant();
}

// fetch the value from the given address
word stack_fetch(address addr)
{
    if (!legal_stack_index(addr)) {
	bail_with_error("Illegal stack index in stack_fetch: %d", addr);
    }
    return stack[addr];
}

// assign val to the given address, addr, on the stack
void stack_assign(address addr, word val)
{
    if (!legal_stack_index(addr)) {
	bail_with_error("Illegal stack index in stack_assign: %d", addr);
    }
    stack[addr] = val;
}

// Requires: stack_size()+2
//                  < MAX_STACK_HEIGHT
// call a subroutine
// without any static link
void stack_call_no_stat_lnk(int ret_addr)
{
    // Assume parameters are on top of stack
    int old_bp = bp;
    int old_sp = sp;
    stack_push(old_bp); // dynamic link
    stack_push(ret_addr);
    bp = old_sp; // base of the new AR
    stack_invariant();
}

// Requires: stack_size()+LINKS_SIZE
//                  < MAX_STACK_HEIGHT
// call a subroutine pushing the static link (at bp)
void stack_call(int ret_addr)
{
    // not dealing with parameters
    int old_bp = bp;
    int old_sp = sp;
    stack_push(stack_fetch(old_bp)); // static link
    stack_push(old_bp); // dynamic link
    stack_push(ret_addr);
    bp = old_sp; // base of the new AR
}

// Requires: saved BP and SP values
//           will satisfy stack's invariant
// return (with no result) from subroutine;
// this assumes there is no static link
void stack_return_no_stat_lnk(int *PC)
{
    // restore PC
    *PC = stack_pop();
    // restore BP
    bp = stack_pop();
    stack_invariant();
}

// Requires: saved BP and SP values
//           will satisfy stack's invariant
// return (with no result) from subroutine
void stack_return(int *PC)
{
    // restore PC
    *PC = stack_pop();
    // restore BP
    bp = stack_pop();
    // toss the old static link
    stack_pop();
    stack_invariant();
}

// Return from a subroutine
// (with a static link saved)
// and push the given return value
void stack_return_value(int *PC,
                        word fun_value)
{
    // restore PC
    *PC = stack_pop();
    // restore BP
    bp = stack_pop();
    // toss the old static link
    stack_pop();
    // push the function's value
    stack_push(fun_value);
    stack_invariant();
}

// print the stack's values in the current AR
// (between stack_base() and stack_size()-1)
void stack_print(FILE *out)
{
    for (address a = bp; a < MAX_STACK_HEIGHT && a < sp; a++) {
	fprintf(out, "S[%d]: %d ", a, stack[a]);
    }
    fprintf(out, "\n");

}
