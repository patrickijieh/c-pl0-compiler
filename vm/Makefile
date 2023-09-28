# $Id: Makefile,v 1.22 2023/03/22 14:28:06 leavens Exp leavens $
# Makefile for the stack-based VM

CC = gcc
CFLAGS = -g -std=c17 -Wall
RM = rm -f
MYO = .myo
VM = vm
SOURCESLIST = sources.txt
TESTS = test*.vmi

$(VM): *.c *.h
	$(CC) $(CFLAGS) -o $(VM) `cat $(SOURCESLIST)`

%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	$(RM) *~ *.o *$(MYO) '#'* *.log
	$(RM) $(VM).exe $(VM)
	$(RM) *.stackdump core

# make file.out from file.vmi by running the VM
%.out: %.vmi $(VM)
	./$(VM) $< > $@ 2>&1

.PRECIOUS: %.myo
%$(MYO): %.vmi $(VM)
	./$(VM) $< > $@ 2>&1

check-outputs: $(VM) $(TESTS)
	DIFFS=0; \
	for f in `echo $(TESTS) | sed -e 's/\\.vmi//g'`; \
	do \
		echo running "$$f.vmi"; \
		./$(VM) "$$f.vmi" >"$$f$(MYO)" 2>&1; \
		diff -w -B "$$f.out" "$$f$(MYO)" && echo 'passed!' || DIFFS=1; \
	done; \
	if test 0 = $$DIFFS; \
	then \
		echo 'All tests passed!'; \
	else \
		echo 'Test(s) failed!'; \
	fi

.PRECIOUS: $(SOURCESLIST)
$(SOURCESLIST):
	echo *.c > $(SOURCESLIST)

# developer's section below...

.PRECIOUS: %.out
%.out: %.vmi $(VM)
	./$(VM) $< > $@ 2>&1

.PHONY: create-outputs
create-outputs: $(VM) $(TESTS)
	@echo 'Students should use the target check-outputs,'
	@echo 'as using this target (create-outputs) will invalidate the tests'
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		exit 1; \
	fi; \
	for f in `echo $(TESTS) | sed -e 's/\\.vmi//g'`; \
	do \
		echo running "$$f.vmi"; \
		$(RM) "$$f.out"; \
		./$(VM) "$$f.vmi" >"$$f.out" 2>&1; \
	done; \
	echo done creating test outputs!

.PHONY: digest
digest digest.vmi: create-outputs
	for f in `ls $(TESTS) | sed -e 's/\\.vmi//g'`; \
        do echo '#' $$f.vmi; cat $$f.out; echo " "; echo " "; \
        done >digest.vmi

# don't use develop-clean unless you want to regenerate the expected outputs
.PHONY: develop-clean
develop-clean: clean
	$(RM) *.out digest.vmi
