# $Id: Makefile,v 1.31 2023/04/14 14:30:46 leavens Exp leavens $
# Makefile for PL/0 compiler and code generation

# Add .exe to the end of target to get that suffix in the rules
COMPILER = compiler
LEXER = ./compiler -l
UNPARSER = ./compiler -u
VM = vm
CC = gcc
# on Linux, the following can be used with gcc:
# CFLAGS = -fsanitize=address -static-libasan -g -std=c17 -Wall
CFLAGS = -g -std=c17 -Wall
MV = mv
RM = rm -f
SUBMISSIONZIPFILE = submission.zip
ZIP = zip -9
SOURCESLIST = sources.txt
SUF = pl0
TESTS = hw3-asttest*.$(SUF) hw3-parseerrtest*.$(SUF) hw3-declerrtest*.$(SUF) hw4-asttest*.$(SUF) hw4-parseerrtest*.$(SUF) hw4-declerrtest*.$(SUF)
VMTESTS = hw4-vmtest*.$(SUF)
EXPECTEDOUTPUTS = `echo $(TESTS) | sed -e 's/\\.$(SUF)/.out/g'`
EXPECTEDVMINPUTS = `echo $(VMTESTS) | sed -e 's/\\.$(SUF)/.vmi/g'`
EXPECTEDVMOUTPUTS = `echo $(VMTESTS) | sed -e 's/\\.$(SUF)/.vmo/g'`

# create the VM executable
.PRECIOUS: $(VM)/$(VM)
$(VM): $(VM)/$(VM)

$(VM)/$(VM):
	cd $(VM); $(MAKE) $(VM)

# create the compiler executable
$(COMPILER): *.c *.h
	$(CC) $(CFLAGS) -o $(COMPILER) `cat $(SOURCESLIST)`

# rule for compiling individual .c files
%.o: %.c %.h
	$(CC) $(CFLAGS) -c $<

.PHONY: clean
clean:
	$(RM) *~ *.o *.myo '#'*
	$(RM) $(COMPILER).exe $(COMPILER)
	$(RM) *.stackdump core
	$(RM) $(SUBMISSIONZIPFILE)

# Rules for making individual outputs (e.g., execute  make vmtest1.myvi)

# the .myvi files are outputs of the compiler on the given PL/0 programs
.PRECIOUS: %.myvi
%.myvi: %.$(SUF) $(COMPILER)
	./$(COMPILER) $< > $@

# the .myvo files are outputs from running compiled .myvi files in the VM
.PRECIOUS: %.myvo
%.myvo: %.myvi $(VM)
	$(VM)/$(VM) $< > $@ 2>&1

# main target for testing
.PHONY: check-outputs check-vm-outputs
check-outputs: check-vm-outputs

check-vm-outputs: $(VM) $(COMPILER) $(VMTESTS)
	DIFFS=0; \
	for f in `echo $(VMTESTS) | sed -e 's/\\.$(SUF)//g'`; \
	do \
		echo compiling "$$f.$(SUF)" ...; \
		$(RM) "$$f.myvi"; \
		./$(COMPILER) "$$f.$(SUF)" > "$$f.myvi"; \
		echo running "$$f.myvi" in the VM ...; \
		vm/vm "$$f.myvi" > "$$f.myvo" 2>&1; \
		diff -w -B "$$f.vmo" "$$f.myvo" && echo 'passed!' \
			|| { echo 'failed!'; DIFFS=1; }; \
	done; \
	if test 0 = $$DIFFS; \
	then \
		echo 'All tests passed!'; \
	else \
		echo 'Test(s) failed!'; \
	fi

# Automatically generate the submission zip file
$(SUBMISSIONZIPFILE): $(SOURCESLIST) *.c *.h *.myvi *.myvo
	$(ZIP) $(SUBMISSIONZIPFILE) $(SOURCESLIST) *.c *.h *.myvi *.myvo Makefile

# Automatically regenerate the sources.txt file
.PRECIOUS: $(SOURCESLIST)
$(SOURCESLIST):
	echo *.c > $(SOURCESLIST)

# the .myo files are for testing the compiler by itself (the front end)
.PRECIOUS: %.myo
%.myo: %.$(SUF) $(VM) $(COMPILER)
	./$(UNPARSER) $< > $@ 2>&1

# instructor's section below...

.PRECIOUS: %.out
%.out: %.$(SUF) $(COMPILER)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	./$(UNPARSER) $< > $@ 2>&1

.PRECIOUS: %.vmi
%.vmi: %.$(SUF) $(VM) $(COMPILER)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	./$(COMPILER) $< > $@

.PRECIOUS: %.vmo
%.vmo: %.vmi $(VM)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should NOT use the target $@,'; \
		echo 'as using this target ($@) will invalidate a test'; \
		exit 1; \
	fi
	$(VM)/$(VM) $< > $@ 2>&1

.PHONY: create-outputs create-vm-outputs
create-outputs: $(VM) $(COMPILER) $(TESTS) $(VMTESTS) 
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-outputs,' ;\
		echo 'as using this target (create-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	$(MAKE) create-non-vm-outputs create-vm-outputs

create-non-vm-outputs: $(COMPILER) $(TESTS) 
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-non-vm-outputs,'; \
		echo 'as using this target (create-non-vm-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	for f in `echo $(TESTS) | sed -e 's/\\.$(SUF)//g'`; \
	do \
		echo running "$$f.$(SUF)"; \
		$(RM) "$$f.out"; \
		./$(COMPILER) -u "$$f.$(SUF)" >"$$f.out" 2>&1; \
	done; \
	echo 'done creating non-VM test outputs!'

create-vm-outputs: $(VM) $(COMPILER) $(VMTESTS)
	@if test '$(IMTHEINSTRUCTOR)' != true ; \
	then \
		echo 'Students should use the target check-vm-outputs,'; \
		echo 'as using this target (create-vm-outputs) will invalidate the tests!' ; \
		exit 1; \
	fi
	for f in `echo $(VMTESTS) | sed -e 's/\\.$(SUF)//g'`; \
	do \
		echo compiling "$$f.$(SUF)"; \
		$(RM) "$$f.vmi"; \
		./$(COMPILER) "$$f.$(SUF)" > "$$f.vmi"; \
		echo running "$$f.vmi"; \
		vm/vm "$$f.vmi" > "$$f.vmo" 2>&1; \
	done
	echo 'done creating VM test outputs!'

.PHONY: digest
digest digest.txt: 
	for f in `ls $(TESTS) | sed -e 's/\\.$(SUF)//g'`; \
        do cat $$f.$(SUF); echo " "; cat $$f.out; echo " "; echo " "; \
        done >digest.txt

.PHONY: vmdigest
vmdigest vmdigest.txt: create-vm-outputs
	for f in `ls $(VMTESTS) | sed -e 's/\\.$(SUF)//g'`; \
        do cat $$f.$(SUF); echo " "; cat $$f.vmo; echo " "; echo " "; \
        done >vmdigest.txt

# don't use develop-clean unless you want to regenerate the expected outputs
.PHONY: develop-clean
develop-clean: clean
	$(RM) $(EXPECTEDOUTPUTS) $(EXPECTEDVMOUTPUTS) digest.txt vmdigest.txt
	cd $(VM); $(MAKE) clean

TESTSZIPFILE = ~/WWW/COP3402/homeworks/hw4-tests.zip
PROVIDEDFILES = compiler_main.c \
		token.[ch] lexer_output.[ch] utilities.[ch] lexer.[ch] \
		reserved.[ch] \
		ast.[ch] parser.[ch] parserInternal.h \
		unparser.[ch] unparserInternal.h \
		file_location.[ch] id_attrs.[ch] id_use.[ch] \
		scope.[ch] scope_check.[ch] symtab.[ch] \
		code.[ch] instruction.[ch] machine_types.h \
		label.[ch] lexical_address.[ch] gen_code.[ch]

.PHONY: stubs
stubs: gen_code_stubs.c
	test -f gen_code_actual.c || $(MV) gen_code.c gen_code_actual.c
	$(MV) gen_code_stubs.c gen_code.c

.PHONY: gen_code_restore
restore: gen_code_actual.c
	test -f gen_code_stubs.c || $(MV) gen_code.c gen_code_stubs.c
	$(MV) gen_code_actual.c gen_code.c
	chmod u+w *

.PHONY: zip
zip hw4-tests.zip: create-vm-outputs stubs $(TESTSZIPFILE) restore

$(TESTSZIPFILE): $(TESTS) $(VMTESTS) Makefile $(PROVIDEDFILES)
	test -f gen_code_actual.c || exit 1
	$(RM) $(TESTSZIPFILE)
	chmod 444 $(VMTESTS) $(PROVIDEDFILES) $(EXPECTEDVMINPUTS) $(EXPECTEDVMOUTPUTS) Makefile
	chmod u+w gen_code.c Makefile 
	$(ZIP) $(TESTSZIPFILE) $(VMTESTS) $(EXPECTEDVMINPUTS) $(EXPECTEDVMOUTPUTS) $(PROVIDEDFILES) Makefile
	$(ZIP) $(TESTSZIPFILE) -x '$(VM)/RCS' -x '$(VM)/RCS/*' -x '$(VM)/$(VM).exe' -x '$(VM)/*.out' -x '$(VM)/*.myo' -x '$(VM)/*.tex' -x '$(VM)/*.aux' -x '$(VM)/$(VM)/*.vmi' -r $(VM)

.PHONY: check-separately
check-separately:
	$(CC) $(CFLAGS) -c *.c
