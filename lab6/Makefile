# Makefile for lab 6 programs.

DIFF = diff --strip-trailing-cr

compile: sexpr xml
test: lab6a_test sexpr_test xml_test
all: compile test

lab6a_test: State.hs Tests_Lab6a.hs
	@echo
	@echo TESTING PART A...
	@echo
	ghc -W -o tests_lab6a Tests_Lab6a.hs -package base -package mtl
	./tests_lab6a

sexpr: Sexpr.hs
	@echo
	@echo COMPILING SEXPR PARSER...
	@echo
	ghc -W -o sexpr Sexpr.hs -package base -package parsec

sexpr_test:
	@echo
	@echo TESTING SEXPR PARSER...
	@echo
	rm -f sexpr.out
	./sexpr test.scm > sexpr.out
	${DIFF} sexpr.out sexpr.expected

xml: XML.hs
	@echo
	@echo COMPILING XML PARSER...
	@echo
	ghc -W -o xml XML.hs -package base -package parsec

xml_test:
	@echo
	@echo TESTING XML PARSER...
	@echo
	rm -f xml.out
	./xml test.xml > xml.out
	${DIFF} xml.out xml.expected

clean:
	rm -f *.hi *.dyn_hi *.o *.dyn_o sexpr sexpr.out xml xml.out tests_lab6a
