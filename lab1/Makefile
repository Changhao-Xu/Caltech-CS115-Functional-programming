TEST = test_lab1

default: test

compile:
	ghc Tests_Lab1.hs -o ${TEST} -package base

test: compile
	./${TEST}

clean:
	rm -f *.hi *.o ${TEST}
