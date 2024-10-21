SRC   = Tests_Lab2.hs
TEST  = test_lab2
SRC2  = Tests_Lab2_TAs.hs
TEST2 = test_lab2_tas
PKG   = -package base -package containers
OPTS  = -W

default: compile

compile:
	ghc ${OPTS} ${SRC} -o ${TEST} ${PKG}

test:
	./${TEST}

compile2:
	ghc ${OPTS} ${SRC2} -o ${TEST2} ${PKG}

ta_test:
	./${TEST2}

clean:
	rm -f *.hi *.o *dyn_o *dyn_hi ${TEST} ${TEST2}
