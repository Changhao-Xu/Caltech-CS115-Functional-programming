SRCA  = Tests_SignedNat
SRCC  = Tests_SparseMatrix
TESTA = tests_SignedNat
TESTC = tests_SparseMatrix
PKG	  = -package base -package containers
OPTS  = -W

test: test_partA test_partC

test_partA:
	ghc ${OPTS} ${SRCA}.hs -o ${TESTA} ${PKG}
	./${TESTA}

test_partC:
	ghc ${OPTS} ${SRCC}.hs -o ${TESTC}  ${PKG}
	./${TESTC}

clean:
	rm -f *.hi *.o *dyn_o *dyn_hi ${TESTA} ${TESTC}
