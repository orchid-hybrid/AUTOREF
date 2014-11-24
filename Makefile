all:
	csc -prologue pat.scm \
            -prologue atomic.scm \
            -prologue builtins.scm \
            -prologue verify.scm \
          autoref.scm -o autoref

test:
	sh runtests.sh
