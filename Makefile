all:
	csc -prologue pat.scm autoref.scm -o autoref

test:
	sh runtests.sh
