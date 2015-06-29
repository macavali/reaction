SRC=../Flow
LITERATE=${SRC}/Kappa.lhs ${SRC}/Internal/KappaAST.lhs
DOC=reaction
LATEX=xelatex -shell-escape -interaction=nonstopmode

all: ${DOC}.pdf

${DOC}.pdf: ${DOC}.tex ${LITERATE}
	${LATEX} $<
	${LATEX} $<

clean:
	rm -f ${DOC}.pdf ${DOC}.aux ${DOC}.log
