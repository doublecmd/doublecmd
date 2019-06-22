all: fpmake
	${CURDIR}/fpmake --verbose

clean: fpmake
	${CURDIR}/fpmake clean --verbose

plugins: fpmake
	${CURDIR}/fpmake plugins --verbose

components: fpmake
	${CURDIR}/fpmake components --verbose

fpmake:
	fpc fpmake.pp

.PHONY: all clean plugins components
