all: fpmake
	${CURDIR}/fpmake.com --verbose

clean: fpmake
	${CURDIR}/fpmake.com $@ --verbose
ifeq (clean,$(lastword $(MAKECMDGOALS)))
	rm -f fpmake.com
endif

plugins: fpmake
	${CURDIR}/fpmake.com $@ --verbose

components: fpmake
	${CURDIR}/fpmake.com $@ --verbose

fpmake:
	fpc fpmake.pp -ofpmake.com

.PHONY: all clean plugins components
