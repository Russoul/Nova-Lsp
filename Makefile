# Executable we're building
NAME = nova-lsp
TARGETDIR = ${CURDIR}/build/exec
TARGET = ${TARGETDIR}/${NAME}

.PHONY: build

build:
	idris2 --build nova-lsp.ipkg

# Just an alias for build
all: build

clean:
	idris2 --clean nova-lsp.ipkg
	$(RM) -r build

repl:
	rlwrap idris2 --repl nova-lsp.ipkg

testbin:
	@${MAKE} -C tests testbin

# usage: `make test only=messages001`
test-only:
	${MAKE} -C tests only=$(only)

test: build testbin test-only

install:
	mkdir -p ${PREFIX}/bin/
	install ${TARGET} ${PREFIX}/bin
ifeq ($(OS), windows)
	-install ${TARGET}.cmd ${PREFIX}/bin
endif
	mkdir -p ${PREFIX}/bin/${NAME}_app
	install ${TARGETDIR}/${NAME}_app/* ${PREFIX}/bin/${NAME}_app
