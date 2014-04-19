;; makefile mode setup

(define-skeleton makefile-main
  "Insert skel."
  "App name: "
  "## " str "

include config.mk

## Properties.
APPNAME = \"" str "\"
BIN = " (downcase str) "
VERSION = U
AUTHOR = \"Pierre Neidhardt\"
MANPAGE = \"NONE\"
MANSECTION = \"1\"
MAIL = \"<ambrevar@gmail.com>\"
YEAR = \"" (format-time-string "%Y") "\"

## Resources.
DEPS =
SRC += " str ".c
OBJ = ${SRC:.c=.o}

## Appends user flags.
CFLAGS_ALL += ${CFLAGS}
LDFLAGS_ALL += ${LDFLAGS}

## Application information.
CFLAGS_ALL += -DAPPNAME=\\\"${APPNAME}\\\"
CFLAGS_ALL += -DVERSION=\\\"${VERSION}\\\"
CFLAGS_ALL += -DAUTHOR=\\\"${AUTHOR}\\\"
CFLAGS_ALL += -DYEAR=\\\"${YEAR}\\\"
CFLAGS_ALL += -DMANPAGE=\\\"${MANPAGE}\\(${MANSECTION}\\)\\\"

## Compiler options.
CFLAGS_ALL += -Wall -Wextra -Wshadow -Winline -pedantic -std=c11

## Standard compliance options.
CFLAGS_ALL += -D_XOPEN_SOURCE=700 ## For use of getopt() with -pedantic.

## Dependencies
CFLAGS_ALL += $$(pkg-config --cflags ${DEPS})
LDFLAGS_ALL += $$(pkg-config --libs ${DEPS})

################################################################################
## Rules

all: options ${BIN}

options:
	@echo ${BIN} build options:
	@echo \"CC      = ${CC}\"
	@echo \"CFLAGS  = ${CFLAGS_ALL}\"
	@echo \"LDFLAGS = ${LDFLAGS_ALL}\"

${BIN}: ${OBJ}
	@${CC} ${LDFLAGS_ALL} $^ -o $@

%.o: %.c
	@${CC} ${CFLAGS_ALL} $< -c -o $@

clean:
	@rm ${BIN} ${OBJ}

.PHONY=all options clean" > \n)

(provide 'mode-makefile)
;; end of file
