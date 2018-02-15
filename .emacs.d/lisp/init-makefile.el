;;; Makefile

(ambrevar/define-keys makefile-mode-map
                      "C-c c" 'ambrevar/makefile-config
                      "C-c d" 'ambrevar/makefile-doc
                      "C-c m" 'ambrevar/makefile-main
                      "C-c s" 'ambrevar/makefile-c)

(define-skeleton ambrevar/makefile-main
  "Insert root Makefile." nil
  "ROOT = .
include ${ROOT}/config.mk

################################################################################
## Build.

.PHONY: all
all: app doc

.PHONY: app
app:
  ${MAKE} -C ${srcdir}

.PHONY: doc
doc:
  ${MAKE} -C ${docsrcdir}

.PHONY: debug
debug:
  CFLAGS+=\"-g3 -O0 -DDEBUG=9\" ${MAKE}

.PHONY: test
test:
  ${MAKE} -C ${testdir}

.PHONY: clean
clean:
  ${MAKE} -C ${srcdir} clean
  ${MAKE} -C ${docsrcdir} clean
  ${MAKE} -C ${testdir} clean

################################################################################
## Install / Uninstall.

INSTALL ?= install
INSTALL_DATA ?= ${INSTALL} -m644
INSTALL_DIR ?= ${INSTALL} -d

prefix ?= /usr/local
exec_prefix ?= ${prefix}
datarootdir ?= ${prefix}/share

bindir ?= ${exec_prefix}/bin
datadir ?= ${datarootdir}
docdir ?= ${datarootdir}/doc
includedir ?= ${prefix}/include
infodir ?= ${datarootdir}/info
libdir ?= ${exec_prefix}/lib
libexecdir ?= ${exec_prefix}/libexecdir
licensedir ?= ${datarootdir}/licenses
localedir ?= ${datarootdir}/locale
localstatedir ?= ${prefix}/var
mandir ?= ${datarootdir}/man
runstatedir ?= ${prefix}/run
sbindir ?= ${exec_prefix}/sbin
sharedstatedir ?= ${prefix}/com
sysconfdir ?= ${perfix}/etc

.PHONY: install
install:
  ${MAKE}
  ${INSTALL_DIR} ${DESTDIR}${bindir}
  ${INSTALL} ${srcdir}/${cmdname} ${DESTDIR}${bindir}/${cmdname}
  ${INSTALL_DIR} ${DESTDIR}${mandir}/man1
  ${INSTALL_DATA} ${docsrcdir}/${cmdname}.1 ${DESTDIR}${mandir}/man1/${cmdname}.1
  ${INSTALL_DIR}  ${DESTDIR}${licensedir}/${cmdname}
  ${INSTALL_DATA} LICENSE ${DESTDIR}${licensedir}/${cmdname}/LICENSE

.PHONY: uninstall
uninstall:
  -rm -f ${DESTDIR}${bindir}/${cmdname}
  -rmdir -p ${DESTDIR}${bindir}
  -rm -f ${DESTDIR}${mandir}/${cmdname}.${mansection}.gz
  -rmdir -p ${DESTDIR}${mandir}
  -rm -f ${DESTDIR}${licensedir}/${cmdname}/LICENSE
  -rmdir -p ${DESTDIR}${licensedir}/${cmdname}
" > \n)

(define-skeleton ambrevar/makefile-doc
  "Insert documentation Makefile." nil
  ".SUFFIXES: .in

ROOT ?= ..
include ${ROOT}/config.mk

manpages = ${cmdname}.1

all: ${manpages}

.in:
  awk -v date=`date '+%Y-%m-%d'` 'BEGIN {section=\"$@\"; sub (/.*\\./, \"\", section); manname=toupper(\"$@\"); sub(/\\.[^.]+$$/, \"\", manname); print \".ds appname ${appname}\\n.ds authors ${authors}\\n.ds cmdname ${cmdname}\\n.ds date \" date \"\\n.ds manname \" manname \"\\n.ds section \" section \"\\n.ds version ${version}\\n.ds year ${year}\\n.\"}' > $@
  cat $< >> $@

clean:
  rm -f ${manpages}
" > \n)

(define-skeleton ambrevar/makefile-c
  "Insert Makefile for building c." nil
  "ROOT ?= ..
include ${ROOT}/config.mk

CPPFLAGS += -DAUTHORS=\"${authors}\" -DVERSION=${version} -DYEAR=${year}
CPPFLAGS += -D_POSIX_C_SOURCE=200809L
CPPFLAGS += -DHAVE_INLINE
LDLIBS += -lm

${cmdname}: ${cmdname}.o

.PHONY: debug
debug:
  CFLAGS+=\"-g3 -O0 -DDEBUG=9\" ${MAKE}

.PHONY: clean
clean:
  rm -f ${cmdname} *.d *.o

## Generate prerequisites automatically. GNU Make only.
## The 'awk' part is used to add the .d file itself to the target, so that it
## gets updated on changes. The -MQ option is to add full path to object files
## in subfolders. (-MM strips parent directories.)
%.d: %.c
  ${CC} -MM -MQ ${<:.c=.o} ${CPPFLAGS} $< | awk -v stem=$* -v target=$@ '{gsub (stem \".o[ :]*\", stem \".o \" target \" : \")} 1' > $@

sources = $(wildcard *.c)
deps = ${sources:.c=.d}
-include ${deps}
" > \n)

(define-skeleton ambrevar/makefile-config
  "Insert Makefile config." nil
  "## This file is included by all (sub-)makefiles.

## Properties.
appname = Name
authors = Pierre Neidhardt
cmdname = name
url =
version = 1.0
year = " (format-time-string "%Y") "

## Folders.
srcdir = src
docsrcdir = doc
testdir = tests

## USER SETTINGS

## Optional compilation flags.
CFLAGS ?= -pedantic -std=c99 -Wall -Wextra -Wshadow

## END OF USER SETTINGS
" > \n)

(provide 'init-makefile)
