;; makefile mode setup.

(add-hook-and-eval
 'awk-mode-hook
 (lambda ()
   (local-set-key (kbd "C-c c") 'makefile-config)
   (local-set-key (kbd "C-c d") 'makefile-doc)
   (local-set-key (kbd "C-c m") 'makefile-main)
   (local-set-key (kbd "C-c s") 'makefile-c)
   ))

(define-skeleton makefile-main
  "Insert root Makefile." nil
  "SHELL = /bin/sh
.POSIX:

include config.mk

################################################################################
## Build.

.PHONY: all
all: app doc

.PHONY: app
app:
	${MAKE} -C ${srcdir}

.PHONY: doc
doc:
	${MAKE} -C ${docsdir}

.PHONY: debug
debug:
	CFLAGS+=\"-g3 -O0 -DDEBUG=9\" ${MAKE}

.PHONY: check
check:
	CFLAGS+=\"-g3 -O0 -DDEBUG=9\" ${MAKE} -C ${testdir}

.PHONY: clean
clean:
	${MAKE} -C ${srcdir} clean
	${MAKE} -C ${docsdir} clean
	${MAKE} -C ${testdir} clean

################################################################################
## Install / Uninstall.
## Cf. the info page of GNU Make for a proper use of these variables.

INSTALL_PROGRAM ?= ${INSTALL}
INSTALL_DATA ?= ${INSTALL} -m644

prefix ?= /usr/local
exec_prefix ?= ${prefix}
bindir ?= ${exec_prefix}/bin
sbindir ?= ${exec_prefix}/sbin
libexecdir ?= ${exec_prefix}/libexecdir
datarootdir ?= ${prefix}/share
datadir ?= ${datarootdir}
sysconfdir ?= ${perfix}/etc
sharedstatedir ?= ${prefix}/com
localstatedir ?= ${prefix}/var
runstatedir ?= ${prefix}/run
includedir ?= ${prefix}/include

docdir ?= ${datarootdir}/doc/${cmdname}
infodir ?= ${datarootdir}/info
htmldir ?= ${docdir}
dvidir ?= ${docdir}
pdfdir ?= ${docdir}
psdir ?= ${docdir}

libdir ?= ${exec_prefix}/lib
lispdir ?= ${datarootdir}/emacs/site-lisp

localedir ?= ${datarootdir}/locale

mandir ?= ${datarootdir}/man
man1dir ?= ${mandir}/man1
man2dir ?= ${mandir}/man2
man3dir ?= ${mandir}/man3
man4dir ?= ${mandir}/man4
man5dir ?= ${mandir}/man5
man6dir ?= ${mandir}/man6
man7dir ?= ${mandir}/man7
man8dir ?= ${mandir}/man8
man9dir ?= ${mandir}/man9

manext ?= .1
man1ext ?= .1
man2ext ?= .2
man3ext ?= .3
man4ext ?= .4
man5ext ?= .5
man6ext ?= .6
man7ext ?= .7
man8ext ?= .8
man9ext ?= .9

.PHONY: install
install: ${cmdname}
	${INSTALL_PROGRAM} $^ ${DESTDIR}${bindir}/$^

.PHONY: uninstall
uninstall:
	rm -f ${DESTDIR}${bindir}/${cmdname}
" > \n)


(define-skeleton makefile-doc
  "Insert documentation Makefile." nil
  "ROOT ?= ..
include ${ROOT}/config.mk

${manpage}.gz: ${manpage}.in
	awk \\
	'$$0 ~ \"^.ds appname\" {print $$1 \" \" $$2 \" '${appname}'\";next} \\
	$$0 ~ \"^.ds cmdname\" {print $$1 \" \" $$2 \" '${cmdname}'\";next} \\
	$$0 ~ \"^.ds manname\" {print $$1 \" \" $$2 \" '${manname}'\";next} \\
	$$0 ~ \"^.ds version\" {print $$1 \" \" $$2 \" '${version}'\";next} \\
	$$0 ~ \"^.ds year\" {print $$1 \" \" $$2 \" '${year}'\";next} \\
	$$0 ~ \"^.ds author\" {print $$1 \" \" $$2 \" '${author}'\";next} 1' \\
	${manpage}.in > ${manpage}
	gzip -c ${manpage} > $@

clean:
	rm -f ${manpage} ${manpage}.gz
" > \n)


(define-skeleton makefile-c
  "Insert Makefile for building c." nil
"ROOT ?= ..
include ${ROOT}/config.mk

CFLAGS += `pkg-config --cflags gtk+-3.0`
LDLIBS += `pkg-config --libs gtk+-3.0`
LDLIBS += -lgsl -lgslcblas

${cmdname}: ${cmdname}.o
	${CC} ${LDFLAGS} ${TARGET_ARCH} ${cmdname}.o $(LOADLIBES) $(LDLIBS) -o $@

.PHONY: debug
debug:
	CFLAGS+=\"-g3 -O0 -DDEBUG=9\" ${MAKE}

clean:
	rm -f ${cmdname} *.d ${cmdname}.o

## Header auto-dependencies. (GNU only.)
## WARNING: filenames cannot contain a comma.
%.d: %.c
	${CC} -MM ${CPPFLAGS} $< > $@; \\
	ex -sc '%s,$*\\.o[ :]*,$*.o $@ : ,g|xit' $@;

srcfiles = ${cmdname}.c
depfiles = ${srcfiles:.c=.d}
-include ${depfiles}
" > \n)



(define-skeleton makefile-config
  "Insert Makefile config." nil
  "## This file is included by all the (sub-)makefiles.

ROOT ?= .
BUILD = ${ROOT}/build

################################################################################
## Properties.
cmdname = " (progn (string-match "\\([^/]*\\)/$" (file-name-directory buffer-file-name)) (match-string 1 (file-name-directory buffer-file-name))) "
version = 1.0
author = \"Pierre Neidhardt\"
year = " (format-time-string "%Y") "
mansection = 1
mail = \"<name [at] domain [dot] tld>\"
url = \"www.example.org\"

################################################################################
## Folders.
srcdir = src
docsdir = doc
testdir = check

################################################################################
## Variables.
appname = `echo ${cmdname} | awk '{printf toupper (substr($$0,1,1)) tolower (substr($$0,2)) \"\n\"}'`
manname = `echo ${cmdname} | awk '{print toupper ($$0)}'`
manpage = ${cmdname}.${mansection}

################################################################################
## Flags.
CPPFLAGS += -DAPPNAME=${appname} -DVERSION=${version} -DAUTHOR=${author} -DYEAR=${year}
CPPFLAGS += -D_POSIX_C_SOURCE=200809L
CPPFLAGS += -DHAVE_INLINE

################################################################################
## USER SETTINGS

## Optional compilation flags.
CFLAGS ?= -pedantic -std=c99 \
 -Wall -Wextra \
 -Wshadow -Wfloat-equal -Wpointer-arith -Winline -Wcast-qual -Wcast-align -Wconversion -Wdouble-promotion -Wfloat-equal

CXXFLAGS ?= -Woverloaded-virtual -Weffc++ -Wold-style-cast

## END OF USER SETTINGS
" > \n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dist rules

;; They are somewhat obsolete since VCS can go better. Here follows some
;; examples for historical interest.

; arcdir = ${ROOT}/${cmdname}-${version}
; arc = ${ROOT}/${arcdir}.tar.xz
; srcfiles = ${srcdir}/Makefile ${srcdir}/${cmdname}.c ${srcdir}/${cmdname}.h
; docsfiles = ${docsdir}/Makefile ${docsdir}/${cmdname}.1.in
; testfiles = ${testdir}/check.c ${testdir}/Makefile
; rootfiles = Makefile TODO
; .PHONY: dist
; dist: ${arc}
; ${arc}: ${rootfiles} ${srcfiles} ${docsfiles} ${testfiles}
; 	-mkdir ${arcdir}/
; 	ln -f ${rootfiles} ${arcdir}/
; 	-mkdir ${arcdir}/${srcdir}
; 	ln -f ${srcfiles} ${arcdir}/${srcdir}/
; 	-mkdir ${arcdir}/${docsdir}
; 	ln -f ${docsfiles} ${arcdir}/${docsdir}/
; 	-mkdir ${arcdir}/${testdir}
; 	ln -f ${testfiles} ${arcdir}/${testdir}
; 	tar -cahf ${arc} ${cmdname}-${version}
; 	rm -rf ${cmdname}-${version}

;; This one is using a list of files generated by an external program (here
;; git). The good is that it is more dynamic. The bad is that it does not depend
;; on any file.

; arc = ${ROOT}/${cmdname}-${version}.tar.xz
; .PHONY: dist
; dist: ${arc}
; ${arc}:
; 	git ls-files -z | { pwd="$$(pwd)" ; sed -z "s/^/$${pwd##*/}\//" ; } | tar caf ${arc} -C .. --null -T -

(provide 'mode-makefile)

;; End of file.
