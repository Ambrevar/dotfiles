#!/bin/bash

PWDSFILE="${HOME}/.pwds.gpg"
MUTTBIN="/usr/bin/mutt"

if [ ! -f "$PWDSFILE" ];then
    echo "Password file ${PWDSFILE} not found!"
    exit
fi

if [ ! -f "${MUTTBIN}" ];then
    echo "Mutt is not installed."
    exit
fi

PWDS=$(gpg --decrypt "${PWDSFILE}")
eval "$PWDS"
exec mutt "$@"
