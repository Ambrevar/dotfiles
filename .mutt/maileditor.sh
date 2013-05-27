#!/bin/sh

## Seems like $EDITOR is internal to Mutt, whereas variable set between
## backquotes are external. By default Mutt will use env EDITOR variable, so we
## do not really need to set it.
# set editor=`echo \$EDITOR`
## If graphical emacs is used, we need to use a non-terminating client.

if [ "$EDITOR" = "em" ]; then
    cat <<EOF
set editor="emc"
EOF
fi
