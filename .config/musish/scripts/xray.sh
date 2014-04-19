#!/bin/sh

## X-Ray Dog.
artist="X-Ray Dog"
## Albums actually have years, but is it relevent since they have a code?
date=""
## Genre could be Soundtrack, or Trailer Music. We choose "" for now.
genre=""

## Extract album code, assuming parent folder is "XRCD## - $album".
XRCD="${FILENAME%/*}"
XRCD="${XRCD##*/}"
XRCD="${XRCD%% - *}"

## Append the album code to the constants array, other it will be capitalized.
constants="$constants $XRCD "

if [ -z "${album%%XRCD *}" ]; then
    ## Change "XRCD - ## - ..." -> "XRCD## - "
    album="${album#XRCD}"
    album="${album##*[0-9] }"
    album="${album#-}"
    album="$XRCD - $album"
elif [ -n "${album%%XRCD*}" ]; then
    album="$XRCD - $album"
fi

if [ -z "${title}" ]; then
    title="${FILENAME##*/}"
    title="${title%.*}"
    # title="$(echo "$title" | sed 's/X-Ray Dog//')"
    title="$(echo "$title" | sed 's/X-Ray Dog\( \?- \?\)\?//')"
fi
if [ -z "${track}" ]; then
    track="${title% *}"
    title="${title#* }"
fi

subscript="$(findscript vorbis)"
if [ ! -f "$subscript" ]; then
    _error "Subscript '$subscript' not found."
else
    . "$subscript"
fi
