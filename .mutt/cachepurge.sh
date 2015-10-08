#!/bin/sh

## Purge old message in Mutt's message cache.
## This is intended to be run at startup, but can be run manually anytime.
## It should not output anything to stdout.

## In KB.
CACHE_LIMIT=51200

cd ~/.cache/mutt_msg 2>/dev/null
[ $? -ne 0 ] && exit

[ $(du -s . | cut -f1 -d'	') -lt $CACHE_LIMIT ] && exit
while IFS= read -r i; do
	rm "$i"
	[ $(du -s . | cut -f1 -d'	') -lt $CACHE_LIMIT ] && exit
done <<EOF
$(ls -rt $(find . -type f))
EOF
