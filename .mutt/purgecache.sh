#!/bin/sh

## Purge old messages in Mutt's message cache.
## "$1" is cache location.
## This is intended to be run at startup, but can be run manually anytime.
## It should not output anything to stdout.

## In KB.
CACHE_LIMIT=51200

cd "$1" 2>/dev/null
[ $? -ne 0 ] && exit

[ $(du -s . | cut -f1 -d'	') -lt $CACHE_LIMIT ] && exit
while IFS= read -r i; do
	rm "$i"
	[ $(du -s . | cut -f1 -d'	') -lt $CACHE_LIMIT ] && exit
done <<EOF
$(ls -rt "$(find . -type f)")
EOF
