#!/bin/sh

## Arg $3 can be used to set space-separated tags.
printf '%s %s [$%s]\n' "$1" "$2" "$(echo "$3" | sed 's/ \+/,$/g')" >> $HOME/personal/bookmarks/bookmarks
