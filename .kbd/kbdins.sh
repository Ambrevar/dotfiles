#!/bin/sh

## Install all map files in .kbd folder

if [ $(id -u) -ne 0 ]; then
	echo "You must be root to run this script."
	exit
fi

_install ()
{
	FULLPATH="$(realpath "$1")"
	NAME="${FULLPATH##*/}"

	BUF="${FULLPATH%/*}"
	CAT="${BUF##*/}"
	BUF="${BUF%/*}"
	CAT="${BUF##*/}/$CAT"

	umask 022
	echo "Installing [$FULLPATH] to [/usr/share/kbd/keymaps/$CAT/$NAME.gz]"
	gzip -c "$FULLPATH" > "/usr/share/kbd/keymaps/$CAT/$NAME.gz"
}

SOURCE_PATH="$(realpath "$0")"
SOURCE_PATH="${SOURCE_PATH%/*}"
while IFS= read -r i; do
	_install "$i"
done<<EOF
$(find "$SOURCE_PATH" -iname '*.map' -type f)
EOF
