#!/bin/sh

## Force UTF-8, otherwise most browsers will display ASCII.

umask 077
root="/tmp/mutt-$(id -u)"
mkdir -p "$root"
file="$root"/mail.html
echo '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>' > "$file"
cat >> "$file"
$BROWSER "$file"
