#!/bin/sh

file="${0%/*}/mail.html"
## Assume utf-8, otherwise most browsers will display ascii.
echo '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>' > "$file"
cat >> "$file"
$BROWSER "$file"
rm "$file"
