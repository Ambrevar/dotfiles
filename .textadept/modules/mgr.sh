#!/bin/sh
source="$HOME/.textadept/modules/repos"
root="$HOME/.textadept/modules/"

cd "$root"
while IFS= read -r i; do
	basename=${i##*/}
	vcs=${basename##*.}
	basename=${basename%.*}
	if [ -n "$vcs" ]; then
		if [ -d "$basename/.$vcs" ]; then
			cd "$basename"
			"$vcs" pull
		elif [ ! -d "$basename" ]; then
			"$vcs" clone "$i"
		else
			echo "Warning: Module $basename is present and not under version control" >&2
		fi
	else
		echo "Error: URI $i lacks VCS extension" >&2
	fi
done <"$source"
