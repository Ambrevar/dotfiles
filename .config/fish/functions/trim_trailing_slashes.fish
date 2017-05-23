# This is useful for rsync when folders have slashes automatically appended to
# them (e.g. with fzf completion).
function trim_trailing_slashes --description "Trim trailing slashes of all commandline tokens"
	set -l list (commandline -o)
	commandline -r ""
	for i in $list
		set i (string replace -r '/$' '' $i)
		set -l prefix (string sub -s 1 -l 1 -- $i)
		switch $prefix
			case '~'
				commandline -t -- (string sub -s 2 (string escape -n -- $i))
			case '*'
				commandline -t -- (string escape -- $i)
		end
		commandline -i -- ' '
 	end
end
