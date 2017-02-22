# This is useful for rsync when folders have slashes automatically appended to
# them (e.g. with fzf completion).
function trim_trailing_slashes --description "Trim trailing slashes of all commandline tokens"
	set -l list (commandline -o)
	commandline -r ""
	for i in $list
		commandline -i -- (string escape (string replace -r '/$' '' $i))
		commandline -i -- ' '
 	end
end
