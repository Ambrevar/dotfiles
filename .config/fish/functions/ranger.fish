# Compatible with ranger >= 1.4.2.
#
# To undo the effect of this function, you can type "cd -" to return to the
# original directory.
function ranger -d 'Run ranger and sync folders with shell'
	[ (count $argv) -eq 0 ]; and set argv .
	set -l tempfile (mktemp)
	command ranger --choosedir="$tempfile" $argv
	if grep -q . "$tempfile" ^ /dev/null
		set -l dest (cat -- "$tempfile")
		## TODO: Clear prompt so that shorter path does not print on top of longer
		## path. Could be a multi-line issue. Happens on every repaint, e.g. with
		## fzf widgets as well.
		[ "$dest" != "$PWD" ]; and cd $dest; and commandline -f repaint
	end
	rm -f -- "$tempfile"
end
