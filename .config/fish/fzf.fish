fzf_key_bindings
bind \cT transpose-chars
bind \e\ct __fzf-file-widget
bind \ec capitalize-word
bind \eC __fzf-cd-widget

function __fzf-select -d 'fzf commandline and print unescaped selection back to commandline'
	set -l cmd (commandline -j)
	[ "$cmd" ]; or return
	eval $cmd | eval (__fzfcmd) -m --tiebreak=index --select-1 --exit-0 | string join ' ' | read -l result
	[ "$result" ]; and commandline -j -- $result
	commandline -f repaint
end
bind \e\cm __fzf-select

function __fzf-complete -d 'fzf completion and print selection back to commandline'
	set -l complist (complete -C(commandline -c))
	set -l result
	string join -- \n $complist | sort | eval (__fzfcmd) -m --tiebreak=index --select-1 --exit-0 --header '(commandline)' | cut -f1 | while read -l r; set result $result $r; end

	for i in (seq (count $result))
		set -l r $result[$i]
		## We need to escape the result. We unescape 'r' first in case 'r' to
		## prevent double escaping.
		switch (string sub -s 1 -l 1 -- (commandline -t))
			case "'"
				commandline -t -- (string escape -- (eval "printf '%s' '$r'"))
			case '"'
				set -l quoted (string escape -- (eval "printf '%s' '$r'"))
				set -l len (string length $quoted)
				commandline -t -- '"'(string sub -s 2 -l (math $len - 2) (string escape -- (eval "printf '%s' '$r'")))'"'
			case '~'
				commandline -t -- (string sub -s 2 (string escape -n -- (eval "printf '%s' '$r'")))
			case '*'
				commandline -t -- (string escape -n -- (eval "printf '%s' '$r'"))
		end
		[ $i -lt (count $result) ]; and commandline -i ' '
	end

	commandline -f repaint
end
bind \t __fzf-complete
## TODO: 'complete' is not completely finished, keep original version for now.
bind \e\t complete

## DONE: Report missing (commandline) upstream.
## TODO: Report use of 'read'.
function __fzf-history-widget
	history | eval (__fzfcmd) +m --tiebreak=index $FZF_CTRL_R_OPTS -q '(commandline)' | read -l result
	and commandline -- $result
	commandline -f repaint
end
bind \cr __fzf-history-widget

## Like original but uses last token as root for 'find'.
## If last token is a path, you can use it as $cwd in FZF_CTRL_T_COMMAND to
## restrict search to this path.
## TODO: Report upstream. Makes '**' obsolete for bash and zsh.
function __fzf-file-widget
	set -l cwd (commandline -t)
	## The commandline token might be escaped, we need to unescape it.
	set cwd (eval "printf '%s' $cwd")
	if [ ! -d "$cwd" ]
		set cwd .
	end

	set -q FZF_CTRL_T_COMMAND; or set -l FZF_CTRL_T_COMMAND "
	command find -L \$cwd \\( -path \$cwd'*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
	-o -type f -print \
	-o -type d -print \
	-o -type l -print 2> /dev/null | sed 1d"

	eval "$FZF_CTRL_T_COMMAND | "(__fzfcmd)" -m $FZF_CTRL_T_OPTS" | while read -l r; set result $result $r; end
	if [ -z "$result" ]
		commandline -f repaint
		return
	end

	if [ "$cwd" != . ]
		## Remove last token from commandline.
		commandline -t ""
	end
	for i in $result
		commandline -it -- (string escape $i)
		commandline -it -- ' '
	end
	commandline -f repaint
end

function __fzf-cd-widget
  set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "
  command find -L . \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
  -o -type d -print 2> /dev/null | sed 1d | cut -b3-"
  eval "$FZF_ALT_C_COMMAND | "(__fzfcmd)" +m --select-1 --exit-0 $FZF_ALT_C_OPTS" | read -l result

	[ "$result" ]; and cd $result
  commandline -f repaint
end

function __fzf-bcd-widget -d 'cd backwards'
	## TODO: (fish upsteam bug) Cannot use eval here.
	# pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | eval (__fzfcmd) +m --select-1 --exit-0 $FZF_BCD_OPTS | read -l result
	pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | fzf +m --select-1 --exit-0 --preview='preview {}' | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \e\cL __fzf-bcd-widget

function __fzf-cdhist-widget -d 'cd to one of the previously visited location'
	## Clear non-existent folders from cdhist.
	set -l buf
	for i in (seq 1 (count $dirprev))
		set -l dir $dirprev[$i]
		if test -d $dir
			set buf $buf $dir
		end
	end
	set dirprev $buf
	## TODO: (fish upsteam bug) Cannot use eval here.
	# string join \n $dirprev | tac | sed 1d | eval (__fzfcmd) +m $FZF_CDHIST_OPTS | read -l result
	string join \n $dirprev | tac | sed 1d | fzf +m --preview='preview {}' | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \er __fzf-cdhist-widget
