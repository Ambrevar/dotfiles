fzf_key_bindings
bind \cT transpose-chars
bind \e\ct fzf-file-widget
bind \ec capitalize-word
bind \eC fzf-cd-widget

## TODO: Force --no-cycle or use global?
function __fzf-select -d 'fzf commandline and print unescaped selection back to commandline'
	set -l cmd (commandline)
	[ $cmd ]; or return
	eval $cmd | eval (__fzfcmd) -m --no-cycle --tac --tiebreak=index --toggle-sort=ctrl-r | string join ' ' | read -l result
	[ "$result" ]; and commandline -- $result
	commandline -f repaint
end
bind \e\cm __fzf-select

function __fzf-complete -d 'fzf completion and print selection back to commandline'
	set -l complist (complete -C)
	set -l result
	switch (count $complist)
		case 0
			return
		case 1
			set result (printf '%s' "$complist[1]" | cut -f1)
		case '*'
			string join -- \n $complist | eval (__fzfcmd) -m --no-cycle --tac --tiebreak=index --toggle-sort=ctrl-r | cut -f1 | while read -l r; set result $result $r; end
	end

	if [ -z "$result" ]
		commandline -f repaint
		return
	end

	## Remove last token from commandline.
	set -l token (commandline -t)
	set -l cmd (commandline)
	set -l len (math (string length -- $cmd) - (string length -- $token))
	commandline -- (string sub -l $len -- (commandline))
	## Insert results.
	for r in $result
		## We need to escape the result. We unescape 'r' first in case 'r' to
		## prevent double escaping.
		switch (string sub -s 1 -l 1 -- $token)
			case "'" '"'
				commandline -i -- (string escape -- (eval "printf '%s' '$r'"))
			case '*'
				commandline -i -- (string escape -n -- (eval "printf '%s' '$r'"))
		end
		commandline -i ' '
	end

	commandline -f repaint
end
bind \t __fzf-complete

## DONE: Report missing (commandline) upstream.
## TODO: Report use of 'read'.
function fzf-history-widget
	history | eval (__fzfcmd) +m --tiebreak=index --toggle-sort=ctrl-r $FZF_CTRL_R_OPTS -q '(commandline)' | read -l result
	and commandline -- $result
	commandline -f repaint
end

## Like original but uses last token as root for 'find'.
## If last token is a path, you can use it as $cwd in FZF_CTRL_T_COMMAND to
## restrict search to this path. $cwd will be suppressed from commandline to
## ensure clean output from the search results.
## TODO: Report upstream. Makes '**' obsolete for bash and zsh.
function fzf-file-widget
	set -l cwd_esc (commandline -t)
	## The commandline token might be escaped, we need to unescape it.
	set -l cwd (eval "printf '%s' '$cwd_esc'")
	if [ ! -d "$cwd" ]
		set cwd .
	end

  set -q FZF_CTRL_T_COMMAND; or set -l FZF_CTRL_T_COMMAND "
  command find -L \$cwd \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
  -o -type f -print \
  -o -type d -print \
  -o -type l -print 2> /dev/null | sed 1d"

	set -l result
  eval $FZF_CTRL_T_COMMAND | eval (__fzfcmd) -m $FZF_CTRL_T_OPTS | while read -l r; set result $result $r; end
	if [ "$result" ]
		if [ "$cwd" != . ]
			## Remove last token from commandline.
			set -l cmd (commandline)
			set -l len (math (string length $cmd) - (string length $cwd_esc))
			commandline -- (string sub -l $len (commandline))
		end
		for i in $result
			commandline -i -- (string escape (eval "printf '%s' '$i'"))
			commandline -i -- ' '
		end
	end
	commandline -f repaint
end

function fzf-bcd-widget -d 'cd backwards'
	pwd | nawk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | eval (__fzfcmd) +m --tac | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \e\cL fzf-bcd-widget

function fzf-cdhist-widget -d 'cd to one of the previously visited location'
	string join \n $dirprev | eval (__fzfcmd) +m | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \er fzf-cdhist-widget
