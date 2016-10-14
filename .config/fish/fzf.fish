fzf_key_bindings
bind \cT transpose-chars
bind \e\ct fzf-file-widget
bind \ec capitalize-word
bind \eC fzf-cd-widget

function __fzf-select -d 'fzf commandline and print selection back to commandline. Awesome!'
	set -l cmd (commandline)
	[ $cmd ]; or return
	eval $cmd | eval (__fzfcmd) -m --no-cycle --tac --tiebreak=index --toggle-sort=ctrl-r | string join ' ' | read -l result
	[ "$result" ]; and commandline -- $result
	commandline -f repaint
end
bind \e\cm __fzf-select

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
	set -l cwd (eval "echo $cwd_esc")
	if [ ! -d $cwd ]
		set cwd .
	end

  set -q FZF_CTRL_T_COMMAND; or set -l FZF_CTRL_T_COMMAND "
  command find -L \$cwd \\( -path '*/\\.*' -o -fstype 'dev' -o -fstype 'proc' \\) -prune \
  -o -type f -print \
  -o -type d -print \
  -o -type l -print 2> /dev/null | sed 1d"

  if eval $FZF_CTRL_T_COMMAND | eval (__fzfcmd) -m $FZF_CTRL_T_OPTS > $TMPDIR/fzf.result
		if [ $cwd != . ]
			## Remove path from commandline: it will be
			set -l cmd (commandline)
			set -l len (math (string length $cmd) - (string length $cwd_esc))
			commandline -- (string sub -l $len (commandline))
		end
		for i in (seq 20); commandline -i (cat $TMPDIR/fzf.result | __fzf_escape) 2> /dev/null; and break; sleep 0.1; end
	end
  commandline -f repaint
  rm -f $TMPDIR/fzf.result
end

function fzf-bcd-widget -d 'cd backwards'
	pwd | nawk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | eval (__fzfcmd) +m --tac | read -l result
	[ $result ]; and cd $result
	commandline -f repaint
end
bind \e\cL fzf-bcd-widget

function fzf-cdhist-widget -d 'cd to one of the previously visited location'
	string join \n $dirprev | eval (__fzfcmd) +m | read -l result
	[ $result ]; and cd $result
	commandline -f repaint
end
bind \er fzf-cdhist-widget
