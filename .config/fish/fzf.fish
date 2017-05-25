fzf_key_bindings

## Emacs bindings
# bind \cT transpose-chars
# bind \e\ct fzf-file-widget
# bind \ec capitalize-word
# bind \eC fzf-cd-widget

bind -M insert \et fzf-file-widget
bind t fzf-file-widget
bind R fzf-history-widget
bind -m insert C fzf-cd-widget

function fzf-select -d 'Eval commandline, fzf result and print out selection'
	set -l cmd (commandline -j)
	[ "$cmd" ]; or return
	eval $cmd | eval (__fzfcmd) -m --select-1 --exit-0 | string join ' ' | read -l result
	[ "$result" ]; and commandline -j -- $result
	commandline -f repaint
end
bind \e\cm fzf-select
bind m fzf-select

## TODO: 'complete' is not completely finished, keep original version for now.
## We need a way to distinguish escape content from unescaped content:
## - https://github.com/fish-shell/fish-shell/issues/1127
## - https://github.com/fish-shell/fish-shell/issues/3469
## Examples:
## - $VAR
## - ~/
## - echo \$HOME-$H<tab>
function fzf-complete -d 'fzf completion and print selection back to commandline'
	set -l complist (complete -C(commandline -c))
	set -l result
	string join -- \n $complist | sort | fzf -m --select-1 --exit-0 --header '(commandline)' | cut -f1 | while read -l r; set result $result $r; end

	set prefix (string sub -s 1 -l 1 -- (commandline -t))
	for i in (seq (count $result))
		set -l r $result[$i]
		switch $prefix
			case "'"
				commandline -t -- (string escape -- $r)
			case '"'
				if string match '*"*' -- $r >/dev/null
					commandline -t --  (string escape -- $r)
				else
					commandline -t -- '"'$r'"'
				end
			case '~'
				commandline -t -- (string sub -s 2 (string escape -n -- $r))
			case '*'
				commandline -t -- (string escape -n -- $r)
		end
		[ $i -lt (count $result) ]; and commandline -i ' '
	end

	commandline -f repaint
end
bind \t fzf-complete
bind \e\t complete
bind -M insert \t fzf-complete
bind -M insert \e\t complete

function fzf-bcd-widget -d 'cd backwards'
	set -lx FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS $FZF_BCD_OPTS"
	pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | eval (__fzfcmd) +m --select-1 --exit-0 | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
# bind \e\cL fzf-bcd-widget
bind -m insert / fzf-bcd-widget

function fzf-cdhist-widget -d 'cd to one of the previously visited locations'
	## Clear non-existent folders from cdhist.
	set -l buf
	for i in (seq 1 (count $dirprev))
		set -l dir $dirprev[$i]
		if test -d $dir
			set buf $buf $dir
		end
	end
	set dirprev $buf
	set -lx FZF_DEFAULT_OPTS "$FZF_DEFAULT_OPTS $FZF_CDHIST_OPTS"
	string join \n $dirprev | tac | sed 1d | eval (__fzfcmd) +m --tiebreak=index --toggle-sort=ctrl-r | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
# bind \er fzf-cdhist-widget
bind -m insert z fzf-cdhist-widget
