fzf_key_bindings
bind \cT transpose-chars
bind \e\ct fzf-file-widget
bind \ec capitalize-word
bind \eC fzf-cd-widget

function fzf-select -d 'fzf commandline and print unescaped selection back to commandline'
	set -l cmd (commandline -j)
	[ "$cmd" ]; or return
	eval $cmd | eval (__fzfcmd) -m --tiebreak=index --select-1 --exit-0 | string join ' ' | read -l result
	[ "$result" ]; and commandline -j -- $result
	commandline -f repaint
end
bind \e\cm fzf-select

function fzf-complete -d 'fzf completion and print selection back to commandline'
	set -l complist (complete -C(commandline -c))
	set -l result
	string join -- \n $complist | sort | fzf -m --tiebreak=index --select-1 --exit-0 --header '(commandline)' | cut -f1 | while read -l r; set result $result $r; end

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
bind \t fzf-complete
## TODO: 'complete' is not completely finished, keep original version for now.
bind \e\t complete


function fzf-bcd-widget -d 'cd backwards'
	## TODO: (fish upsteam bug) Cannot use eval here.
	# pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | eval (__fzfcmd) +m --select-1 --exit-0 $FZF_BCD_OPTS | read -l result
	pwd | awk -v RS=/ '/\n/ {exit} {p=p $0 "/"; print p}' | tac | fzf +m --select-1 --exit-0 --preview='preview {}' | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \e\cL fzf-bcd-widget

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
	## TODO: (fish upsteam bug) Cannot use eval here.
	# string join \n $dirprev | tac | sed 1d | eval (__fzfcmd) +m $FZF_CDHIST_OPTS | read -l result
	string join \n $dirprev | tac | sed 1d | fzf +m --preview='preview {}' | read -l result
	[ "$result" ]; and cd $result
	commandline -f repaint
end
bind \er fzf-cdhist-widget
