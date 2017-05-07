set -l listinstalled "(pacman -Q | string replace ' ' \t)"
set -l listall "(__fish_print_packages)"

function pacfiles -d 'List of files in pacman package packages sorted by size'
	pacman -Qlq $argv | grep -v '/$' | xargs du -cbh | sort -h
end
complete -c pacfiles -a "$listinstalled"

function pacls -d 'List/open package files with fzf'
	set -l result
	set -lx OPT "--bind=ctrl-j:'execute-multi(rifle {})' --preview='preview {}'"
	pacman -Qlq $argv | grep -v '/$' | eval (__fzfcmd) -m --tiebreak=index --toggle-sort=ctrl-r $OPT | string join ' ' | read -l result
	[ "$result" ]; and commandline -- $result
end
complete -c pacls -a "$listinstalled"

function paclog -a size -d 'Pacman last installed packages'
	[ $size ]; or set size 30
	expac -t '%F %T' '%-8l %n' | sort -rn | head -$size
end
