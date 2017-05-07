set -l listinstalled "(pacman -Q | string replace ' ' \t)"
set -l listall "(__fish_print_packages)"

function pacfiles -d 'List of files in pacman package packages sorted by size'
	pacman -Qlq $argv | grep -v '/$' | xargs du -cbh | sort -h
end
complete -c pacfiles -a "$listinstalled"

function paced -d 'Edit files from packages'
	set -l result
	pacman -Qlq $argv | grep -v '/$' | eval (__fzfcmd) -m --tiebreak=index --toggle-sort=ctrl-r | while read -l r; set result $result $r; end
	[ "$result" ]; and eval $EDITOR $result
end
complete -c pel -a "$listinstalled"

function paclog -a size -d 'Pacman last installed packages'
	[ $size ]; or set size 30
	expac -t '%F %T' '%-8l %n' | sort -rn | head -$size
end
