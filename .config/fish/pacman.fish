set -l listinstalled "(pacman -Q | string replace ' ' \t)"
set -l listall "(__fish_print_packages)"

complete -c pacfiles -a "$listinstalled"

function pacls -d 'List/open package files with fzf'
	set -l result
	set -lx OPT $FZF_CTRL_T_OPTS
	pacman -Qlq $argv | grep -v '/$' | eval (__fzfcmd) -m --tiebreak=index --toggle-sort=ctrl-r $OPT | string join ' ' | read -l result
	[ "$result" ]; and commandline -- $result
end
complete -c pacls -a "$listinstalled"
