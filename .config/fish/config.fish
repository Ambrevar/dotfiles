## fish working paths
set -q XDG_DATA_HOME; or set -l XDG_DATA_HOME $HOME/.local/share
set -q XDG_CONFIG_HOME; or set -l XDG_CONFIG_HOME $HOME/.config
set -q fish_data_path; or set -g fish_data_path $XDG_DATA_HOME/fish
set -q fish_config_path; or set -g fish_config_path $XDG_CONFIG_HOME/fish

## cdhist options
set -g fish_cdhist_path $fish_data_path/fish_cdhist
set -g fish_cdhist_max 128

## Go back to last cdhist folder. Run this before the SHELL_FILEBROWSER hook.
if grep -q . $fish_cdhist_path ^ /dev/null
	set dirprev (cat $fish_cdhist_path)
	set -q dirprev[$fish_cdhist_max]; and set dirprev $dirprev[(math - $fish_cdhist_max)..-1]
	cd $dirprev[(count $dirprev)] ^ /dev/null
end

## Start at a specific location. Useful when switching to a shell from a browser
## for instance.
[ -n "$SHELL_CD" ]; and cd $SHELL_CD; and set -u SHELL_CD

## Misc
set fish_greeting

## Colors
## For custom prompt.
set fish_color_user brcyan
set fish_color_hostname brcyan

## Aliases
function ls --description 'List contents of directory'
	set -l param --color=auto
	if isatty 1
		set param $param --indicator-style=classify
	end
	if [ (uname -o) = "GNU/Linux" ]
		set param $param --group-directories-first
	end
	command ls $param $argv
end
alias l 'ls -1'
alias la 'll -lAh'

alias mkdir 'mkdir -p'
function mkcd -a folder -d 'Make directory and change into it.'
	mkdir -p $folder
	cd $folder
end

alias cal 'cal -m'

if type -pq pacman
	source $fish_config_path/pacman.fish
end
