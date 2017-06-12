function fish_prompt
	set HOST (hostname)
	printf "%s%s%s%s%s" (set_color -o)'(' \
	(set_color normal) \
	(set_color $fish_color_user)$USER \
	(set_color normal) \
	(set_color -o)@ \
	(set_color normal) \
	(set_color $fish_color_hostname)$HOST \
	(set_color normal) \
	(set_color -o)')'

	## Path
	set -l cwd_color $fish_color_cwd
	if test (id -u) -eq 0
		set cwd_color $fish_color_cwd_root
	end
	set PROMPT_PWD (prompt_pwd)
	printf "%s%s%s%s" \
	(set_color -o)'[' \
	(set_color -o $cwd_color)$PROMPT_PWD \
	(set_color normal) \
	(set_color -o)']'

	echo
	echo '> '
end
