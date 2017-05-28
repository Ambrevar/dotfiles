function fish_user_key_bindings
	fish_vi_key_bindings
	set fish_escape_delay_ms 10

	## Useful for rsync when folders are completed with fzf.
	bind \cw trim_trailing_slashes

	## As of fish 2.5, Alt-f binding is missing in vi-mode.
	bind -M insert \ef forward-word

	bind -M insert \ce edit_command_buffer
	bind -m insert \ce edit_command_buffer

	## fzf
	if type -pq fzf
		source $fish_config_path/fzf.fish
	end

	# bind \el downcase-word # For Emacs bindings only, load after fzf.
end
