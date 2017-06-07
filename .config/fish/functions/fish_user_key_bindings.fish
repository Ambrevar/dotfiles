function fish_user_key_bindings
	fish_vi_key_bindings
	set fish_escape_delay_ms 10

	## Useful for rsync when folders are completed with fzf.
	bind \cw trim_trailing_slashes

	bind \e\x7f backward-kill-word
	bind -M insert \e\x7f backward-kill-word

	bind -M insert \ce edit_command_buffer
	bind -m insert \ce edit_command_buffer

	bind \cv fish_primary_paste
	bind \cx fish_primary_copy

	## fzf
	if type -pq fzf
		source $fish_config_path/fzf.fish
	end
end
