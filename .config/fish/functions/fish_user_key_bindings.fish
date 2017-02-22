function fish_user_key_bindings
	## fzf
	if type -pq fzf
		source $fish_config_path/fzf.fish
	end

	bind \el downcase-word
	bind \cw trim_trailing_slashes
end
