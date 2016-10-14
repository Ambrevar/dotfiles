function fish_user_key_bindings
	## fzf
	if type -pq fzf
		source $fish_config_path/fzf.fish
	end

	## Ranger
	type -q ranger; and bind \e\cd ranger
end

