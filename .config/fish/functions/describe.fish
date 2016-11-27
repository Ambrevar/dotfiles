function describe -d 'Extended "type"'
	for i in $argv
		set typ (type -t $i ^ /dev/null)
		if test $status -ne 0
			set -l path $i
			ls -l $path
			file $path | cut -d':' -f2 | cut -b 2-
			type -pq pacman; and pacman -Qo $path ^ /dev/null
			continue
		end
		switch $typ
			case file
				set -l path (type -p $i)
				ls -l $path
				file $path | cut -d':' -f2 | cut -b 2-
				type -pq pacman; and pacman -Qo $path ^ /dev/null
			case '*'
				type $i
		end
	end
end
