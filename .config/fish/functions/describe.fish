function describe -d 'Extend on "type" for executables'
	for i in $argv
		switch (type -t $i)
			case file
				set -l path (type -p $i)
				ls -l $path
				file $path | cut -d':' -f2 | cut -b 2-
				type -pq pacman; and pacman -Qo $path
			case '*'
		type $i
		end
	end
end
