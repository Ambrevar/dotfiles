-- Keep result of 'encoding.lua' except flac.

if output.format == 'flac' then
	output.format = 'ogg'
	output.parameters = {'-c:a', 'libvorbis', '-q:a', '10'}
end

