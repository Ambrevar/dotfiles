-- Keep result of 'encoding.lua' except flac.

local bitrate = bps or 9999999
local bitrate_max = 192000

if output.format == 'flac' then
	if bitrate > input.bitrate then
		bitrate = input.bitrate
	end
	output.format = 'ogg'
	output.parameters = {'-c:a', 'libvorbis', '-b:a', tostring(math.min(bitrate, bitrate_max))}
end
