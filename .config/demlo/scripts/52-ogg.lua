-- Force output to OGG.

local bitrate = bps or 9999999
local bitrate_max = 192000

if bitrate > input.bitrate then
	bitrate = input.bitrate
end

output.parameters = {'-c:a', 'libvorbis', '-b:a', tostring(math.min(bitrate, bitrate_max))}

output.format = 'ogg'
