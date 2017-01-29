-- Force output to OGG.

local OGGMAX = 500000

local bitrate = bps or 9999999
local OGGMAX = 500000

if bitrate > input.bitrate then
	bitrate = input.bitrate
end

output.parameters = {'-c:a', 'libvorbis', '-b:a', tostring(math.min(bitrate, OGGMAX))}

output.format = 'ogg'
