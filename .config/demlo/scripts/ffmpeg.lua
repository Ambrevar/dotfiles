-- Force tagging with FFmpeg: this is a workaround until taglib gains arbitrary
-- tag support.

output.parameters = output.parameters or {}
local flag = false
for k, v in ipairs(output.parameters) do
	if v == "-c:a" then
		flag = true
		break
	end
end

if not flag then
	output.parameters[#output.parameters+1] = "-c:a"
	output.parameters[#output.parameters+1] = "copy"
end
output.parameters[#output.parameters+1] = "-vn"
