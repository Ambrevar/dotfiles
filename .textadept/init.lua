ui.tabs = false
ui.set_theme(not CURSES and 'dark' or 'custom_term')

args.register('-vm', '--view-mode', 0, function()
		-- Make all opened buffers read-only.
		events.connect(events.FILE_OPENED, function()
				buffer.read_only = true
			end)
		-- Hide the menubar.
		ui.menubar = {}
	end, 'View-only mode')

keys.ch = buffer.char_left
keys.cj = buffer.line_down
keys.ck = buffer.line_up
keys.cl = buffer.char_right

-- TODO: Test when file gets more than 10000 lines while editing.
events.connect(events.FILE_OPENED, function()
		local buffer = buffer
		local c = _SCINTILLA.constants
		local width = #(buffer.line_count..'')
		if width > 4 then
			buffer.margin_width_n[0] = 4 + width * buffer:text_width(c.STYLE_LINENUMBER, '9')
		end
	end)

function browse_url ()
	local url
	local web = os.getenv ('BROWSER') or 'xdg-open'
	local sel = buffer:get_sel_text()
	if #sel == 0 then
		url = buffer.filename
	else
		url = sel
	end
	if url then
		local cmd = string.format(OSX and 'open "file://%s"' or web .. ' "%s" &', url)
	end
end

keys.cy = browse_url

-- Sandboxed environment for scritps.
-- See http://lua-users.org/wiki/SandBoxes
local function make_sandbox (extra)
	local sandbox = {
		assert = assert,
		ipairs = ipairs,
		error = error,
		next = next,
		pairs = pairs,
		pcall = pcall,
		select = select,
		tonumber = tonumber,
		tostring = tostring,
		type = type,
		unpack = unpack,
		_VERSION = _VERSION,
		xpcall = xpcall,
		string = {
			byte = string.byte,
			char = string.char,
			find = string.find,
			format = string.format,
			gmatch = string.gmatch,
			gsub = string.gsub,
			len = string.len,
			lower = string.lower,
			match = string.match,
			rep = string.rep,
			reverse = string.reverse,
			sub = string.sub,
			upper = string.upper,
		},
		table = {
			concat = table.concat,
			insert = table.insert,
			maxn = table.maxn,
			remove = table.remove,
			sort = table.sort,
		},
		math = {
			abs = math.abs,
			acos = math.acos,
			asin = math.asin,
			atan = math.atan,
			atan2 = math.atan2,
			ceil = math.ceil,
			cos = math.cos,
			cosh = math.cosh,
			deg = math.deg,
			exp = math.exp,
			floor = math.floor,
			fmod = math.fmod,
			frexp = math.frexp,
			huge = math.huge,
			ldexp = math.ldexp,
			log = math.log,
			log10 = math.log10,
			max = math.max,
			min = math.min,
			modf = math.modf,
			pi = math.pi,
			pow = math.pow,
			rad = math.rad,
			random = math.random,
			sin = math.sin,
			sinh = math.sinh,
			sqrt = math.sqrt,
			tan = math.tan,
			tanh = math.tanh
		},
		os = {
			clock = os.clock,
			difftime = os.difftime,
			time = os.time,
			tmpname = os.tmpname
		}
	}

	if extra then
		for k, v in pairs (extra) do
			sandbox[k] = v
		end
	end
	return sandbox
end

run = function () return false, 'Sandboxed call disabled.' end
if _VERSION == 'Lua 5.1' then
	-- Run code under environment [Lua 5.1]
	run = function (untrusted_code)
		local env = make_sandbox ()
		if not untrusted_code then return nil, 'No code provided' end
		if untrusted_code:byte (1) == 27 then return nil, 'Binary bytecode prohibited' end
		local untrusted_function, message = loadstring (untrusted_code)
		if not untrusted_function then return nil, message end
		setfenv (untrusted_function, env)
		return pcall (untrusted_function)
	end
elseif _VERSION == 'Lua 5.2' then
	-- Run code under environment [Lua 5.2]
	run = function (untrusted_code, env)
		local env = make_sandbox ()
		local untrusted_function, message = load (untrusted_code, nil, 't', env)
		if not untrusted_function then return nil, message end
		return pcall (untrusted_function)
	end
end

-- TODO: This function runs the code in selection in a sandbox, which may be
-- quite limitating. The evaluated code must be a function (i.e. it must return
-- a value).
function eval ()
	local sel = buffer:get_sel_text()
	if #sel ~= 0 then
		local result = {run (sel)}
		if result[1] then
			if result[2] then
				buffer:replace_sel(result[2])
				for i = 3, #result do
					if result[i] then
						buffer:insert(-1, result[i])
					end
				end
			end
		end
	else
		ui.print (result[2])
	end
end

require('textredux').hijack()
