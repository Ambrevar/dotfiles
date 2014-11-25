--------------------------------------------------------------------------------
-- Awesome configuration
--------------------------------------------------------------------------------

-- TODO: Update system bar on audio volume update.
-- TODO: Use Shift as a primary modifier.

-- Get OS. Take care to read one line only, skipping end of line.
local f = io.popen("uname")
local ostype = f:read("*l")
f:close()

local userf = io.popen("id -u")
local uid = userf:read("*l")
userf:close()

local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")

if ostype == "Linux" then
	vicious = require("vicious")
end

--------------------------------------------------------------------------------
-- Error handling
--------------------------------------------------------------------------------
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({ preset = naughty.config.presets.critical,
			title = "Oops, there were errors during startup!",
			text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error",
		function (err)
			-- Make sure we don't go into an endless error loop
			if in_error then return end
			in_error = true

			naughty.notify({ preset = naughty.config.presets.critical,
					title = "Oops, an error happened!",
					text = err })
			in_error = false
	end)
end

--------------------------------------------------------------------------------
-- Themes define colours, icons, and wallpapers
--------------------------------------------------------------------------------
-- beautiful.init("/usr/share/awesome/themes/default/theme.lua")
beautiful.init(awful.util.getdir("config") .. "/themes/custom/theme.lua")

-- {{{ Wallpaper
if beautiful.wallpaper then
	for s = 1, screen.count() do
		gears.wallpaper.maximized(beautiful.wallpaper, s, true)
	end
end
-- }}}

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
modkey = "Mod4"

-- Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
	-- Each screen has its own tag table.
	tags[s] = awful.tag({ " 1 ", " 2 "," 3 "," 4 ", " 5 ⚒ ", " 6 ♫ ", "7 ✉ " }, s, awful.layout.suit.tile)
end

--------------------------------------------------------------------------------
-- Wibox
--------------------------------------------------------------------------------

-- Separator
separator = " | "
separator_date = wibox.widget.textbox()
separator_date:set_text(" |")

-- Clock
mytextclock = awful.widget.textclock()

cpuwidget = wibox.widget.textbox()
netwidget = wibox.widget.textbox()
volmwidget = wibox.widget.textbox()
volpwidget = wibox.widget.textbox()
batwidget = wibox.widget.textbox()

if ostype == "Linux" then

	-- CPU
	vicious.register(cpuwidget, vicious.widgets.cpu, separator .. '<span color="#CC8F52">CPU $1%</span>')

	-- Net
	-- CHECK: not sure if args["{".. device .." carrier}"] may have values below 0. What do values of the args table mean?
	-- Note: we must make sure note to take the loopback interface into account.
	local proc = io.popen("ls -1 /sys/class/net | grep -v '^lo$'")
	local ifarray = {}
	for line in proc:lines() do
		table.insert (ifarray, line);
	end
	proc:close()

	vicious.register(netwidget, vicious.widgets.net, function (widget, args)
			for _,device in pairs(ifarray) do
				value = tonumber(args["{".. device .." carrier}"])
				if value ~= nil and value ~= 0 then
					return separator .. '<span color="#CC9393">↓' .. args["{" .. device .. " down_kb}"] .. '</span> <span color="#7F9F7F">↑' .. args["{" .. device .. " up_kb}"] .. '</span>'
				end
			end
			return ""
	end, 3)

	-- Volume
	vicious.register(volmwidget, vicious.widgets.volume, separator .. "$2 $1%", 1, "Master")

	-- Battery
	local batf = io.popen("ls '/sys/class/power_supply' 2>/dev/null")
	local batl = batf:read("*a")
	local batlimit = 10
	if batl ~= "" then
		--{{ Simple version (perf friendly)
		-- vicious.register(batwidget, vicious.widgets.bat, '<span color="#73A9CD">$2%$1$3</span> | ', 60, "BAT0")
		--{{ Complex version (time warning)
		-- This functions changes the status color when batlimit is reached.
		vicious.register(batwidget, vicious.widgets.bat,
			function (widget, args)
				-- We check if time is displayed (otherwise it's 'N/A'), and if minutes are less than limit.
				if args[1] == "-"
					and string.len(args[3]) == 5
					and tonumber(string.sub(args[3],1,2)) == 0
					and tonumber(string.sub(args[3],4,5)) <= batlimit
				then
					return separator .. '<span color="#FF0000">' .. args[2] .. '%' .. args[1] .. args[3] .. '</span>'
				else
					return separator .. '<span color="#73A9CD">' .. args[2] .. '%' .. args[1] .. args[3] .. '</span>'
				end
			end,
			60, "BAT0")
	end
	batf:close()
end

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytasklist = {}

for s = 1, screen.count() do
	-- Create a promptbox for each screen
	mypromptbox[s] = awful.widget.prompt()

	-- Create a taglist widget
	mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

	-- Create a tasklist widget
	mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

	-- Create the wibox
	mywibox[s] = awful.wibox({ position = "top", screen = s })

	-- Widgets that are aligned to the left
	local left_layout = wibox.layout.fixed.horizontal()
	left_layout:add(mytaglist[s])
	left_layout:add(mypromptbox[s])

	-- Widgets that are aligned to the right
	local right_layout = wibox.layout.fixed.horizontal()
	right_layout:add(batwidget)
	right_layout:add(volmwidget)
	right_layout:add(volpwidget)
	right_layout:add(netwidget)
	right_layout:add(cpuwidget)
	right_layout:add(separator_date)
	right_layout:add(mytextclock)
	if s == 1 then right_layout:add(wibox.widget.systray()) end


	-- Now bring it all together (with the tasklist in the middle)
	local layout = wibox.layout.align.horizontal()
	layout:set_left(left_layout)
	layout:set_middle(mytasklist[s])
	layout:set_right(right_layout)

	mywibox[s]:set_widget(layout)
end

--------------------------------------------------------------------------------
-- Mouse
--------------------------------------------------------------------------------

-- Mouse control
local f = io.popen([[ xrandr | awk '/connected/ {getline; while($0 !~ "\\*") getline; print $1; exit}' ]])
local res = f:read("*l")
f:close()

-- Set the desired pixel coordinates.
local corner_coords = {x=res:match('(%d+)x'), y=res:match('x(%d+)')}

-- Simple function to move the mouse to the coordinates set above.
local function move_mouse_away()
	mouse.coords({ x=corner_coords.x, y=corner_coords.y })
end

-- Optionally move the mouse when rc.lua is read (startup)
move_mouse_away()

--------------------------------------------------------------------------------
-- Key bindings
-- Note that some laptop will not work when pressing Super+Fn.
-- Therefore we only use Fn and Mod1+Fn.
--------------------------------------------------------------------------------
termcmd = os.getenv("TERMCMD") or "urxvt"
term = termcmd .. " -e "
home = os.getenv("HOME")

globalkeys = awful.util.table.join(
	-- Terminal
	awful.key({ modkey,  }, "Return", function () awful.util.spawn(termcmd) end),
	awful.key({ }, "XF86Terminal",    function () awful.util.spawn(termcmd) end),

	-- Calc
	awful.key({ }, "XF86Calculator", function () awful.util.spawn(term .. "calc") end),

	-- Editor
	awful.key({ modkey,  }, "e",     function () awful.util.spawn(os.getenv("EDITOR")) end),

	-- Directory browser
	awful.key({ modkey,  }, "d",     function () awful.util.spawn("browser-autostart") end),
	awful.key({ }, "XF86Explorer",   function () awful.util.spawn("browser-autostart") end),
	awful.key({ }, "XF86MyComputer", function () awful.util.spawn("browser-autostart") end),

	-- Screen lock. xlockmore is useful for LDAP login because slock does not work with it.
	awful.key({ modkey,  }, "s",      function () awful.util.spawn_with_shell("xlock 2>/dev/null || slock") end),
	awful.key({ }, "XF86ScreenSaver", function () awful.util.spawn_with_shell("xlock 2>/dev/null || slock") end),
	awful.key({ }, "XF86Sleep",       function () awful.util.spawn_with_shell("xlock 2>/dev/null || slock") end),
	awful.key({ }, "XF86Standby",     function () awful.util.spawn_with_shell("xlock 2>/dev/null || slock") end),

	-- PDF Reader
	awful.key({ modkey,  }, "p", function () awful.util.spawn("zathura") end),

	-- Mail user agent
	awful.key({ modkey,  }, "m", function () awful.util.spawn(term .. "mutt") end),
	awful.key({ }, "XF86Mail",   function () awful.util.spawn(term .. "mutt") end),

	-- Web browser
	awful.key({ modkey, }, "w",       function () awful.util.spawn_with_shell("dwb 2>>/tmp/dwb" .. uid .. ".log") end),
	awful.key({         }, "XF86WWW", function () awful.util.spawn_with_shell("dwb 2>>/tmp/dwb" .. uid .. ".log") end),

	-- Music player
	awful.key({ modkey,           }, "a", function () awful.util.spawn(term .. "cmus") end),
	awful.key({ modkey, "Mod1"    }, "a", function () awful.util.spawn("cmus-remote -u") end),
	awful.key({ modkey, "Shift"   }, "a", function () awful.util.spawn("cmus-remote -n") end),
	awful.key({ modkey, "Control" }, "a", function () awful.util.spawn("cmus-remote -r") end),

	awful.key({ }, "XF86AudioMedia", function () awful.util.spawn(term .. "cmus") end),
	awful.key({ }, "XF86AudioPlay",  function () awful.util.spawn("cmus-remote -u") end),
	awful.key({ }, "XF86AudioNext",  function () awful.util.spawn("cmus-remote -n") end),
	awful.key({ }, "XF86AudioPrev",  function () awful.util.spawn("cmus-remote -r") end),

	-- Screenshot
	-- Using $HOME in command line does not work for scrot.
	awful.key({}, "Print",         function () awful.util.spawn("scrot '" .. home .. "/temp/screen-%F-%T.png'") end),

	-- TODO manager
	awful.key({ modkey }, "t", function () awful.util.spawn("todo") end),

	-- Mouse control
	-- Touchpad
	awful.key({ }, "XF86TouchpadToggle", function () os.execute("synclient TouchpadOff=`synclient -l | grep -c 'TouchpadOff.*=.*0'`") end),
	awful.key({ }, "XF86Tools",          function () os.execute("synclient TouchpadOff=`synclient -l | grep -c 'TouchpadOff.*=.*0'`") end),
	awful.key({ modkey, "Shift" }, "m",  function () os.execute("synclient TouchpadOff=`synclient -l | grep -c 'TouchpadOff.*=.*0'`") end),
	-- `move_mouse_away` is useful if the mouse is getting in your way.
	awful.key({ modkey, "Control" }, "m", move_mouse_away),

	--------------------------------------------------------------------------------
	-- Awesome specific
	--------------------------------------------------------------------------------

	-- Standard program
	awful.key({ modkey, "Control" }, "r", awesome.restart),
	-- Too risky, so we disable it. You can still quit by calling the Lua command manually (modkey+x by default).
	-- awful.key({ modkey, "Shift"   }, "q", awesome.quit),

	-- Tags
	awful.key({ modkey }, "Prior",  awful.tag.viewprev),
	awful.key({ modkey }, "Next",   awful.tag.viewnext),
	awful.key({ modkey }, "Escape", awful.tag.history.restore),

	-- Layout select and switch
	awful.key({ modkey }, "l", function () awful.client.swap.byidx(  1) end),
	awful.key({ modkey }, "h", function () awful.client.swap.byidx( -1) end),
	-- awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
	awful.key({ modkey }, "Tab", function ()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
	end),

	awful.key({ modkey }, "k",
		function ()
			awful.client.focus.byidx( 1)
			if client.focus then client.focus:raise() end
	end),
	awful.key({ modkey }, "j",
		function ()
			awful.client.focus.byidx(-1)
			if client.focus then client.focus:raise() end
	end),

	-- Layout resize
	awful.key({ modkey, "Mod1" }, "h",    function () awful.tag.incmwfact(-0.05)   end),
	awful.key({ modkey, "Mod1" }, "l",  function () awful.tag.incmwfact(0.05)   end),
	awful.key({ modkey, "Mod1" }, "k",  function () awful.client.incwfact(0.05) end),
	awful.key({ modkey, "Mod1" }, "j", function () awful.client.incwfact(-0.05) end),

	-- Layout organization
	awful.key({ modkey, "Control" }, "Left", function () awful.tag.incnmaster( 1) end),
	awful.key({ modkey, "Control" }, "Right",  function () awful.tag.incnmaster(-1) end),
	awful.key({ modkey, "Control" }, "Up",    function () awful.tag.incncol( 1)    end),
	awful.key({ modkey, "Control" }, "Down",  function () awful.tag.incncol(-1)    end),

	-- Multi screen
	awful.key({ modkey, "Control" }, "Next", function () awful.screen.focus_relative( 1) end),
	awful.key({ modkey, "Control" }, "Prior", function () awful.screen.focus_relative(-1) end),


	-- Prompt
	awful.key({ modkey }, "r", function () mypromptbox[mouse.screen]:run() end),

	-- Lua code
	awful.key({ modkey }, "x", function ()
			awful.prompt.run({ prompt = "Run Lua code: " },
				mypromptbox[mouse.screen].widget,
				awful.util.eval, nil,
				awful.util.getdir("cache") .. "/history_eval")
	end)
)

-- Sound Volume
if ostype == "Linux" then
	globalkeys = awful.util.table.join (globalkeys,
		awful.key({ modkey,        }, "KP_Subtract",   function () awful.util.spawn("amixer set Master 5%- >/dev/null") end),
		awful.key({ modkey,        }, "KP_Add",        function () awful.util.spawn("amixer set Master 5%+ >/dev/null") end),
		awful.key({ modkey,        }, "KP_Enter",      function () awful.util.spawn("amixer set Master toggle >/dev/null") end),

		awful.key({        }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set Master 5%+ >/dev/null") end),
		awful.key({        }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set Master 5%- >/dev/null") end),
		awful.key({        }, "XF86AudioMute",        function () awful.util.spawn("amixer set Master toggle >/dev/null") end)
	)
elseif ostype == "FreeBSD" then
	globalkeys = awful.util.table.join (globalkeys,
		awful.key({ modkey,        }, "KP_Subtract",   function () awful.util.spawn("mixer vol -5 >/dev/null") end),
		awful.key({ modkey,        }, "KP_Add",        function () awful.util.spawn("mixer vol +5 >/dev/null") end),
		awful.key({ modkey,        }, "KP_Enter",      function () awful.util.spawn("mixer vol ^ >/dev/null")  end),

		awful.key({        }, "XF86AudioRaiseVolume", function () awful.util.spawn("mixer vol -5 >/dev/null") end),
		awful.key({        }, "XF86AudioLowerVolume", function () awful.util.spawn("mixer vol +5 >/dev/null") end),
		awful.key({        }, "XF86AudioMute",        function () awful.util.spawn("mixer vol ^ >/dev/null")  end)
	)
end


-- Client keys
clientkeys = awful.util.table.join(
	awful.key({ modkey,         }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
	-- awful.key({ modkey, "Shift" }, "f",      function () awful.layout.inc(layouts, 1) end),
	awful.key({ modkey, "Shift" }, "f",      function ()
		if awful.layout.getname() == 'tile' then
			awful.layout.set(awful.layout.suit.max)
		else
			awful.layout.set(awful.layout.suit.tile)
		end
	end),
	awful.key({ modkey, "Shift" }, "c",      function (c) c:kill()                         end),
	awful.key({ modkey, "Shift" }, "space",  awful.client.floating.toggle                     ),
	awful.key({ modkey, "Shift" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
	awful.key({ modkey,         }, "o",      awful.client.movetoscreen                        )
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
	keynumber = math.min(9, math.max(#tags[s], keynumber))
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
	globalkeys = awful.util.table.join(
		globalkeys,

		awful.key({ modkey }, "#" .. i + 9, function ()
				local screen = mouse.screen
				if tags[screen][i] then
					awful.tag.viewonly(tags[screen][i])
				end
		end),

		awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function ()
				local screen = mouse.screen
				if tags[screen][i] then
					awful.tag.viewtoggle(tags[screen][i])
				end
		end),

		awful.key({ modkey, "Shift" }, "#" .. i + 9, function ()
				if client.focus then
					local tag = awful.tag.gettags(client.focus.screen)[i]
					if tag then
						awful.client.movetotag(tag)
					end
				end
		end),

		-- Multi screen
		awful.key({ modkey, "Control" }, "#" .. i + 9, function ()
				if client.focus then
					if tags[client.focus.screen+1] and tags[client.focus.screen+1][i] then
						awful.client.movetotag(tags[client.focus.screen+1][i])
					elseif tags[1][i] then
						awful.client.movetotag(tags[1][i])
					end
				end
		end)
	)
end

clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

awful.rules.rules = {
	-- All clients will match this rule.
	{ rule = { },
		properties = { border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			size_hints_honor = false,
			focus = awful.client.focus.filter,
			-- focus = true,
			keys = clientkeys,
			buttons = clientbuttons } },

	{ rule = { class = "MPlayer" },
		properties = { floating = true } },
	{ rule = { class = "mplayer2" },
		properties = { floating = true } },
	{ rule = { class = "mpv" },
		properties = { floating = true } },
	{ rule = { class = "pinentry" },
		properties = { floating = true } },
	{ rule = { class = "Steam" },
		properties = { floating = true } },
	{ rule = { name = "QEMU" },
		properties = { floating = true } },


	-- Flash workaround. Does not work?
	{ rule = { instance = "plugin-container" },
		properties = { floating = true } },

	{ rule = { instance = "exe" },
		properties = { floating = true } },

	-- Only works for terminal with WM_COMMAND property?
	-- Does not work with LXTerminal.
	{ rule = { name = "cmus" },
		properties = { tag = tags[1][6] } },
	{ rule = { name = "mutt" },
		properties = { tag = tags[1][7] } },

	-- { rule = { class = "Gimp" },
	--   properties = { floating = false } },

	-- Emacs Speedbar. This does not work when Speedbar is first launched,
	-- because its frame is called "emacs...", not speedbar. It only works when
	-- 'speedbar' command is issued thereafter. Use 'C-x z z' after the first
	-- speedbar call to automate this.
	{ rule = { name = "Speedbar 1.0" },
		callback = function( c ) awful.tag.setmwfact(0.15) end },
}

--------------------------------------------------------------------------------
-- Signals
--------------------------------------------------------------------------------

-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
		-- Enable sloppy focus
		c:connect_signal("mouse::enter", function(c)
				if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
					and awful.client.focus.filter(c) then
					client.focus = c
				end
			end)

		if not startup then
			-- Set the windows at the slave,
			-- i.e. put it at the end of others instead of setting it master.
			-- awful.client.setslave(c)

			-- Put windows in a smart way, only if they does not set an initial position.
			if not c.size_hints.user_position and not c.size_hints.program_position then
				awful.placement.no_overlap(c)
				awful.placement.no_offscreen(c)
			end
		end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--------------------------------------------------------------------------------
-- End of Awesome config
--------------------------------------------------------------------------------
