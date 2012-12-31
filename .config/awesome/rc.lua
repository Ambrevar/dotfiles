--------------------------------------------------------------------------------
-- Awesome configuration
-- Date 2012-10-28
--------------------------------------------------------------------------------

-- Get OS. Take care to read one line only, skipping end of line.
local f = io.popen("uname")
local ostype = f:read("*l")
f:close()

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
-- local menubar = require("menubar")

-- Widget library
--naughty.notify({ preset = naughty.config.presets.critical,
--                 title = "aze",
--                 text = err })
                
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
-- beautiful.init("/usr/share/awesome/themes/zenburn/theme.lua")
beautiful.init(awful.util.getdir("config") .. "/themes/default/theme.lua")

-- {{{ Wallpaper
-- if beautiful.wallpaper then
--     for s = 1, screen.count() do
--         gears.wallpaper.maximized(beautiful.wallpaper, s, true)
--     end
-- end
-- }}}

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =   {
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.floating
}

-- Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   tags[s] = awful.tag({ " 1 ", " 2 "," 3 "," 4 ", " 5 ⚒ ", " 6 ♫ ", "7 ✉ " }, s, layouts[1])
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
batwidget = wibox.widget.textbox()

if ostype == "Linux" then

   -- CPU
   vicious.register(cpuwidget, vicious.widgets.cpu, separator .. '<span color="#CC8F52">CPU $1%</span>')

   -- Net
   local networks = { "eth0", "wlan0" }
   vicious.register(netwidget, vicious.widgets.net, 
                    function (widget, args)
                       for _,device in pairs(networks) do
                          value = tonumber(args["{".. device .." carrier}"])
                          if value ~= nil and value > 0 then
                             return separator .. '<span color="#CC9393">↓' .. args["{" .. device .. " down_kb}"] .. '</span> <span color="#7F9F7F">↑' .. args["{" .. device .. " up_kb}"] .. '</span>'
                          end
                       end
                    end, 3)

   -- Volume
   vicious.register(volmwidget, vicious.widgets.volume, separator .. "Master $1% $2 ", 1, "Master")
   -- PCM may not be available all the time on every machine.  If PCM is toggled
   -- after awesome has been started, you'll need to reload the configuration.
   -- If 'amixer' is not installed, status will never display.
   local volpf = io.popen("amixer | grep PCM 2>/dev/null")
   local volpl = volpf:read("*a")
   volpf:close()
   if volpl ~= "" then
      volpwidget = wibox.widget.textbox()
      vicious.register(volpwidget, vicious.widgets.volume, "PCM $1%", 1, "PCM")
   end
 
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
mytaglist.buttons = awful.util.table.join(
   awful.button({ }, 1, awful.tag.viewonly),
   awful.button({ modkey }, 1, awful.client.movetotag),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, awful.client.toggletag),
   awful.button({ }, 4, awful.tag.viewnext),
   awful.button({ }, 5, awful.tag.viewprev)
                                         )

mytasklist = {}
mytasklist.buttons = awful.util.table.join(
   awful.button({ }, 1,
                function (c)
                   if c == client.focus then
                      c.minimized = true
                   else
                      if not c:isvisible() then
                         awful.tag.viewonly(c:tags()[1])
                      end
                      -- This will also un-minimize
                      -- the client, if needed
                      client.focus = c
                      c:raise()
                   end
                end),

   awful.button({ }, 3,
                function ()
                   if instance then
                      instance:hide()
                      instance = nil
                   else
                      instance = awful.menu.clients({ width=250 })
                   end
                end),

   awful.button({ }, 4,
                function ()
                   awful.client.focus.byidx(1)
                   if client.focus then client.focus:raise() end
                end),

   awful.button({ }, 5,
                function ()
                   awful.client.focus.byidx(-1)
                   if client.focus then client.focus:raise() end
                end))

for s = 1, screen.count() do
   -- Create a promptbox for each screen
   mypromptbox[s] = awful.widget.prompt()

   -- Create an imagebox widget which will contains an icon indicating which layout we're using.
   -- We need one layoutbox per screen.
   mylayoutbox[s] = awful.widget.layoutbox(s)
   mylayoutbox[s]:buttons(awful.util.table.join(
                             awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                             awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                             awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
   -- Create a taglist widget
   mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

   -- Create a tasklist widget
   mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylayoutbox[s])
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
-- CUSTOM
--------------------------------------------------------------------------------

-- Mouse control
-- set the desired pixel coordinates:
local safeCoords = {x=0, y=0}
-- Flag to tell Awesome whether to do this at startup.
local moveMouseOnStartup = true

-- Simple function to move the mouse to the coordinates set above.
local function moveMouseAway(x_co, y_co)
   mouse.coords({ x=x_co, y=y_co })
end

local function moveMouse(x_co, y_co)
   t=mouse.coords()
   mouse.coords( {x = t.x + x_co, y = t.y + y_co} )
end

-- Optionally move the mouse when rc.lua is read (startup)
if moveMouseOnStartup then
   moveMouseAway(safeCoords.x, safeCoords.y)
end

--------------------------------------------------------------------------------
-- Key bindings
-- Note that some laptop will not work when pressing Super+Fn.
-- Therefore we only use Fn and Mod1+Fn.
--------------------------------------------------------------------------------
term = "urxvt"
termcmd = term .. " -e "

globalkeys = awful.util.table.join(
   -- Terminal
   awful.key({ modkey,  }, "Return", function () awful.util.spawn(term) end),
   awful.key({ }, "XF86Terminal",    function () awful.util.spawn(term) end),

   -- Calc
   awful.key({ modkey,  }, "b",     function () awful.util.spawn(termcmd .. "calc") end),
   awful.key({ }, "XF86Calculator", function () awful.util.spawn(termcmd .. "calc") end),

   -- File browser
   awful.key({ modkey,  }, "e",     function () awful.util.spawn(termcmd .. "ranger") end),
   awful.key({ }, "XF86Explorer",   function () awful.util.spawn(termcmd .. "ranger") end),
   awful.key({ }, "XF86MyComputer", function () awful.util.spawn(termcmd .. "ranger") end),

   -- Screen lock
   awful.key({ modkey,  }, "s",      function () awful.util.spawn("slock") end),
   awful.key({ }, "XF86ScreenSaver", function () awful.util.spawn("slock") end),
   awful.key({ }, "XF86Sleep",       function () awful.util.spawn("slock") end),
   awful.key({ }, "XF86Standby",     function () awful.util.spawn("slock") end),

   -- PDF Reader
   awful.key({ modkey,  }, "p", function () awful.util.spawn("zathura") end),

   -- Mail user agent
   awful.key({ modkey,  }, "m", function () awful.util.spawn(termcmd .. "mutt") end),
   awful.key({ }, "XF86Mail",   function () awful.util.spawn(termcmd .. "mutt") end),

   -- Web browser
   awful.key({ modkey, }, "w",       function () awful.util.spawn_with_shell("dwb 2>>/tmp/dwb.log") end),
   awful.key({         }, "XF86WWW", function () awful.util.spawn_with_shell("dwb 2>>/tmp/dwb.log") end),

   -- Music player
   awful.key({ modkey,           }, "a", function () awful.util.spawn(termcmd .. "cmus") end),
   awful.key({ modkey, "Mod1"    }, "a", function () awful.util.spawn("cmus-remote -u") end),
   awful.key({ modkey, "Shift"   }, "a", function () awful.util.spawn("cmus-remote -n") end),
   awful.key({ modkey, "Control" }, "a", function () awful.util.spawn("cmus-remote -r") end),

   awful.key({ }, "XF86AudioMedia", function () awful.util.spawn(termcmd .. "cmus") end),
   awful.key({ }, "XF86AudioPlay",  function () awful.util.spawn("cmus-remote -u") end),
   awful.key({ }, "XF86AudioNext",  function () awful.util.spawn("cmus-remote -n") end),
   awful.key({ }, "XF86AudioPrev",  function () awful.util.spawn("cmus-remote -r") end),

   -- Screenshot
   awful.key({}, "Print", function () awful.util.spawn("scrot 'screen-%Y-%m-%d-%H%M%S.png' -e 'mkdir -p ~/temp && mv $f ~/temp/'") end),

   -- Touchpad
   awful.key({ }, "XF86TouchpadToggle", function () os.execute("synclient TouchpadOff=`synclient -l | grep -c 'TouchpadOff.*=.*0'`") end),
   awful.key({ }, "XF86Tools",          function () os.execute("synclient TouchpadOff=`synclient -l | grep -c 'TouchpadOff.*=.*0'`") end),

   -- Mouse control
   -- Bind ''Meta4+Ctrl+m'' to move the mouse to the coordinates set above.
   -- This is useful if you needed the mouse for something and now want it out of the way.
   awful.key({ modkey, "Control" }, "m", function() moveMouseAway(safeCoords.x, safeCoords.y) end),
   -- awful.key({ modkey, "Control" }, "h", function() moveMouse(-5, 0) end),
   -- awful.key({ modkey, "Control" }, "j", function() moveMouse(0, 5) end),
   -- awful.key({ modkey, "Control" }, "k", function() moveMouse(0, -5) end),
   -- awful.key({ modkey, "Control" }, "l", function() moveMouse(5, 0) end),

   --------------------------------------------------------------------------------
   -- Awesome specific
   --------------------------------------------------------------------------------

   -- Standard program
   awful.key({ modkey, "Control" }, "r", awesome.restart),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),

   -- Tags
   awful.key({ modkey }, "Prior",  awful.tag.viewprev       ),
   awful.key({ modkey }, "Next",   awful.tag.viewnext       ),
   awful.key({ modkey }, "Escape", awful.tag.history.restore),

   -- Layout select and switch
   awful.key({ modkey }, "Up", function () awful.client.swap.byidx(  1)    end),
   awful.key({ modkey }, "Down", function () awful.client.swap.byidx( -1)    end),
   -- awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey }, "Tab",
             function ()
                awful.client.focus.history.previous()
                if client.focus then
                   client.focus:raise()
                end
             end),

   awful.key({ modkey }, "Right",
             function ()
                awful.client.focus.byidx( 1)
                if client.focus then client.focus:raise() end
             end),
   awful.key({ modkey }, "Left",
             function ()
                awful.client.focus.byidx(-1)
                if client.focus then client.focus:raise() end
             end),

   -- Layout resize
   awful.key({ modkey, "Mod1" }, "Left",    function () awful.tag.incmwfact(-0.05)   end),
   awful.key({ modkey, "Mod1" }, "Right",  function () awful.tag.incmwfact(0.05)   end),
   awful.key({ modkey, "Mod1" }, "Up",  function () awful.client.incwfact(0.05) end),
   awful.key({ modkey, "Mod1" }, "Down", function () awful.client.incwfact(-0.05) end),

   -- Floating clients resize
   awful.key({ modkey , "Mod1"}, "j", function () awful.client.moveresize( 20,  20, -40, -40) end),
   awful.key({ modkey , "Mod1"}, "k", function () awful.client.moveresize(-20, -20,  40,  40) end),

   -- Layout organization
   awful.key({ modkey, "Control" }, "Left", function () awful.tag.incnmaster( 1) end),
   awful.key({ modkey, "Control" }, "Right",  function () awful.tag.incnmaster(-1) end),
   awful.key({ modkey, "Control" }, "Up",    function () awful.tag.incncol( 1)    end),
   awful.key({ modkey, "Control" }, "Down",  function () awful.tag.incncol(-1)    end),

   awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
   awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

   -- Multi screen
   awful.key({ modkey, "Mod1" }, "Next", function () awful.screen.focus_relative( 1) end),
   awful.key({ modkey, "Mod1" }, "Prior", function () awful.screen.focus_relative(-1) end),


   -- Prompt
   awful.key({ modkey }, "r", function () mypromptbox[mouse.screen]:run() end),

   -- Lua code
   awful.key({ modkey }, "x",
             function ()
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
      awful.key({ modkey, "Mod1" }, "KP_Subtract",   function () awful.util.spawn("amixer set PCM 5%- >/dev/null") end),
      awful.key({ modkey, "Mod1" }, "KP_Add",        function () awful.util.spawn("amixer set PCM 5%+ >/dev/null") end),

      awful.key({        }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set Master 5%+ >/dev/null") end),
      awful.key({        }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set Master 5%- >/dev/null") end),
      awful.key({        }, "XF86AudioMute",        function () awful.util.spawn("amixer set Master toggle >/dev/null") end),
      awful.key({ "Mod1" }, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer set PCM 5%+ >/dev/null") end),
      awful.key({ "Mod1" }, "XF86AudioLowerVolume", function () awful.util.spawn("amixer set PCM 5%- >/dev/null") end)
                                     )
elseif ostype == "FreeBSD" then
   globalkeys = awful.util.table.join (globalkeys,
      awful.key({ modkey,        }, "KP_Subtract",   function () awful.util.spawn("mixer vol -5 >/dev/null") end),
      awful.key({ modkey,        }, "KP_Add",        function () awful.util.spawn("mixer vol +5 >/dev/null") end),
      awful.key({ modkey,        }, "KP_Enter",      function () awful.util.spawn("mixer vol ^ >/dev/null")  end),
      awful.key({ modkey, "Mod1" }, "KP_Subtract",   function () awful.util.spawn("mixer pcm -5 >/dev/null") end),
      awful.key({ modkey, "Mod1" }, "KP_Add",        function () awful.util.spawn("mixer pcm +5 >/dev/null") end),

      awful.key({        }, "XF86AudioRaiseVolume", function () awful.util.spawn("mixer vol -5 >/dev/null") end),
      awful.key({        }, "XF86AudioLowerVolume", function () awful.util.spawn("mixer vol +5 >/dev/null") end),
      awful.key({        }, "XF86AudioMute",        function () awful.util.spawn("mixer vol ^ >/dev/null")  end),
      awful.key({ "Mod1" }, "XF86AudioRaiseVolume", function () awful.util.spawn("mixer pcm -5 >/dev/null") end),
      awful.key({ "Mod1" }, "XF86AudioLowerVolume", function () awful.util.spawn("mixer pcm +5 >/dev/null") end)
                                     )
end


-- Client keys
clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
   -- TODO: redraw does not work since 3.5?
   -- awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),

   
   -- Floating clients move
   awful.key({ modkey }, "j",         function () awful.client.moveresize(  0,  20,   0,   0) end),
   awful.key({ modkey }, "k",         function () awful.client.moveresize(  0, -20,   0,   0) end),
   awful.key({ modkey }, "h",         function () awful.client.moveresize(-20,   0,   0,   0) end),
   awful.key({ modkey }, "l",         function () awful.client.moveresize( 20,   0,   0,   0) end),

   -- awful.key({ modkey,           }, "n",
   --     function (c)
   --         -- The client currently has the input focus, so it cannot be
   --         -- minimized, since minimized clients can't have the focus.
   --         c.minimized = true
   --     end),
   -- awful.key({ modkey, "Control" }, "n", awful.client.restore),

   awful.key({ modkey, "Mod1" }, "m",
             function (c)
                c.maximized_horizontal = not c.maximized_horizontal
                c.maximized_vertical   = not c.maximized_vertical
             end)
                                  )

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
   globalkeys = awful.util.table.join(globalkeys,
                                      awful.key({ modkey }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewonly(tags[screen][i])
                                                   end
                                                end),

                                      awful.key({ modkey, "Control" }, "#" .. i + 9,
                                                function ()
                                                   local screen = mouse.screen
                                                   if tags[screen][i] then
                                                      awful.tag.viewtoggle(tags[screen][i])
                                                   end
                                                end),

                                      awful.key({ modkey, "Mod1" }, "#" .. i + 9,
                                                function ()
                                                   if client.focus and tags[client.focus.screen][i] then
                                                      awful.client.movetotag(tags[client.focus.screen][i])
                                                   end
                                                end)
                                     )
   -- awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
   --           function ()
   --               if client.focus and tags[client.focus.screen][i] then
   --                   awful.client.toggletag(tags[client.focus.screen][i])
   --               end
   --           end)
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
                    focus = true,
                    keys = clientkeys,
                    buttons = clientbuttons } },

   { rule = { class = "MPlayer" },
     properties = { floating = true } },
   { rule = { class = "mplayer2" },
     properties = { floating = true } },

   { rule = { class = "pinentry" },
     properties = { floating = true } },

   { rule = { class = "Gimp" },
     properties = { floating = false } },

   -- Flash workaround. Does not work?
   { rule = { instance = "plugin-container" },
     properties = { floating = true } },

   { rule = { instance = "exe" },
     properties = { floating = true } },

   -- Only works for terminal with WM_COMMAND property?
   -- Does not work with LXTerminal.
   { rule = { name = "cmus"},
     properties = { tag = tags[1][6] } },
   { rule = { name = "mutt"},
     properties = { tag = tags[1][7] } },

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

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local title = awful.titlebar.widget.titlewidget(c)
        title:buttons(awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
                ))

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(title)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
