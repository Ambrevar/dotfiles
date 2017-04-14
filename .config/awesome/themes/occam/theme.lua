-- Occam theme for Awesome

local path = os.getenv ("HOME") .. "/.config/awesome/themes/occam/"
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

theme = {}

theme.font          = "sans 8"

theme.bg_normal     = "#222222"
theme.bg_focus      = "#444444"
theme.bg_urgent     = "#0088CC"
theme.bg_minimize   = "#535d6c"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#dddddd"
theme.fg_focus      = theme.fg_normal
theme.fg_urgent     = theme.fg_normal
theme.fg_minimize   = theme.fg_normal

theme.border_width  = dpi(2)
theme.border_normal = theme.bg_focus
theme.border_focus  = "#808080"
theme.border_marked = theme.border_normal

theme.wallpaper = path .. "occam-wallpaper.png"

--[[ There are other variable sets overriding the default one when defined, the
sets are:

taglist_[bg|fg]_[focus|urgent|occupied|empty]
tasklist_[bg|fg]_[focus|urgent]
titlebar_[bg|fg]_[normal|focus]
tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
mouse_finder_[color|timeout|animate_timeout|radius|factor]
--]]

theme.taglist_bg_occupied = theme.bg_focus
theme.taglist_bg_focus = theme.bg_urgent

-- Display nothing the taglist squares. We do this since if they are not set,
-- `theme.taglist_bg_occupied` gets ignored. Might be a bug.

theme.taglist_squares_sel   = path .. "void.png"
theme.taglist_squares_unsel = theme.taglist_squares_sel

return theme
