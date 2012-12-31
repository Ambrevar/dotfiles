---------------------------
-- Custom awesome theme --
---------------------------

local configdir = awful.util.getdir("config") .. "/themes/custom/"

theme = {}

theme.font          = "sans 8"

theme.bg_normal     = "#222222"
theme.bg_focus      = "#535d6c"
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#aaaaaa"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.border_width  = 1
theme.border_normal = "#000000"
-- theme.border_normal = "#222222"
-- theme.border_focus  = "#BFBFBF"
theme.border_focus  = "#535d6c"
theme.border_marked = "#91231c"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- [taglist|tasklist]_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Display the taglist squares
theme.taglist_squares_sel   = configdir .. "taglist/squarefw.png"
theme.taglist_squares_unsel = configdir .. "taglist/squarew.png"

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = configdir .. "submenu.png"
theme.menu_height = 15
theme.menu_width  = 100

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = configdir .. "titlebar/close_normal.png"
theme.titlebar_close_button_focus  = configdir .. "titlebar/close_focus.png"

theme.titlebar_ontop_button_normal_inactive = configdir .. "titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = configdir .. "titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = configdir .. "titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = configdir .. "titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = configdir .. "titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = configdir .. "titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = configdir .. "titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = configdir .. "titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = configdir .. "titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = configdir .. "titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = configdir .. "titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = configdir .. "titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = configdir .. "titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = configdir .. "titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = configdir .. "titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = configdir .. "titlebar/maximized_focus_active.png"

theme.wallpaper = configdir .. "background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = configdir .. "layouts/fairhw.png"
theme.layout_fairv = configdir .. "layouts/fairvw.png"
theme.layout_floating  = configdir .. "layouts/floatingw.png"
theme.layout_magnifier = configdir .. "layouts/magnifierw.png"
theme.layout_max = configdir .. "layouts/maxw.png"
theme.layout_fullscreen = configdir .. "layouts/fullscreenw.png"
theme.layout_tilebottom = configdir .. "layouts/tilebottomw.png"
theme.layout_tileleft   = configdir .. "layouts/tileleftw.png"
theme.layout_tile = configdir .. "layouts/tilew.png"
theme.layout_tiletop = configdir .. "layouts/tiletopw.png"
theme.layout_spiral  = configdir .. "layouts/spiralw.png"
theme.layout_dwindle = configdir .. "layouts/dwindlew.png"

theme.awesome_icon = "/usr/share/awesome/icons/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons 
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme
-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:encoding=utf-8:textwidth=80
