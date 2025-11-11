--  |   |
--  __| __ \   _ \ __ `__ \   _ \
--  |   | | |  __/ |   |   |  __/
-- \__|_| |_|\___|_|  _|  _|\___|
--

theme = {}

theme.wallpaper = "/home/m/.config/awesome/wall/beach6.jpg"

theme.font          = "BitstromWera Nerd Font:style=Regular 12"
theme.taglist_font  = "BitstromWera Nerd Font 14"

theme.black         = "#000000"
theme.white         = "#FFFFFF"
theme.grey          = "#D3D3D3"
theme.blue          = "#00BFFF"
theme.darkblue      = "#000080"
theme.magenta       = "#FF00FF"
theme.darkmagenta   = "#8B008B"
theme.darkgrey      = "#696969"

theme.bg_normal     = theme.black
theme.bg_focus      = theme.bg_normal
theme.bg_urgent     = theme.bg_normal
theme.bg_minimize   = theme.bg_normal
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = theme.grey
theme.fg_focus      = theme.magenta
theme.fg_urgent     = theme.blue
theme.fg_minimize   = theme.darkgrey

-- cool: border doesnt know names, this gives transparent background!
--theme.border_normal = "LightGray"

theme.border_width  = 1
theme.border_normal = "#444444"
theme.border_focus  = theme.border_normal
theme.border_marked = theme.fg_focus

-- there are other variable sets overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- mouse_finder_[color|timeout|animate_timeout|radius|factor]
-- Example:
-- theme.taglist_bg_focus = "#ff0000"
-- theme.tasklist_bg_focus = "#ff0000"

theme.taglist_bg_empty    = theme.bg_normal
theme.taglist_bg_occupied = theme.bg_normal

theme.taglist_fg_empty    = theme.fg_minimize
theme.taglist_fg_occupied = theme.fg_normal

-- display the taglist squares:
--theme.taglist_squares_sel   = "/usr/share/awesome/themes/default/taglist/squarefw.png"
--theme.taglist_squares_unsel = "/usr/share/awesome/themes/default/taglist/squarew.png"

-- You can add as many variables as you wish and access them by using
-- beautiful.variable in your rc.lua:
--theme.bg_widget = "#cc0000"

-- define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used:
-- theme.icon_theme = nil
theme.icon_theme = "/usr/share/icons/HighContrast"

theme.notification_icon_size = 70

return theme
