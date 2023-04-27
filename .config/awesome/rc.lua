local gears = require("gears") -- standard awesome library.
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox") -- widget and layout library.
local beautiful = require("beautiful") -- theme handling library.
local naughty = require("naughty") -- notification library.
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local ror = require("aweror") -- run or raise.
local machi = require("layout-machi") -- git: weird fancy layout thing.
local revelation=require("revelation") -- git: mac exposee mode.

--   _ \  __|  __| _ \   __| 
--   __/ |    |   (   | |    
-- \___|_|   _|  \___/ _|    

-- check if awesome encountered an error during startup and fall back to
-- another config (this code will only ever execute for the fallback config):
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "Error 3.1415: Error Database not found!",
                    text = awesome.startup_errors })
end

-- handle runtime errors after startup:
do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
                             -- make sure we don't go into an endless error loop.
                             if in_error then return end
                             in_error = true
                             naughty.notify({ preset = naughty.config.presets.critical,
                                              title = "Oops, an error happened!",
                                              text = tostring(err) })
                             in_error = false
   end)
end

-- \ \   / _` |  __| __| 
--  \ \ / (   | |  \__ \ 
--   \_/ \__,_|_|  ____/ 

-- themes define colours, icons, font and wallpapers:
beautiful.init(awful.util.getdir("config") .. "/canyon.lua")
beautiful.layout_machi = machi.get_icon()
revelation.init() -- load after beautiful!

-- config systray:
--wibox.widget.systray:set_base_size( 100 )

-- this is used later as the default terminal and editor to run:
terminal = "urxvtc"
editor = os.getenv("EDITOR") or "vi"
editor_cmd = terminal .. " -e " .. editor

-- default modkey:
-- usually, Mod4 is the key with a logo between Control and Alt.
-- if you do not like this or do not have such a key,
-- i suggest you to remap Mod4 to another key using xmodmap or other tools.
-- however, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- table of layouts to cover with awful.layout.inc, order matters:
awful.layout.layouts = {
   awful.layout.suit.tile,
   -- awful.layout.suit.tile.left,
   awful.layout.suit.max,
   awful.layout.suit.tile.bottom,
   -- awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   -- awful.layout.suit.fair.horizontal,
   -- awful.layout.suit.spiral,
   -- awful.layout.suit.spiral.dwindle,
   awful.layout.suit.floating,
   -- awful.layout.suit.max.fullscreen,
   -- awful.layout.suit.magnifier,
   -- awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
   machi.default_layout,
}

--   _|                  |  _)                  
--  |   |   | __ \   __| __| |  _ \  __ \   __| 
--  __| |   | |   | (    |   | (   | |   |\__ \ 
-- _|  \__,_|_|  _|\___|\__|_|\___/ _|  _|____/ 

-- create reminder function:
local function remind_prompt()
    awful.prompt.run {
        prompt       = '<span color="orange">Remind: </span>',
        bg_cursor    = 'orange',
        textbox      = mouse.screen.mypromptbox.widget,
        exe_callback = function(input)
           if not input or #input == 0 then
              naughty.notify{ text = 'usage: set reminder with "remind $time $name" eg "remind 10s asdf"'..input }
              return
           end
           awful.spawn( 'remind '..input ) 
        end
    }
end

local function client_menu_toggle_fn()
   local instance = nil
   return function ()
      if instance and instance.wibox.visible then
         instance:hide()
         instance = nil
      else
         instance = awful.menu.clients({ theme = { width = 250 } })
      end
   end
end

-- menubar configuration:
menubar.utils.terminal = terminal -- set the terminal for applications that require it.

-- keyboard map indicator and switcher:
mykeyboardlayout = awful.widget.keyboardlayout()

--           _) |                
-- \ \  \   / | __ \   _` |  __| 
--  \ \  \ /  | |   | (   | |    
--   \_/\_/  _|_.__/ \__,_|_|    

-- create a textclock widget.
mytextclock = wibox.widget.textclock('<span color="DeepSkyBlue">%_H%M </span>', 5)

-- create a wibox for each screen and add it.
local taglist_buttons = awful.util.table.join(
   awful.button({ }, 1, function(t) t:view_only() end),
   awful.button({ modkey }, 1, function(t)
         if client.focus then
            client.focus:move_to_tag(t)
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function(t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end),
   awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
   awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
)

local tasklist_buttons = awful.util.table.join(
   awful.button({ }, 1, function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- without this, the following
            -- :isvisible() makes no sense:
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- this will also un-minimize
            -- the client, if needed:
            client.focus = c
            c:raise()
         end
   end),
   awful.button({ }, 3, client_menu_toggle_fn()),
   awful.button({ }, 4, function ()
         awful.client.focus.byidx(1)
   end),
   awful.button({ }, 5, function ()
         awful.client.focus.byidx(-1)
end))
                               
--   __|  __|  __| _ \  _ \ __ \  
-- \__ \ (    |    __/  __/ |   | 
-- ____/\___|_|  \___|\___|_|  _|

-- wallpaper:
local function set_wallpaper(s)
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- if wallpaper is a function, call it with the screen:
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- re-set wallpaper when a screen's geometry changes (e.g. different resolution):
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
      -- wallpaper:
      set_wallpaper(s)
      
      -- each screen has its own tag table:
      -- working: ↯ 𝄞♫ ♞♟♤♡♢♧⚛
      awful.tag({ "∅", "⚡", "$", "⛁", "≣", "♬", "♻", "@", "✆","♞", "♠", "♥", "♦", "♣" }, s, awful.layout.layouts[1]) 
      
      -- create a promptbox for each screen:
      s.mypromptbox = awful.widget.prompt()
      -- create an imagebox widget which contains an icon indicating which layout we're using.
      -- We need one layoutbox per screen:
      s.mylayoutbox = awful.widget.layoutbox(s)
      s.mylayoutbox:buttons(awful.util.table.join(
                               awful.button({ }, 1, function () awful.layout.inc( 1) end),
                               awful.button({ }, 3, function () awful.layout.inc(-1) end),
                               awful.button({ }, 4, function () awful.layout.inc( 1) end),
                               awful.button({ }, 5, function () awful.layout.inc(-1) end)))
      
      -- create a tasklist widget:
      s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)
      
      -- create a taglist widget:
      s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)
      
      -- create the wibox:
      s.mywibox = awful.wibar({ position = "top", screen = s, height = 30, opacity = 0.8 })
      
      -- add widgets to the wibox:
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         { -- left widgets:
            layout = wibox.layout.fixed.horizontal,
            --            mylauncher,
            s.mytaglist,
            s.mypromptbox,
         },
         s.mytasklist, -- middle widget:
         { -- right widgets:
            layout = wibox.layout.fixed.horizontal,
            -- systray with margin around (left, right, top, bottom):
            wibox.layout.margin(wibox.widget.systray(), 3, 3, 3, 3), 
            --mykeyboardlayout,
            mytextclock,
            --s.mylayoutbox,
         },
      }
end)

--  __ `__ \   _ \  |   |  __|  _ \ 
--  |   |   | (   | |   |\__ \  __/ 
-- _|  _|  _|\___/ \__,_|____/\___| 

-- mouse bindings:
root.buttons(awful.util.table.join(
                awful.button({ }, 3, function () mymainmenu:toggle() end),
                awful.button({ }, 4, awful.tag.viewnext),
                awful.button({ }, 5, awful.tag.viewprev)
))

--  |                    
--  |  /  _ \ |   |  __| 
--    <   __/ |   |\__ \ 
-- _|\_\\___|\__, |____/ 
--           ____/       

-- key bindings:
globalkeys = awful.util.table.join(
   
   -- awesome:
   awful.key({ modkey, "Shift" }, "q", awesome.restart),
   awful.key({ modkey, "Shift", "Control" }, "q", awesome.quit),

   -- machi:
   awful.key({ modkey }, ".", function () machi.default_editor.start_interactive() end),
   awful.key({ modkey }, "/", function () machi.switcher.start(client.focus) end),

   -- revelation:
   awful.key({ modkey }, "w", revelation),
   awful.key({ modkey, "Shift" }, "w", function() -- only terminals.
         revelation({rule={class="URxvt"}})
   end),

   -- current tag only:
   -- awful.key({ modkey, "Shift", "Control" }, "w", function()
   --       revelation({rule={class="conky"}, is_excluded=true, 
   --                   curr_tag_only=true})
   -- end),
   
   -- not working only terminals:
   -- awful.key({ modkey, "Shift", "Control" }, "w", function() 
   --       revelation({
   --             rule{class={"URxvt", "Xterm"},
   --                   any=true}
   --       })
   -- end),

   -- show everything:
   awful.key({ modkey }, "c",
      function ()
         awful.layout.set(awful.layout.suit.corner.nw)
         for _, c in ipairs(client.get()) do
            if c.maximized then
               c.maximized = not c.maximized
               c:raise()
            end 
         end
   end),
      
   -- menubar:
   awful.key({ modkey }, "p", function() menubar.show() end),
   
   -- navigation:
   awful.key({ modkey }, "Left",  awful.tag.viewprev),
   awful.key({ modkey }, "j",  awful.tag.viewprev),
   awful.key({ modkey }, "Right",  awful.tag.viewnext),
   awful.key({ modkey }, "k",  awful.tag.viewnext),
   awful.key({ modkey }, "Escape", awful.tag.history.restore),
   awful.key({ modkey }, "z", awful.client.urgent.jumpto),
   
   -- layout manipulation:
   awful.key({ modkey }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
   end),
   
   awful.key({ modkey }, "l", function () awful.tag.incmwfact( 0.05) end),
   awful.key({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end),
   awful.key({ modkey }, "space", function () awful.layout.inc( 1) end), -- "space" is lo-caps!
   awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(-1) end), -- "space" is lo-caps!
   --awful.key({ modkey }, "m", awful.layout.suit.max.fullscreen),
   --awful.key({ modkey }, "b", awful.layout.suit.floating),
   
   awful.key({ modkey, "Shift" }, "n",
      function ()
         local c = awful.client.restore()
         -- focus restored client:
         if c then
            client.focus = c
            c:raise()
         end
   end),
   
   awful.key({ modkey, "Shift" }, "f",
      function ()
         myscreen = awful.screen.focused()
         myscreen.mywibox.visible = not myscreen.mywibox.visible
   end),

   -- prompt:
   awful.key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end),
   awful.key({ modkey }, "<", remind_prompt),
  
   -- launch:
   awful.key({ modkey }, "Return", function () awful.spawn(terminal) end),
   awful.key({ modkey, "Shift", "Control" }, "Return", function () awful.spawn("urxvtc -title admin2 -e 2") end),
   awful.key({ modkey, "Shift"}, "Return", function () awful.spawn("urxvtc -title master1 -e 1") end),
   awful.key({ modkey }, "e", function () awful.spawn("emacsclient -ca ''", false) end),
   awful.key({ modkey, "Shift" }, "d", function () awful.spawn("urxvtc -title Waldläufer -e sudo ranger", false) end),
   awful.key({ modkey, "Shift", "Control" }, "d", function () awful.spawn("sudo thunar", false) end),
   awful.key({ modkey, "Shift" }, "s", function () awful.spawn("open_primary_selection_in_cromium") end),
   awful.key({ modkey, "Control" }, "s", function () awful.spawn("open_primary_selection_in_google_translate") end),
   awful.key({ modkey, "Shift", "Control" }, "s", function () awful.spawn("open_primary_selection_in_thesaurus") end),
   awful.key({ modkey, "Shift" }, "t", function () awful.spawn("urxvtc -title Torronator -e rtorrent") end),
   awful.key({ modkey }, "g", function () awful.spawn("urxvtc -title Toppings -e top") end),
   awful.key({ modkey, "Shift" }, "g", function () awful.spawn("urxvtc -title Toppings -e htop") end),
   --awful.key({ modkey }, "o", function () awful.spawn("octave --gui") end),
   awful.key({ modkey }, "x", function () awful.spawn("xterm -T 'VSConsole' -fa 'xft:DejaVuSansMono' -fs 24 -e 'bash'") end),
   awful.key({ modkey, "Shift" }, "x", function () awful.spawn("xterm -T 'VSConsole' -fa 'xft:DejaVuSansMono' -fs 24 -e 'trainee'") end),
   
   -- audio:
   awful.key({ }, "XF86AudioRaiseVolume", function () awful.spawn("amixer set Master 1%+", false) end),
   awful.key({ }, "XF86AudioLowerVolume", function () awful.spawn("amixer set Master 1%-", false) end),
   --awful.key({ }, "XF86AudioMute", function () awful.spawn('for x in {"Master","Headphone","Front","Surround","Center","LFE","Side"} ; do amixer -c 0 set "${x}" toggle; done', false) end),
   awful.key({ }, "XF86AudioMute", function () awful.spawn("amixer set Master toggle", false) end),
   awful.key({ }, "XF86AudioMicMute", function () awful.spawn("amixer set Capture toggle", false) end),
   --awful.key({ }, "XF86Tools", function () awful.spawn(terminal .. " -e ncmpcpp", false) end),
   --awful.key({ }, "XF86Tools", function () awful.spawn("spotify", false) end),
   --awful.key({ }, "XF86AudioPrev", function () awful.spawn("playerctl previous", false) end),
   --awful.key({ }, "XF86AudioPlay", function () awful.spawn("playerctl play-pause", false) end),
   --awful.key({ }, "XF86AudioNext", function () awful.spawn("playerctl next", false) end),

   -- lights:
   awful.key({ }, "XF86MonBrightnessDown", function () awful.spawn("sudo light -U 30", false) end),    
   awful.key({ }, "XF86MonBrightnessUp", function () awful.spawn("sudo light -A 30", false) end),    
   awful.key({ }, "XF86Display", function () awful.spawn("xset dpms force off", false) end),    

   -- screenshot:
   awful.key({ }, "Print", function () awful.spawn("scrot -e 'mv $f ~/.screens/ 2>/dev/null'") end),
   
   -- killer:
   awful.key({ modkey, "Shift" }, "k", function () awful.spawn("sudo xkill", false) end),
   
   -- razer:
   awful.key({ modkey }, "#86", function () awful.spawn("razercfg -l glowinglogo:off -l scrollwheel:on", false) end),
   awful.key({ modkey }, "#82", function () awful.spawn("razercfg -l all:off", false) end),

   -- -- boinc:
   -- awful.key({ modkey }, "b", function () awful.spawn("boinccmd --set_run_mode never 3600", false) end), -- snooze whole boinc for 1h
   -- awful.key({ modkey, "control" }, "b", function () awful.spawn("boinccmd --set_run_mode auto", false) end), -- wake up whole boinc
   -- awful.key({ modkey, "shift" }, "b", function () awful.spawn("boinccmd --set_gpu_mode never 3600", false) end),  -- snooze up boinc gpu
   -- awful.key({ modkey, "shift", "control" }, "b", function () awful.spawn("boinccmd --set_gpu_mode auto", false) end),  -- wake up boinc gpu

   -- -- slurm:
   -- awful.key({ modkey }, "c", function () awful.spawn("schnegg -p", false) end), -- snooze whole (drain node + suspend all jobs) slurm for 1h
   -- awful.key({ modkey, "shift" }, "c", function () awful.spawn("schnegg -r", false) end), -- resume all

   -- rotate:
   awful.key({ modkey, "Shift" }, "r", function () awful.spawn("xrandr -o 1", false) end),
   awful.key({ modkey, "Control", "shift" }, "r", function () awful.spawn("xrandr -o 0", false) end),
 
   -- fun:
   awful.key({ modkey }, "z", function () awful.spawn.with_shell('notify-send "$(cowsay $(fortune))"', false) end)
   --awful.key({ modkey }, "w", function () awful.spawn.with_shell("killall conky && feh --bg-fill ~/wind/canvas/wrld12.png & conky -c ~/wind/wind_blow.lua", false) end),
   --awful.key({ modkey , "Shift" }, "w", function () awful.spawn.with_shell("killall conky && feh --bg-fill ~/.config/awesome/themes/canyon.jpg && conky -c ~/.config/conky/left.lua && conky -c ~/.config/conky/middle.lua", false) end)

)

clientkeys = awful.util.table.join(
   awful.key({ modkey }, "q", function (c) c:kill() end),
   awful.key({ modkey, "Shift" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   awful.key({ modkey }, "b",  awful.client.floating.toggle),
   
   awful.key({ modkey }, "n",
      function (c)
         -- the client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
   end),
   
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
   end),
   
   awful.key({ modkey, "Control" }, "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
   end),
   
   awful.key({ modkey, "Shift"   }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c:raise()
   end),
   
   awful.key({ modkey }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
   end)
)

-- generate and add the 'run or raise' key bindings to the globalkeys table:
globalkeys = awful.util.table.join(globalkeys, ror.genkeys(modkey))

root.keys(globalkeys)

--  |                    
--  __|  _` |  _` |  __| 
--  |   (   | (   |\__ \ 
-- \__|\__,_|\__, |____/ 
--           |___/       

-- bind all key numbers to tags:
-- be careful: we use keycodes to make it works on any keyboard layout.
-- this should map on the top row of your keyboard, usually 1 to 9.
local keys = { "#49", 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, "#20", "#21", "#22" }
for i = 1, 14 do
   globalkeys = awful.util.table.join(globalkeys,
                                      -- view tag only:
                                      awful.key({ modkey }, keys[i],
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               tag:view_only()
                                            end
                                      end),
                                      -- toggle tag display:
                                      awful.key({ modkey, "Control" }, keys[i],
                                         function ()
                                            local screen = awful.screen.focused()
                                            local tag = screen.tags[i]
                                            if tag then
                                               awful.tag.viewtoggle(tag)
                                            end
                                      end),
                                      -- move client to tag.
                                      awful.key({ modkey, "Shift" }, keys[i],
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:move_to_tag(tag)
                                               end
                                            end
                                      end),
                                      -- toggle tag on focused client:
                                      awful.key({ modkey, "Control", "Shift" }, keys[i],
                                         function ()
                                            if client.focus then
                                               local tag = client.focus.screen.tags[i]
                                               if tag then
                                                  client.focus:toggle_tag(tag)
                                               end
                                            end
                                      end)
   )
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- set keys:
root.keys(globalkeys)

--             |           
--   __| |   | |  _ \  __| 
--  |    |   | |  __/\__ \ 
-- _|   \__,_|_|\___|____/ 

-- rules to apply to new clients (through the "manage" signal):
awful.rules.rules = {
   -- all clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    raise = true,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                    size_hints_honor = false,
     }
   },

   -- non focus clients:
   { rule_any = {
        class = {
           "zoom",
        },
   }, properties = { raise = false }},
   
   -- floating clients:
   { rule_any = {
        class = {
           "scrcpy",
           "zoom",
        },
        name = {
           "Event Tester",  -- xev.
           --           "Zoom - Licensed Account", -- zoom main window
        },
        role = {
           "AlarmWindow",
           "pop-up",       -- e.g. google chrome's (detached) developer tools.
        }
   }, properties = { floating = true }},

   -- maximized clients:
   { rule_any = {
        name = {
           "Zoom Meeting",
        },
   }, properties = { maximized = true }},
   
   -- -- add titlebars to normal clients and dialogs SERIOUSELY?
   -- { rule_any = {type = { "normal", "dialog" }
   --              }, properties = { titlebars_enabled = true },
   -- },

   { rule_any = { -- vsc
        name = {
           "VSConsole",
   }}, properties = { tag = "∅", switchtotag = true }},
   
   
   { rule_any = { -- internet
        class = {
           "Google-chrome",
           "Chromium",
           "Firefox",
   }}, properties = { tag = "⚡", switchtotag = true }},
   
   { rule_any = { -- code
        class = {
           "Emacs",
   }}, properties = { tag = "$", switchtotag = true }},
   
   { rule_any = { -- file
        class = {
           "Nemo",
           "Thunar",
        },
        name = {
           "ranger:",
           "Waldläufer",
   }}, properties = { tag = "⛁", switchtotag = true }},
   
   { rule_any = { -- doc
        class = {
           "Evince",
           "libreoffice",
           "libreoffice-startcenter",
           "libreoffice-writer",
           "libreoffice-calc",
           "libreoffice-impress",
           "Qtiplot",
           "calibre",
           "calibre-gui",
           "Zathura",
   }}, properties = { tag =  "≣", switchtotag = true }},
   
   { rule_any = { -- music & video
        class = {
           "spotify",
           "Spotify",
           "Vlc",
           "vlc",
           "MPlayer",
           "mpv",
        },
        name = {
           "ncmpcpp 0.8.2",
           "ncmpcpp*",
   }}, properties = { tag = "♬", switchtotag = true }},
   
   { rule_any = { -- calendar
        class = {
           "gnome-calendar",
           "Gnome-calendar",
   }}, properties = { tag = "♻", switchtotag = true }},
   
   { rule_any = { -- mail
        class = {
           "Evolution",
   }}, properties = { tag = "@", switchtotag = true }},
   
   { rule_any = { -- comms
        class = {
           "psi",
           "Pidgin",
           "Skype",
           "Ts3client_linux_amd64",
           "TelegramDesktop",
           "Signal",
           "scrcpy",
           "rocket.chat",
           "Rocket.Chat"
   }}, properties = { tag = "✆", switchtotag = true }},
   
   { rule_any = { -- games & conf
        class = {
           "steam",
           "Steam",
           "zoom",
        },
        name = {
           "Zoom Meeting",
           "Zoom Cloud Meetings",
   }}, properties = { tag = "♞", switchtotag = true }},
   
   { rule_any = { -- clubs
        name = {
   }}, properties = { tag = "♠", switchtotag = true }},
   
   { rule_any = { -- hearts
        class = {
   }}, properties = { tag = "♥", switchtotag = true }},
   
   { rule_any = { -- diamonds
        class = {
   }}, properties = { tag = "♦", switchtotag = true }},
   
   { rule_any = { -- spades
        class = {
   }}, properties = { tag = "♣", switchtotag = true }},
}

--      _)                   | 
--   __| |  _` | __ \   _` | | 
-- \__ \ | (   | |   | (   | | 
-- ____/_|\__, |_|  _|\__,_|_| 
--        |___/                

-- signal function to execute when a new client appears:
client.connect_signal("manage", function (c)
                         -- set the windows at the slave,
                         -- i.e. put it at the end of others instead of setting it master.
                         -- if not awesome.startup then awful.client.setslave(c) end
                         
                         if awesome.startup and
                            not c.size_hints.user_position
                            and not c.size_hints.program_position then
                            -- prevent clients from being unreachable after screen count changes:
                            awful.placement.no_offscreen(c)
                         end
end)

-- enable sloppy focus, so that focus follows mouse:
client.connect_signal("mouse::enter", function(c)
                         if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
                            and awful.client.focus.filter(c) then
                            client.focus = c
                         end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--  |  _) |   |      
--  __| | __| |  _ \ 
--  |   | |   |  __/ 
-- \__|_|\__|_|\___| 

-- add a titlebar if titlebars_enabled is set to true in the rules:
client.connect_signal("request::titlebars", function(c)
                         -- buttons for the titlebar:
                         local buttons = awful.util.table.join(
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
                         )
                         
                         awful.titlebar(c) : setup {
                            { -- left:
                               awful.titlebar.widget.iconwidget(c),
                               buttons = buttons,
                               layout  = wibox.layout.fixed.horizontal
                            },
                            { -- middle:
                               { -- title:
                                  align  = "center",
                                  widget = awful.titlebar.widget.titlewidget(c)
                               },
                               buttons = buttons,
                               layout  = wibox.layout.flex.horizontal
                            },
                            { -- right:
                               awful.titlebar.widget.floatingbutton (c),
                               awful.titlebar.widget.maximizedbutton(c),
                               awful.titlebar.widget.stickybutton   (c),
                               awful.titlebar.widget.ontopbutton    (c),
                               awful.titlebar.widget.closebutton    (c),
                               layout = wibox.layout.fixed.horizontal()
                            },
                            layout = wibox.layout.align.horizontal
                                                   }
end)

