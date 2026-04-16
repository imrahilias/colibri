--
--   _` |\ \  \   / _ \  __|  _ \  __ `__ \   _ \
--  (   | \ \  \ /  __/\__ \ (   | |   |   |  __/
-- \__,_|  \_/\_/ \___|____/\___/ _|  _|  _|\___|
--

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
local ror = require("aweror")
--local machi = require("layout-machi")
--local revelation=require("revelation")

--
--   _ \  __|  __| _ \   __|
--   __/ |    |   (   | |
-- \___|_|   _|  \___/ _|
--

-- Check if awesome encountered an error during startup and fall back to another
-- config (this code will only ever execute for the fallback config).
if awesome.startup_errors then
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "fallback mode",
                    text = awesome.startup_errors })
end

-- Handle runtime errors after startup.
do
   local in_error = false
   awesome.connect_signal(
      "debug::error",
      function (err)
         -- make sure we don't go into an endless error loop.
         if in_error then return end
         in_error = true
         naughty.notify({ preset = naughty.config.presets.critical,
                          title = "error:",
                          text = tostring(err) })
         in_error = false
   end)
end

-- _)      _) |
--  | __ \  | __|
--  | |   | | |
-- _|_|  _|_|\__|
--

-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/m/.config/awesome/light.lua")
--beautiful.layout_machi = machi.get_icon()

-- Revelation, load after beautiful.
--revelation.init()

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "emacs" or "nvim" or "vim" or "vi"
editor_cmd = terminal .. " -e " .. editor

-- Usually, mod4 is the key with a logo between control and alt.  if you do not
-- like this or do not have such a key, i suggest you to remap mod4 to another
-- key using xmodmap or other tools.  however, you can use another modifier like
-- mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   awful.layout.suit.tile,
   awful.layout.suit.max,
   -- awful.layout.suit.tile.bottom,
   -- awful.layout.suit.fair,
   -- awful.layout.suit.spiral,
   -- awful.layout.suit.spiral.dwindle,
   awful.layout.suit.floating,
   -- machi.default_layout,
}

--  |
--  |  /  _ \ |   |  __|
--    <   __/ |   |\__ \
-- _|\_\\___|\__, |____/
--           ____/

-- Key bindings.
globalkeys = gears.table.join(

   -- Awesome.
   awful.key({ modkey, "Shift" }, "q", awesome.restart),
   awful.key({ modkey, "Shift", "Control" }, "q", awesome.quit),

   -- Machi.
   -- awful.key({ modkey }, ".", function () machi.default_editor.start_interactive() end),
   -- awful.key({ modkey }, "/", function () machi.switcher.start(client.focus) end),

   -- -- Revelation.
   -- awful.key({ modkey }, "w", revelation),
   -- awful.key({ modkey, "Shift" }, "w", function () -- only terminals.
   --       revelation({rule={class="URxvt"}})
   -- end),

   -- -- Current tag only.
   -- awful.key({ modkey, "Control" }, "w", function ()
   --       revelation({rule={class="conky"}, is_excluded=true,
   --                   curr_tag_only=true})
   -- end),

   -- -- Not working: Only terminals.
   -- awful.key({ modkey, "Shift", "Control" }, "w", function ()
   --       revelation({
   --             rule{class={"URxvt", "Xterm"},
   --                   any=true}
   --       })
   -- end),

   -- Reset all clients on current screen.
   awful.key({ modkey }, "c",
      function ()
         --awful.layout.set(awful.layout.suit.corner.nw)
         for _, c in ipairs(client.get()) do
            if c.maximized then c.maximized = not c.maximized end
            if c.minimized then c.minimized = not c.minimized end
            c:raise()
         end
   end),

   -- Menubar.
   --awful.key({ modkey }, "p", function () menubar.show() end),
   awful.key({ modkey }, "a", function () awful.spawn("rofi -drun-match-fields name -show drun -show-icons") end),
   awful.key({ modkey, "Shift" }, "a", function () awful.spawn("rofi -drun -show window") end),
   awful.key({ modkey, "Control" }, "a", function () awful.spawn("rofi -drun -show windowcd") end),

   -- Navigation.
   awful.key({ modkey }, "Left",  awful.tag.viewprev),
   awful.key({ modkey }, "j",  awful.tag.viewprev),
   awful.key({ modkey }, "Right",  awful.tag.viewnext),
   awful.key({ modkey }, "k",  awful.tag.viewnext),
   awful.key({ modkey }, "Escape", awful.tag.history.restore),
   awful.key({ modkey }, "z", awful.client.urgent.jumpto),

   -- Layout manipulation.
   awful.key({ modkey }, "Tab", function () awful.client.focus.byidx(1) end),
   awful.key({ modkey }, "l", function () awful.tag.incmwfact( 0.05) end),
   awful.key({ modkey }, "h", function () awful.tag.incmwfact(-0.05) end),
   awful.key({ modkey }, "space", function () awful.layout.inc( 1) end), -- "space" is lo-caps!
   awful.key({ modkey, "Shift" }, "space", function () awful.layout.inc(-1) end), -- "space" is lo-caps!

   -- Restore minimised client.
   awful.key({ modkey, "Shift" }, "n",
      function ()
         local c = awful.client.restore()
         if c then -- focus restored client.
            client.focus = c
            c:raise()
         end
   end),

   -- Toggle wibox (taskbar).
   awful.key({ modkey, "Shift" }, "f",
      function ()
         myscreen = awful.screen.focused()
         myscreen.mywibox.visible = not myscreen.mywibox.visible
   end),

   -- Toggle systray visibility.
   awful.key({ modkey, "Control" }, "f",
      function ()
         myscreen = awful.screen.focused()
         myscreen.mysystray.visible = not myscreen.mysystray.visible
   end),

   -- Run prompt.
   --awful.key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end),

   -- Remind prompt.
   awful.key({ modkey }, "<",
      function ()
         awful.prompt.run {
            --prompt       = '<span color="orange">Remind: </span>',
            --bg_cursor    = 'orange',
            prompt       = 'Remind: ',
            textbox      = mouse.screen.mypromptbox.widget,
            exe_callback = function (input)
               if not input or #input == 0 then
                  naughty.notify{ text = 'usage: set reminder with "remind $time $name" eg "remind 10s asdf"'..input }
                  return
               end
               awful.spawn( 'remind '..input )
            end
         }
   end),

   -- Launch. Icons:   
   awful.key({ modkey, "Control"}, "q", function () awful.spawn("systemctl suspend") end),
   awful.key({ modkey }, "Return", function () awful.spawn(terminal) end),
   awful.key({ modkey, "Shift"}, "Return", function () awful.spawn('urxvt -e sh -c "ssh login01"') end),
   awful.key({ modkey, "Control" }, "Return", function () awful.spawn('urxvt -e sh -c "TERM=rxvt-unicode ssh l55"') end),
   awful.key({ modkey }, "d", function () awful.spawn.with_shell('urxvt -title "  Yazi" -e zsh -i -c "yazi"', false) end),
   awful.key({ modkey, "Control" }, "d", function () awful.spawn.with_shell('urxvt -e sudo yazi', false) end),
   awful.key({ modkey }, "e", function() awful.spawn("emacsclient -ca ''", false) end),
   awful.key({ modkey, "Shift" }, "s", function () awful.spawn("/home/m/bin/open_primary_selection_in_browser") end),
   awful.key({ modkey, "Control" }, "s", function () awful.spawn("/home/m/bin/open_primary_selection_in_google_translate") end),
   awful.key({ modkey, "Shift", "Control" }, "s", function () awful.spawn("/home/m/bin/open_primary_selection_in_thesaurus") end),
   awful.key({ modkey, "Shift" }, "z", function () awful.spawn("/home/m/bin/open_primary_selection_as_qr_code") end),
   awful.key({ modkey, "Shift" }, "e", function () awful.spawn("/home/m/bin/open_primary_selection_in_emacs") end),
   awful.key({ modkey, "Shift" }, "d", function () awful.spawn("/home/m/bin/open_primary_selection_in_yazi") end),
   awful.key({ modkey, "Shift" }, "t", function () awful.spawn.with_shell("urxvt -e rtorrent") end),
   awful.key({ modkey }, "g", function () awful.spawn.with_shell("urxvt -e htop") end),
   awful.key({ modkey, "Shift" }, "g", function () awful.spawn.with_shell("urxvt -e top") end),
   -- awful.key({ modkey }, "x", function () awful.spawn("urxvt -T 'VSConsole' -fa 'xft:DejaVuSansMono' -fs 24 -e 'bash'") end),
   -- awful.key({ modkey, "Shift" }, "x", function () awful.spawn("urxvt -T 'VSConsole' -fa 'xft:DejaVuSansMono' -fs 24 -e 'trainee'") end),
   awful.key({ modkey }, "p", function () awful.spawn("wdisplays") end),
   --awful.key({ modkey, "Shift" }, "p", function () awful.spawn("autorandr --change") end),
   awful.key({ modkey }, "l", function () naughty.notify{ text = 'HAHA: nice try Mr. Hickel. Get lost 😁'} end),

   -- Audio.
   awful.key({ }, "XF86AudioRaiseVolume", function () awful.spawn("amixer set Master 1%+", false) end),
   awful.key({ }, "XF86AudioLowerVolume", function () awful.spawn("amixer set Master 1%-", false) end),
   --awful.key({ }, "XF86AudioMute", function () awful.spawn('for x in {"Master","Headphone","Front","Surround","Center","LFE","Side"} ; do amixer -c 0 set "${x}" toggle; done', false) end)
   awful.key({ }, "XF86AudioMute", function () awful.spawn("amixer set Master toggle", false) end),
   awful.key({ }, "XF86AudioMicMute", function () awful.spawn("amixer set Capture toggle", false) end),
   awful.key({ }, "XF86Tools", function () awful.spawn("spotify", false) end),
   awful.key({ }, "XF86AudioPrev", function () awful.spawn("playerctl previous", false) end),
   awful.key({ }, "XF86AudioPlay", function () awful.spawn("playerctl play-pause", false) end),
   awful.key({ }, "XF86AudioNext", function () awful.spawn("playerctl next", false) end),

   -- Screen.
   --awful.key({ }, "Print", function () awful.spawn("scrot -s -e 'mv $f ~/.screens/ 2>/dev/null'") end), -- screenshot selection
   --awful.key({ modkey }, "Print", function () awful.spawn("scrot -e 'mv $f ~/.screens/ 2>/dev/null'") end), -- screenshot screen

   -- Kill.
   --awful.key({ modkey, "Shift" }, "k", function () awful.spawn("sudo xkill", false) end),

   -- Display.
   awful.key({ }, "XF86MonBrightnessDown", function () awful.spawn("sudo light -U 30", false) end),
   awful.key({ }, "XF86MonBrightnessUp", function () awful.spawn("sudo light -A 30", false) end),
   --awful.key({ }, "XF86Display", function () awful.spawn("xset dpms force off", false) end),
   awful.key({ modkey }, "#86", function () awful.spawn("razercfg -l glowinglogo:off -l scrollwheel:on", false) end),
   awful.key({ modkey }, "#82", function () awful.spawn("razercfg -l all:off", false) end)
   --awful.key({ modkey, "Shift" }, "r", function () awful.spawn("xrandr -o 1", false) end),
   --awful.key({ modkey, "Control", "Shift" }, "r", function () awful.spawn("xrandr -o 0", false) end)
)

-- Client keys.
clientkeys = gears.table.join(
   awful.key({ modkey }, "q", function (c) c:kill() end),
   awful.key({ modkey, "Shift" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
   -- Drag tag left; move the current tag to the left in the taglist, and switch
   -- there.
   awful.key({ modkey, "Shift" }, "Left",
      function (t)
         if client.focus then
            tag = client.focus.screen.tags[awful.tag.getidx()-1]
            if tag then
               client.focus:move_to_tag(tag)
               awful.tag.viewprev(t.screen)
            end
         end
      end
   ),
   -- Drag tag right; move the current tag to the right in the taglist, and
   -- switch there.
   awful.key({ modkey, "Shift" }, "Right",
      function (t)
         if client.focus then
            tag = client.focus.screen.tags[awful.tag.getidx()+1]
            if tag then
               client.focus:move_to_tag(tag)
               awful.tag.viewnext(t.screen)
            end
         end
      end
   ),
   awful.key({ modkey }, "b",  awful.client.floating.toggle),
   awful.key({ modkey }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
   end),
   awful.key({ modkey }, "m",
      function (c)
         c.maximized = not c.maximized
         c:raise()
   end),
   awful.key({ modkey, "Control" }, "m",
      function (c)
         c.maximized_vertical = not c.maximized_vertical
         c:raise()
   end),
   -- awful.key({ modkey, "Shift" }, "m",
   --    function (c)
   --       c.maximized_horizontal = not c.maximized_horizontal
   --       c:raise()
   -- end),
   awful.key({ modkey }, "f",
      function (c)
         c.fullscreen = not c.fullscreen
         c:raise()
   end),
   -- Switch to the left/right tag.
   awful.key({ modkey, "Shift"}, "wheel-up", function (t) awful.tag.viewprev(t.screen) end),
   awful.key({ modkey, "Shift"}, "wheel-down", function (t) awful.tag.viewnext(t.screen) end),
   -- Drag tag left; Move the current tag to the left in the taglist, and follow.
   awful.key({ modkey, "Control" }, "wheel-up",
      function (t)
         if client.focus then
            tag = client.focus.screen.tags[awful.tag.getidx()-1]
            if tag then
               client.focus:move_to_tag(tag)
               awful.tag.viewprev(t.screen)
            end
         end
      end),
   -- Drag tag right; Move the current tag to the right in the taglist, and follow.
   awful.key({ modkey, "Control" }, "wheel-down",
      function (t)
         if client.focus then
            tag = client.focus.screen.tags[awful.tag.getidx()+1]
            if tag then
               client.focus:move_to_tag(tag)
               awful.tag.viewnext(t.screen)
            end
         end
      end)
)

-- Bind all key numbers to tags. Be careful: We use keycodes to make it works on
-- any keyboard layout. This should map on the top row of your keyboard, usually
-- 1 to 9.
local keys = { "#49", 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, "#20", "#21", "#22" }
for i = 1, 14 do
   globalkeys = gears.table.join(
      globalkeys,
      -- View tag only.
      awful.key({ modkey }, keys[i],
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               tag:view_only()
            end
      end),
      -- Toggle tag display.
      awful.key({ modkey, "Control" }, keys[i],
         function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
               awful.tag.viewtoggle(tag)
            end
      end),
      -- Move client to tag & follow.
      awful.key({ modkey, "Shift" }, keys[i],
         function ()
            if client.focus then
               local tag = client.focus.screen.tags[i]
               if tag then
                  client.focus:move_to_tag(tag)
                  tag:view_only()
               end
            end
      end),
      -- Toggle tag on focused client.
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

-- Generate and add the 'run or raise' key bindings to the globalkeys table.
globalkeys = gears.table.join(globalkeys, ror.genkeys(modkey))

-- Set root keys.
root.keys(globalkeys)

--
--  __ `__ \   _ \  |   |  __|  _ \
--  |   |   | (   | |   |\__ \  __/
-- _|  _|  _|\___/ \__,_|____/\___|
--

local function lower_client_state(c)
    if c.maximized then
       c.maximized = false
       c:raise()
       c:swap(awful.client.getmaster())
    else
       c.minimized = true
    end
end

local function raise_client_state(c)
    if c.minimized then
       c.minimized = false
    else
       c.maximized = true
    end
    c:raise()
    c:swap(awful.client.getmaster())
end

-- Client mouse bindings that should also work anywhere always (no
-- client on blank desktop).
root.buttons(gears.table.join(
                --awful.button({ modkey, "Shift" }, 2, revelation),
                awful.button({ modkey, "Control" }, 2, function () awful.spawn("rofi -drun -show window") end)
                -- awful.button({ }, 3, function () mymainmenu:toggle() end),
                --awful.button({ modkey }, 4, awful.tag.viewprev),
                --awful.button({ modkey }, 5, awful.tag.viewnext)
))

-- Mouse bindings for tags in tasklist for each screen.
local mytaglist_buttons = gears.table.join(
   awful.button({ }, 1, function (t) t:view_only() end),
   -- move client to tag and follow.
   awful.button({ }, 2, function (t)
         if client.focus then
            client.focus:move_to_tag(t)
            t:view_only()
         end
   end),
   awful.button({ }, 3, awful.tag.viewtoggle),
   awful.button({ modkey }, 3, function (t)
         if client.focus then
            client.focus:toggle_tag(t)
         end
   end)
   -- -- Switch tags in tag list, confusing.
   --awful.button({ }, 4, function (t) awful.tag.viewprev(t.screen) end),
   --awful.button({ }, 5, function (t) awful.tag.viewnext(t.screen) end)
)

-- Mouse bindings for clients in tasklist.
local mytasklist_buttons = gears.table.join(
   --  On tasklist: Force unmaximize on click in tasklist and make main.
   -- awful.button({ }, 1, function (c)
   --       client.focus = c
   --       c:raise()
   --       --c:swap(awful.client.getmaster()) -- that switches the order, so no double clicking.
   -- end),
   --  On tasklist: traditional un/minimise client.
   awful.button({ }, 1,
      function (c)
         if c == client.focus then
            c.minimized = true
         else
            -- without this, the following :isvisible() makes no sense.
            c.minimized = false
            if not c:isvisible() and c.first_tag then
               c.first_tag:view_only()
            end
            -- this will also un-minimize the client, if needed.
            client.focus = c
            c:raise()
         end
   end),
   -- On tasklist: Kill client (analogous to revelation).
   awful.button({ }, 2, function (c)
         c:kill()
   end),
   -- -- On tasklist: Unmaximize both this client and the client in focus
   -- -- already; Uncommented because this is now the default even if you just
   -- -- hover.
   awful.button({ }, 3,
      function(c)
         c.maximized = false
         if client.focus then client.focus.maximized = false end
         -- this will also un-minimize the client, if needed.
         client.focus = c
         c:raise()
   end),
   -- -- Switch clients in tasklist confusing when scrolling, can switch with
   -- -- mod+mouse anywhere anyway.
   --awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
   --awful.button({ }, 5, function () awful.client.focus.byidx(-1) end),
   awful.button({ }, 8, function (c) lower_client_state(c) end),
   awful.button({ }, 9, function (c) raise_client_state(c) end)
)

-- Mouse bindings for clients.
clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 2, function (c) c:kill() end),
   awful.button({ modkey, "Shift"  }, 2, revelation),
   awful.button({ modkey, "Control" }, 2, function () awful.spawn("rofi -drun -show window") end),
   awful.button({ modkey }, 3, function (c)
         c:emit_signal("request::activate", "mouse_click", {raise = true})
         awful.mouse.client.resize(c)
   end),
   -- Switch to the left/right tag.
   --awful.button({ modkey, "Shift"}, 4, function (t) awful.tag.viewprev(t.screen) end),
   --awful.button({ modkey, "Shift"}, 5, function (t) awful.tag.viewnext(t.screen) end),
   -- -- Drag tag left; Move the current tag to the left in the taglist, and follow.
   -- awful.button({ modkey, "Control" }, 4,
   --    function (t)
   --       if client.focus then
   --          tag = client.focus.screen.tags[awful.tag.getidx()-1]
   --          if tag then
   --             client.focus:move_to_tag(tag)
   --             awful.tag.viewprev(t.screen)
   --          end
   --       end
   --    end
   -- ),
   -- -- Drag tag right; Move the current tag to the right in the taglist, and follow.
   -- awful.button({ modkey, "Control" }, 5,
   --    function (t)
   --       if client.focus then
   --          tag = client.focus.screen.tags[awful.tag.getidx()+1]
   --          if tag then
   --             client.focus:move_to_tag(tag)
   --             awful.tag.viewnext(t.screen)
   --          end
   --       end
   -- end),
   awful.button({ modkey }, 8,
      function (c)
         lower_client_state(c)
   end),
   awful.button({ modkey }, 9,
      function (c)
         raise_client_state(c)
   end)
   -- -- -- Un/maximise current client, set master; Good combination to exit the
   -- -- -- cyclic max preview loop below with just the raised client.
   -- awful.button({ modkey }, 8,
   --    function (c)
   --       c.maximized = false
   --       c:raise()
   --       c:swap(awful.client.getmaster())
   -- end),
   -- awful.button({ modkey }, 9,
   --    function (c)
   --       c.maximized = true
   --       c:raise()
   --       c:swap(awful.client.getmaster())
   -- end)
   --    -- Cyclic Max Preview: Unmaximise current client, cycle to next/previous
   -- -- client on current tag and maximize that
   --    awful.button({ modkey }, 4, function ()
   --       for _, c in ipairs(client.get()) do c.maximized = false end
   --       awful.client.focus.byidx(-1)
   --       if client.focus then
   --          client.focus.maximized = true
   --          client.focus:raise()
   --       end
   -- end),
   -- awful.button({ modkey }, 5, function ()
   --       for _, c in ipairs(client.get()) do c.maximized = false end
   --       awful.client.focus.byidx(-1)
   --       if client.focus then
   --          client.focus.maximized = true
   --          client.focus:raise()
   --       end
   -- end)
)

--             |
--   __| |   | |  _ \  __|
--  |    |   | |  __/\__ \
-- _|   \__,_|_|\___|____/
--

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {

   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
                    border_color = beautiful.border_normal,
                    focus = awful.client.focus.filter,
                    raise = false,
                    keys = clientkeys,
                    buttons = clientbuttons,
                    screen = awful.screen.preferred,
                    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                    size_hints_honor = false,
                    titlebars_enabled = false,
     }
   },

   -- Non-focus clients.
   { rule_any = {
        class = {
           "zoom",
           "1password",
           "1Password",
           "Nextcloud",
           "nextcloud",
        },
        name = {
           "Meeting",
           "Zoom Workplace - Licensed account",
           "Meeting chat",
           "Breakout rooms - In Progress",
           "TU Wien — Employee — 1Password",
        }
   }, properties = { focus = false }},

   -- Non-raise clients.
   { rule_any = {
        class = {
           "zoom",
           "1password",
           "1Password",
           "Nextcloud",
           "nextcloud",
        },
        name = {
           "Meeting",
           "Zoom Workplace - Licensed account",
           "Meeting chat",
           "Breakout rooms - In Progress",
           --"TU Wien — Employee — 1Password",
        }
   }, properties = { raise = false }},

   -- Floating clients.
   { rule_any = {
        class = {
           "Scrcpy",
           --"zoom",
           "Blueman-manager",
           "Sxiv",

        },
        name = {
           "Event Tester",  -- xev.
           --"Zoom - Licensed Account", -- zoom main window
        },
        role = {
           "AlarmWindow", -- thunderbird's calendar.
           "ConfigManager",  -- thunderbird's about:config.
           "pop-up",       -- e.g. google chrome's (detached) developer tools.
        }
   }, properties = { floating = true }},

   -- Non-floating clients.
   { rule_any = {
        class = {
           "Nextcloud",
        },
   }, properties = { floating = false }},

   -- Maximized clients.
   { rule_any = {
        class = {
           "Discord",
           "Element",
           "Slack",
           "Signal",
           "TelegramDesktop",
           "firefox",
        },
        name = {
           --"Zoom Meeting",
           --"Zoom - Licensed Account",
        },
   }, properties = { maximized = true }},



   -- ASC
   { rule_any = {
        class = {
           --"XTerm",
   }}, properties = { tag = " ", switchtotag = true }},

   -- Internet
   { rule_any = {
        class = {
           "chromium",
           "firefox",
           "Navigator",
           "google-chrome",
           "Google-chrome",
           "luakit",
           "Luakit",
   }}, properties = { tag = "󰇧 ", switchtotag = true }},

   -- Code
   { rule_any = {
        class = {
           "Emacs",
   }}, properties = { tag = "󰛓 ", switchtotag = true }},

   -- File
   { rule_any = {
        class = {
           "Nemo",
           "Thunar",
        },
        name = {
           "  Yazi", -- customised file manager
   }}, properties = { tag = " ", switchtotag = true }},

   -- Doc
   { rule_any = {
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
   }}, properties = { tag =  " ", switchtotag = true }},

   -- Music & video
   { rule_any = {
        class = {
           "Spotify",
           "Vlc",
           "Mplayer",
           "Mpv",
           "mpv",
        },
        name = {
           "ncmpcpp 0.8.2",
           "ncmpcpp",
   }}, properties = { tag = " ", switchtotag = true }},

   -- Calendar
   { rule_any = {
        class = {
           "gnome-calendar"
   }}, properties = { tag = "⏾", switchtotag = true }},

   -- Mail
   { rule_any = {
        class = {
           "Evolution",
   }}, properties = { tag = " ", switchtotag = true }},

   -- Comms
   { rule_any = {
        class = {
           "Psi",
           "Pidgin",
           "Skype",
           "Ts3client_linux_amd64",
           "TelegramDesktop",
           "Signal",
           "scrcpy",
           "Slack",
           "Element",
   }}, properties = { tag = " ", switchtotag = true }},

   -- Games & conf.
   { rule_any = {
        class = {
           "steam",
           "zoom",
           "Zoom",
        },
        name = {
           "Zoom Meeting",
           "Zoom Cloud Meetings",
           "Torronator",
   }}, properties = { tag = "󰼁 ", switchtotag = true }},

   -- Melon.
   { rule_any = {
        name = {
   }}, properties = { tag = "󱁇 ", switchtotag = true }},

   -- Grapes.
   { rule_any = {
        class = {
   }}, properties = { tag = "󱁄 ", switchtotag = true }},

   -- Cherry.
   { rule_any = {
        class = {
   }}, properties = { tag = "󱁂 ", switchtotag = true }},

   -- Ananas.
   { rule_any = {
        class = {
   }}, properties = { tag = "󱁆 ", switchtotag = true }},
}


--           _) |
-- \ \  \   / | __ \   _` |  __|
--  \ \  \ /  | |   | (   | |
--   \_/\_/  _|_.__/ \__,_|_|
--

local function set_wallpaper(s)
   if beautiful.wallpaper then
      local wallpaper = beautiful.wallpaper
      -- If wallpaper is a function, call it with the screen.
      if type(wallpaper) == "function" then
         wallpaper = wallpaper(s)
      end
      gears.wallpaper.maximized(wallpaper, s, true)
   end
end

-- Reset wallpaper when a screen's geometry changes (e.g. different resolution).
screen.connect_signal("property::geometry", set_wallpaper)

-- Menubar configuration: set the terminal for applications that require it.
menubar.utils.terminal = terminal

-- Keyboard map indicator and switcher.
mykeyboardlayout = awful.widget.keyboardlayout()

awful.screen.connect_for_each_screen(
   function (s)
      set_wallpaper(s)

      -- Each screen has its own tag table.
      -- Unicode with bitstream vera: ↯ ♫ ♞ ♟ ♤ ♡ ♢ ♧ ⚛  ✫ ♻ ✇ ∅ ⚡ $ ⛁ ≣ ♬ ⏾ @ ✆♞ ♠ ♥ ♦ ♣
      -- Unicode with bitstrom wera nerd fonts:   󰟢  ⚡   󰇧 󰛍         󰭹  󰛓 󰇂 󰽚 󱥐 󰜁 󱥔 󰖟   󰇈 󰛍 󱁇 󱁆 󱁄 󱨎 󱁂 󱁃 󰼂 󰼁  󰦪  󰦨   ♬ 󰝚 󰎌 󰎈 󰽰 󱑽  󰒊  󱅥   
      awful.tag({ " ", "󰇧 ", "󰛓 ", " ", " ", " ", "⏾", " ", " ","󰼁 ", "󱁇 ", "󱁄 ", "󱁂 ", "󱁆"  }, s, awful.layout.layouts[1])

      -- Create a promptbox for each screen.
      s.mypromptbox = awful.widget.prompt()

       -- Create a tasklist for each screen.
      s.mytasklist = awful.widget.tasklist {
         screen   = s,
         filter   = awful.widget.tasklist.filter.currenttags,
         buttons  = mytasklist_buttons,
         -- Notice that there is *no* wibox.wibox prefix, it is a template.
         widget_template = {
            {
               {
                  {
                     id     = 'icon_role',
                     widget = wibox.widget.imagebox,
                  },
                  margins = 4,
                  widget  = wibox.container.margin,
               },
               {
                  id     = 'text_role',
                  widget = wibox.widget.textbox,
               },
               layout = wibox.layout.fixed.horizontal,
            },
            left    = 10,
            right   = 10,
            widget  = wibox.container.margin,

            -- -- Preview task on hover; Signal to maximise client on hover over,
            -- -- unmaximise on leaving on each task.
            -- create_callback = function(self, c, _, _)
            --    self:connect_signal("mouse::enter", function()
            --                           if c.valid then
            --                              c.maximized = true
            --                              client.focus = c
            --                              c:raise()
            --                           end
            --    end)

            --    self:connect_signal("mouse::leave", function()
            --                           if c.valid then
            --                              c.maximized = false
            --                           end
            --    end)
            -- end

         }
      }

      -- Create a taglist widget for each screen.
      s.mytaglist = awful.widget.taglist {
         screen  = s,
         filter  = awful.widget.taglist.filter.all,
         buttons = mytaglist_buttons,
      }

      -- Create a textclock widget on each screen.
      s.mytextclock = wibox.widget.textclock('<span color="DeepSkyBlue">%_H:%M </span>', 5)

      -- Cant create a Systray for each screen. Systray would stick to the last
      -- screen in the loop; So stick it to the primary screen instead.
      if s.index == 1 then
         s.mysystray = wibox.widget.systray()
      else
         s.mysystray = wibox.widget.textbox() -- A dummy placeholder widget.
      end

      -- By default the tray is invisible. Ensure icons are a consistent size so
      -- spacing looks even.
      s.mysystray.visible = false
      s.mysystray.base_size = 20

      -- Wrap the systray in a constraint/width container so the taskbar does
      -- not clip when the tray un/hides. Align systray icons to the right.
      s.systray_wrapper = wibox.widget {
         {
            s.mysystray,
            halign = "right",
            widget = wibox.container.place
         },
         forced_width = 150,
         layout = wibox.container.constraint
      }

      -- Create the main tray container.
      s.mytray_widget = wibox.widget {
         s.systray_wrapper,
         layout = wibox.layout.fixed.horizontal
      }

      -- Signals to un/hide the systray inside the container.
      s.mytray_widget:connect_signal("mouse::enter", function()
                                        s.mysystray.visible = true
      end)

      s.mytray_widget:connect_signal("mouse::leave", function()
                                        s.mysystray.visible = false
      end)

      -- Create the wibox.
      s.mywibox = awful.wibar({ position = "top", screen = s, height = 30, opacity = 1 })

      -- Add widgets to the wibox with gap between the container and clock.
      s.mywibox:setup {
         layout = wibox.layout.align.horizontal,
         {
            layout = wibox.layout.fixed.horizontal,
            s.mytaglist,
            s.mypromptbox,
         },
         s.mytasklist,
         {
            layout = wibox.layout.fixed.horizontal,
            s.mytray_widget,
            s.mytextclock,
            spacing = 20,
         },
      }
   end
)


--      _)                   |
--   __| |  _` | __ \   _` | |
-- \__ \ | (   | |   | (   | |
-- ____/_|\__, |_|  _|\__,_|_|
--        |___/

-- Signal function to execute when a new client appears.
client.connect_signal(
   "manage",
   function (c)
      -- -- Set the windows at the slave, i.e. put it at the end of others instead
      -- -- of setting it master.
      -- if not awesome.startup then
      --    awful.client.setslave(c)
      -- end

      if awesome.startup and
         not c.size_hints.user_position
         and not c.size_hints.program_position then
         -- Prevent clients from being unreachable after screen count changes.
         awful.placement.no_offscreen(c)
      end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal(
   "request::titlebars",
   function(c)
      local buttons = gears.table.join(
         awful.button({ }, 1, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.move(c)
         end),
         awful.button({ }, 3, function()
               c:emit_signal("request::activate", "titlebar", {raise = true})
               awful.mouse.client.resize(c)
         end)
      )

      awful.titlebar(c) : setup {
         { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
         },
         { -- Middle
            { -- Title
               align  = "center",
               widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
         },
         { -- Right
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

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

-- Change border on un/focus.
client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

--              |              |              |
--   _` | |   | __|  _ \   __| __|  _` |  __| __|
--  (   | |   | |   (   |\__ \ |   (   | |    |
-- \__,_|\__,_|\__|\___/ ____/\__|\__,_|_|   \__|
--

autorun = true
autorunners =
   {
      -- Start deamons.
      "udiskie",
      "urxvtd -q -f -o",
      -- "xss-lock -n /usr/lib/xsecurelock/dimmer -l -- xsecurelock",
      "emacs --daemon",
      "1password",

      -- Start trays.
      --"pasystray",
      --"blueman-applet",
      --"nm-applet",
      --"cbatticon -r 10 -c 'notify-send Power on 10%'",
   }

if autorun then
   -- These just add up.
   --awful.spawn.with_shell("killall pasystray")
   --awful.spawn.with_shell("killall cbatticon")

   -- These dont switch mode otherwise.
   --awful.spawn.with_shell("killall blueman-applet && nm-applet")

   -- Both *cloud runs on qt, which runs qt6gtk2. So whenever qt or gtk
   -- change, this qt6gtk2 aur pkg needs to be rebuilt (install, no clean
   -- build). Nextcloud might not like the gtk2 theme, so needs to
   -- run default.
   --awful.spawn.with_shell("QT_QPA_PLATFORMTHEME='' QT_STYLE_OVERRIDE='' nextcloud")

   for _, app in ipairs(autorunners) do
      awful.spawn.once(app, awful.rules.rules)
   end
end

--           _)
--  __ `__ \  |  __|  __|
--  |   |   | |\__ \ (
-- _|  _|  _|_|____/\___|
--

-- Run garbage collector regularly to prevent memory leaks.
gears.timer {
   timeout = 30,
   autostart = true,
   callback = function () collectgarbage() end
}
