-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
-- Theme handling library
local beautiful = require("beautiful")

-- hold all the configuration
config = {}
-- needed by tools.rc
config.home = os.getenv("HOME") .. "/.config/awesome/"

-- tools require the config.home variable.
require("tools")
loadsafe("error-handle")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
beautiful.init("/usr/share/awesome/themes/default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "xterm"

-- Table of layouts to cover with awful.layout.inc, order matters.
config.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4}, s, config.layouts[1])
end
-- }}}

-- Default modkey.
config.modkey = "Mod4"

config.keys = {}
config.buttons = {}

-- keybindings.rc set the config.buttons and config.keys
loadsafe("keybindings")
-- menu definition
loadsafe("menu")
-- including launcher, tag list, task list, widget
loadsafe("statusbar")
-- rules.rc set the rules for client window
loadsafe("rules")

-- {{{ Mouse bindings for the root window
root.buttons(config.buttons.rootbuttons)
-- }}}

-- Set keys
root.keys(config.keys.globalkeys)
-- }}}
