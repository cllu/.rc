local awful = require("awful")
local menubar = require("menubar")
local beautiful = require("beautiful")

-- {{{ Menu
-- Create a laucher widget and a main menu
config.menu = {}
config.menu.awesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

-- the menu used on the launcher
config.menu.mainmenu = awful.menu({ items = {
   { "awesome", config.menu.awesomemenu, beautiful.awesome_icon },
   { "open terminal", terminal }
   }
})

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

