local awful = require("awful")
local beautiful = require("beautiful")
-- Widget and layout library
local wibox = require("wibox")

-- Create a textclock widget
local mytextclock = awful.widget.textclock()

-- the launch icon
local mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = config.menu.mainmenu })

-- Create a wibox for each screen and add it
local mywibox = {}
local mypromptbox = {}
local mylayoutbox = {}

local mytaglist = {}
local mytasklist = {}

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(config.layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(config.layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(config.layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(config.layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, config.buttons.taglistbuttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, config.buttons.tasklistbuttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mytextclock)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end

config.keys.globalkeys = awful.util.table.join(config.keys.globalkeys,
    -- Prompt
    awful.key({ config.modkey },            "r",     function () mypromptbox[mouse.screen]:run() end)
					      )
