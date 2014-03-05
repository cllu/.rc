-- http://awesome.naquadah.org/wiki/Rman's_Simple_Volume_Widget
--

local vicious = require("vicious")
local awful = require("awful")

local alsawidget = {
channel = "Master",
step = "5%",
colors =
   {
   unmute = "#AECF96",
   mute = "#FF5656"
   },
   mixer = terminal .. " -e alsamixer", -- or whatever your preferred sound mixer is
   notifications =
 {
 icons =
    {
    -- the first item is the 'muted' icon
    "/usr/share/icons/gnome/48x48/status/audio-volume-muted.png",
    -- the rest of the items correspond to intermediate volume levels - you can have as many as you want (but must be >= 1)
    "/usr/share/icons/gnome/48x48/status/audio-volume-low.png",
    "/usr/share/icons/gnome/48x48/status/audio-volume-medium.png",
    "/usr/share/icons/gnome/48x48/status/audio-volume-high.png"
    },
    font = "Monospace 11", -- must be a monospace font for the bar to be sized consistently
    icon_size = 48,
    bar_size = 20 -- adjust to fit your font if the bar doesn't fit
 }
}

-- widget
alsawidget.bar = awful.widget.progressbar ()
alsawidget.bar:set_width (8)
alsawidget.bar:set_vertical (true)
alsawidget.bar:set_background_color ("#494B4F")
alsawidget.bar:set_color (alsawidget.colors.unmute)
alsawidget.bar:buttons (awful.util.table.join (
		      awful.button ({}, 1, function()
				       awful.util.spawn (alsawidget.mixer)
					   end),
		      awful.button ({}, 3, function()
				       awful.util.spawn ("amixer sset " .. alsawidget.channel .. " toggle")
				       vicious.force ({ alsawidget.bar })
					   end),
		      awful.button ({}, 4, function()
				       awful.util.spawn ("amixer sset " .. alsawidget.channel .. " " .. alsawidget.step .. "+")
				       vicious.force ({ alsawidget.bar })
					   end),
		      awful.button ({}, 5, function()
				       awful.util.spawn ("amixer sset " .. alsawidget.channel .. " " .. alsawidget.step .. "-")
				       vicious.force ({ alsawidget.bar })
					   end)
					 ))
-- tooltip
alsawidget.tooltip = awful.tooltip ({ objects = { alsawidget.bar } })
-- naughty notifications
alsawidget._current_level = 0
alsawidget._muted = false

function alsawidget:notify ()
   local preset =
 {
 height = 75,
 width = 300,
 font = alsawidget.notifications.font
 }
 local i = 1;
 while alsawidget.notifications.icons[i + 1] ~= nil
 do
    i = i + 1
 end
 if i >= 2
 then
    preset.icon_size = alsawidget.notifications.icon_size
    if alsawidget._muted or alsawidget._current_level == 0
    then
       preset.icon = alsawidget.notifications.icons[1]
    elseif alsawidget._current_level == 100
    then
       preset.icon = alsawidget.notifications.icons[i]
    else
       local int = math.modf (alsawidget._current_level / 100 * (i - 1))
       preset.icon = alsawidget.notifications.icons[int + 2]
    end
 end
 if alsawidget._muted
 then
    preset.title = alsawidget.channel .. " - Muted"
 elseif alsawidget._current_level == 0
 then
    preset.title = alsawidget.channel .. " - 0% (muted)"
    preset.text = "[" .. string.rep (" ", alsawidget.notifications.bar_size) .. "]"
 elseif alsawidget._current_level == 100
 then
    preset.title = alsawidget.channel .. " - 100% (max)"
    preset.text = "[" .. string.rep ("|", alsawidget.notifications.bar_size) .. "]"
 else
    local int = math.modf (alsawidget._current_level / 100 * alsawidget.notifications.bar_size)
    preset.title = alsawidget.channel .. " - " .. alsawidget._current_level .. "%"
    preset.text = "[" .. string.rep ("|", int) .. string.rep (" ", alsawidget.notifications.bar_size - int) .. "]"
 end
 if alsawidget._notify ~= nil
 then

    alsawidget._notify = naughty.notify (
       {
	  replaces_id = alsawidget._notify.id,
	  preset = preset
       })
 else
    alsawidget._notify = naughty.notify ({ preset = preset })
 end
end

-- register the widget through vicious
vicious.register (alsawidget.bar, vicious.widgets.volume, function (widget, args)
			alsawidget._current_level = args[1]
			if args[2] == "♩"
			then
			   alsawidget._muted = true
			   alsawidget.tooltip:set_text (" [Muted] ")
			   widget:set_color (alsawidget.colors.mute)
			   return 100
			end
			alsawidget._muted = false
			alsawidget.tooltip:set_text (" " .. alsawidget.channel .. ": " .. args[1] .. "% ")
			widget:set_color (alsawidget.colors.unmute)
			return args[1]
							     end, 5, alsawidget.channel) -- relatively high update time, use of keys/mouse will force updatei
