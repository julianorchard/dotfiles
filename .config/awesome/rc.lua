-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- This is probably not the play
Awesome, Client, Screen, Tag = awesome, client, screen, tag

-- Standard awesome library
Gears = require("gears")
Awful = require("awful")
require("awful.autofocus")

-- Widget and layout library
Wibox = require("wibox")

-- Theme handling library
Beautiful = require("beautiful")

-- Notification library
Naughty = require("naughty")

-- Declarative object management
Ruled = require("ruled")
Menubar = require("menubar")
Popkeys = require("awful.hotkeys_popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

--------------------------------------------------------------------------------
-- Logging and error handling

function Julog(message, level)
  -- Useful for logging stuff for testing stuff
  local date_string, debug_file

  -- ERROR, WARN, DEBUG, INFO
  if level == nil then level = "DEBUG" end
  date_string = os.date("%c")
  debug_file = io.open(
    Gears.filesystem.get_configuration_dir() .. "awesome.log", "a"
  )
  if debug_file ~= nil then
    debug_file:write(date_string .. " - " .. level .. ": " .. message .. "\n")
    debug_file:close()
  else
    error("Debug file does not exist")
  end
end

-- Log a restart
Julog("Restarting configuration", "INFO")

Naughty.connect_signal("request::display_error", function(message, startup)
  -- Log the error
  Julog(message, "ERROR")

  -- Notify about fallbacking
  Naughty.notification({
    urgency = "critical",
    title   = "Oops, an error happened" .. (startup and " during startup!" or "!"),
    message = message
  })
end)

--------------------------------------------------------------------------------
-- Configuration

-- {{{ Variables
Beautiful.init(Gears.filesystem.get_themes_dir() .. "default/theme.lua")
Term = "alacritty"
Modkey = "Mod4"
-- }}}

-- {{{ Set wallpapers and stuff
require("components/wallpaper")
-- }}}

-- {{{ Menu bar stuffs
Menu = require("components/bar")
-- }}}

-- {{{ Key Mappings
--     NOTE: This uses the Menu global defined above
require("components/keys")
-- }}}

-- {{{ Rules
require("components/rules")
-- }}}

-- {{{ Notifications
require("components/notifications")
-- }}}

-- {{{ Other settings
require("misc/other-settings")
-- }}}
