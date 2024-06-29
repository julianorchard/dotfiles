-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- This is probably not the play
-- Awesome, Client, Screen, Tag = awesome, client, screen, tag

local logging = require("helpers.logging")

-- Standard awesome library
local gears = require("gears")
require("awful.autofocus")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Log a restart
logging.julog("Restarting configuration", "INFO")

naughty.connect_signal("request::display_error", function(message, startup)
  -- Log the error
  logging.julog(message, "ERROR")

  -- Notify about fallbacking
  naughty.notification({
    urgency = "critical",
    title = "Oops, an error happened"
      .. (startup and " during startup!" or "!"),
    message = message,
  })
end)

--------------------------------------------------------------------------------
-- Configuration

-- {{{ Variables
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.font = "ComicShannsMono Nerd Font Mono 9"
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
