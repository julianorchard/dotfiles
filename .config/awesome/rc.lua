-- awesome_mode: api-level=4:screen=on
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Prevents errors in the rest of the file
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
Hotkeys_popup = require("awful.hotkeys_popup")

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

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
Beautiful.init(Gears.filesystem.get_themes_dir() .. "default/theme.lua")
Term = "alacritty"
Modkey = "Mod4"
-- }}}


-- {{{ Wallpaper
Screen.connect_signal("request::wallpaper", function(s)
  Awful.wallpaper {
    screen = s,
    widget = {
      {
        image     = Beautiful.wallpaper,
        upscale   = true,
        downscale = true,
        widget    = Wibox.widget.imagebox,
      },
      valign = "center",
      halign = "center",
      tiled  = false,
      widget = Wibox.container.tile,
    }
  }
end)
-- }}}

-- {{{ Menu bar stuffs
local main_menu = require("bar")
-- }}}

-- General Awesome keys
Awful.keyboard.append_global_keybindings({
  Awful.key({ Modkey, }, "s", Hotkeys_popup.show_help,
    { description = "show help", group = "awesome" }),
  Awful.key({ Modkey, }, "w", function() main_menu:show() end,
    { description = "show main menu", group = "awesome" }),
  Awful.key({ Modkey, "Shift" }, "r", Awesome.restart,
    { description = "reload awesome", group = "awesome" }),
  Awful.key({ Modkey, "Shift" }, "q", Awesome.quit,
    { description = "quit awesome", group = "awesome" }),
  Awful.key({ Modkey }, "x",
    function()
      Awful.prompt.run {
        prompt       = "Run Lua code: ",
        textbox      = Awful.screen.focused().mypromptbox.widget,
        exe_callback = Awful.util.eval,
        history_path = Awful.util.get_cache_dir() .. "/history_eval"
      }
    end,
    { description = "lua execute prompt", group = "awesome" }),
  Awful.key({ Modkey, }, "Return", function() Awful.spawn(Term) end,
    { description = "open a terminal", group = "launcher" }),
  Awful.key({ Modkey }, "d", function() Awful.screen.focused().mypromptbox:run() end,
    { description = "run prompt", group = "launcher" }),
  Awful.key({ Modkey }, "p", function() Menubar.show() end,
    { description = "show the menubar", group = "launcher" }),

  -- Volume keys
  Awful.key({}, "XF86AudioRaiseVolume", function()
    Awful.util.spawn("pactl set-sink-volume @DEFAULT_SINK@ +10%", false)
  end),
  Awful.key({}, "XF86AudioLowerVolume", function()
    Awful.util.spawn("pactl set-sink-volume @DEFAULT_SINK@ -10%", false)
  end),
  Awful.key({}, "XF86AudioMute", function()
    Awful.util.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
  end),

  -- Screenshot (flameshot)
  Awful.key({}, "Print", function()
    Awful.util.spawn("flameshot gui -p " .. os.getenv("HOME") .. "/Pictures/captures", false)
  end),

  -- Brightness
  Awful.key({}, "XF86MonBrightnessDown", function()
    Awful.util.spawn("light -U 5", false)
  end),
  Awful.key({}, "XF86MonBrightnessUp", function()
    Awful.util.spawn("light -A 5", false)
  end),
  Awful.key({ "Ctrl" }, "XF86MonBrightnessDown", function()
    Awful.util.spawn("light -S 2", false)
  end),
  Awful.key({ "Ctrl" }, "XF86MonBrightnessUp", function()
    Awful.util.spawn("light -S 100", false)
  end),
})

-- Tags related keybindings
Awful.keyboard.append_global_keybindings({
  Awful.key({ Modkey, }, "Left", Awful.tag.viewprev,
    { description = "view previous", group = "tag" }),
  Awful.key({ Modkey, }, "Right", Awful.tag.viewnext,
    { description = "view next", group = "tag" }),
  Awful.key({ Modkey, }, "Escape", Awful.tag.history.restore,
    { description = "go back", group = "tag" }),
})

-- Focus related keybindings
Awful.keyboard.append_global_keybindings({
  Awful.key({ Modkey, }, "j",
    function()
      Awful.client.focus.byidx(1)
    end,
    { description = "focus next by index", group = "client" }
  ),
  Awful.key({ Modkey, }, "k",
    function()
      Awful.client.focus.byidx(-1)
    end,
    { description = "focus previous by index", group = "client" }
  ),
  Awful.key({ Modkey, }, "Tab",
    function()
      Awful.client.focus.history.previous()
      if Client.focus then
        Client.focus:raise()
      end
    end,
    { description = "go back", group = "client" }),
  Awful.key({ Modkey, "Control" }, "j", function() Awful.screen.focus_relative(1) end,
    { description = "focus the next screen", group = "screen" }),
  Awful.key({ Modkey }, "o", function() Awful.screen.focus_relative(-1) end,
    { description = "focus the previous screen", group = "screen" }),
  Awful.key({ Modkey, "Control" }, "n",
    function()
      local c = Awful.client.restore()
      -- Focus restored client
      if c then
        c:activate { raise = true, context = "key.unminimize" }
      end
    end,
    { description = "restore minimized", group = "client" }),
})

-- Layout related keybindings
Awful.keyboard.append_global_keybindings({
  Awful.key({ Modkey, "Shift" }, "j", function() Awful.client.swap.byidx(1) end,
    { description = "swap with next client by index", group = "client" }),
  Awful.key({ Modkey, "Shift" }, "k", function() Awful.client.swap.byidx(-1) end,
    { description = "swap with previous client by index", group = "client" }),
  Awful.key({ Modkey, }, "u", Awful.client.urgent.jumpto,
    { description = "jump to urgent client", group = "client" }),
  Awful.key({ Modkey, }, "l", function() Awful.tag.incmwfact(0.05) end,
    { description = "increase master width factor", group = "layout" }),
  Awful.key({ Modkey, }, "h", function() Awful.tag.incmwfact(-0.05) end,
    { description = "decrease master width factor", group = "layout" }),
  Awful.key({ Modkey, "Shift" }, "h", function() Awful.tag.incnmaster(1, nil, true) end,
    { description = "increase the number of master clients", group = "layout" }),
  Awful.key({ Modkey, "Shift" }, "l", function() Awful.tag.incnmaster(-1, nil, true) end,
    { description = "decrease the number of master clients", group = "layout" }),
  Awful.key({ Modkey, "Control" }, "h", function() Awful.tag.incncol(1, nil, true) end,
    { description = "increase the number of columns", group = "layout" }),
  Awful.key({ Modkey, "Control" }, "l", function() Awful.tag.incncol(-1, nil, true) end,
    { description = "decrease the number of columns", group = "layout" }),
  Awful.key({ Modkey, }, "space", function() Awful.layout.inc(1) end,
    { description = "select next", group = "layout" }),
  Awful.key({ Modkey, "Shift" }, "space", function() Awful.layout.inc(-1) end,
    { description = "select previous", group = "layout" }),
})


Awful.keyboard.append_global_keybindings({
  Awful.key {
    modifiers   = { Modkey },
    keygroup    = "numrow",
    description = "only view tag",
    group       = "tag",
    on_press    = function(index)
      Screen = Awful.screen.focused()
      Tag = Screen.tags[index]
      if Tag then
        Tag:view_only()
      end
    end,
  },
  Awful.key {
    modifiers   = { Modkey, "Control" },
    keygroup    = "numrow",
    description = "toggle tag",
    group       = "tag",
    on_press    = function(index)
      Screen = Awful.screen.focused()
      Tag = Screen.tags[index]
      if Tag then
        Awful.tag.viewtoggle(Tag)
      end
    end,
  },
  Awful.key {
    modifiers   = { Modkey, "Shift" },
    keygroup    = "numrow",
    description = "move focused client to tag",
    group       = "tag",
    on_press    = function(index)
      if Client.focus then
        Tag = Client.focus.screen.tags[index]
        if Tag then
          Client.focus:move_to_tag(Tag)
        end
      end
    end,
  },
  Awful.key {
    modifiers   = { Modkey, "Control", "Shift" },
    keygroup    = "numrow",
    description = "toggle focused client on tag",
    group       = "tag",
    on_press    = function(index)
      if Client.focus then
        Tag = Client.focus.screen.tags[index]
        if Tag then
          Client.focus:toggle_tag(Tag)
        end
      end
    end,
  },
  Awful.key {
    modifiers   = { Modkey },
    keygroup    = "numpad",
    description = "select layout directly",
    group       = "layout",
    on_press    = function(index)
      local t = Awful.screen.focused().selected_tag
      if t then
        t.layout = t.layouts[index] or t.layout
      end
    end,
  }
})

Client.connect_signal("request::default_mousebindings", function()
  Awful.mouse.append_client_mousebindings({
    Awful.button({}, 1, function(c)
      c:activate { context = "mouse_click" }
    end),
    Awful.button({ Modkey }, 1, function(c)
      c:activate { context = "mouse_click", action = "mouse_move" }
    end),
    Awful.button({ Modkey }, 3, function(c)
      c:activate { context = "mouse_click", action = "mouse_resize" }
    end),
  })
end)

Client.connect_signal("request::default_keybindings", function()
  Awful.keyboard.append_client_keybindings({
    Awful.key({ Modkey, }, "f",
      function(c)
        c.fullscreen = not c.fullscreen
        c:raise()
      end,
      { description = "toggle fullscreen", group = "client" }),
    Awful.key({ Modkey }, "q", function(c) c:kill() end,
      { description = "close", group = "client" }),
    Awful.key({ Modkey, "Control" }, "space", Awful.client.floating.toggle,
      { description = "toggle floating", group = "client" }),
    Awful.key({ Modkey, "Control" }, "Return", function(c) c:swap(Awful.client.getmaster()) end,
      { description = "move to master", group = "client" }),
    Awful.key({ Modkey, "Shift" }, "o", function(c) c:move_to_screen() end,
      { description = "move to screen", group = "client" }),
    Awful.key({ Modkey, }, "t", function(c) c.ontop = not c.ontop end,
      { description = "toggle keep on top", group = "client" }),
    Awful.key({ Modkey, }, "n",
      function(c)
        -- The client currently has the input focus, so it cannot be
        -- minimized, since minimized clients can't have the focus.
        c.minimized = true
      end,
      { description = "minimize", group = "client" }),
    Awful.key({ Modkey, }, "m",
      function(c)
        c.maximized = not c.maximized
        c:raise()
      end,
      { description = "(un)maximize", group = "client" }),
    Awful.key({ Modkey, "Control" }, "m",
      function(c)
        c.maximized_vertical = not c.maximized_vertical
        c:raise()
      end,
      { description = "(un)maximize vertically", group = "client" }),
    Awful.key({ Modkey, "Shift" }, "m",
      function(c)
        c.maximized_horizontal = not c.maximized_horizontal
        c:raise()
      end,
      { description = "(un)maximize horizontally", group = "client" }),
  })
end)

-- }}}

-- {{{ Rules
-- Rules to apply to new clients.
Ruled.client.connect_signal("request::rules", function()
  -- All clients will match this rule.
  Ruled.client.append_rule {
    id         = "global",
    rule       = {},
    properties = {
      focus     = Awful.client.focus.filter,
      raise     = true,
      screen    = Awful.screen.preferred,
      placement = Awful.placement.no_overlap + Awful.placement.no_offscreen,
      floating  = false,
    }
  }

  -- Floating clients.
  Ruled.client.append_rule {
    id         = "floating",
    rule_any   = {
      instance = { "copyq", "pinentry" },
      class    = {
        "Arandr", "Nautilus",
      },
      -- Note that the name property shown in xprop might be set slightly after creation of the client
      -- and the name shown there might not match defined rules here.
      name     = {
        "Event Tester", -- xev.
      },
      role     = {
        "AlarmWindow",   -- Thunderbird's calendar.
        "ConfigManager", -- Thunderbird's about:config.
        "pop-up",        -- e.g. Google Chrome's (detached) Developer Tools.
      }
    },
    properties = { floating = true }
  }

  -- Add titlebars to normal clients and dialogs
  Ruled.client.append_rule {
    id         = "titlebars",
    rule_any   = { type = { "normal", "dialog" } },
    properties = { titlebars_enabled = true }
  }

  -- Set Firefox to always map on the tag named "2" on screen 1.
  Ruled.client.append_rule {
    rule       = { class = "Firefox" },
    properties = { floating = false, screen = 1, tag = "2" }
  }
end)
-- }}}

-- {{{ Titlebars
-- Add a titlebar if titlebars_enabled is set to true in the rules.
Client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    Awful.button({}, 1, function()
      c:activate { context = "titlebar", action = "mouse_move" }
    end),
    Awful.button({}, 3, function()
      c:activate { context = "titlebar", action = "mouse_resize" }
    end),
  }

  Awful.titlebar(c).widget = {
    { -- Left
      Awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout  = Wibox.layout.fixed.horizontal
    },
    {   -- Middle
      { -- Title
        halign = "center",
        widget = Awful.titlebar.widget.titlewidget(c)
      },
      buttons = buttons,
      layout  = Wibox.layout.flex.horizontal
    },
    { -- Right
      Awful.titlebar.widget.floatingbutton(c),
      Awful.titlebar.widget.maximizedbutton(c),
      Awful.titlebar.widget.stickybutton(c),
      Awful.titlebar.widget.ontopbutton(c),
      Awful.titlebar.widget.closebutton(c),
      layout = Wibox.layout.fixed.horizontal()
    },
    layout = Wibox.layout.align.horizontal
  }
end)
-- }}}

-- {{{ Notifications

Ruled.notification.connect_signal('request::rules', function()
  -- All notifications will match this rule.
  Ruled.notification.append_rule {
    rule       = {},
    properties = {
      screen           = Awful.screen.preferred,
      implicit_timeout = 5,
    }
  }
end)

Naughty.connect_signal("request::display", function(n)
  Naughty.layout.box { notification = n }
end)

-- }}}

-- Enable sloppy focus, so that focus follows mouse
Client.connect_signal("mouse::enter", function(c)
  c:activate { context = "mouse_enter", raise = false }
end)

-- Reload the configuration when geometry change
Screen.connect_signal("property::geometry", Awesome.restart)

-- TESTING ---------------------------------------------------------------------

-- Capture executable output, https://stackoverflow.com/a/326715
-- @param cmd String, the command to execute
-- @param raw Bool, return without modifiying output nicely
function os.capture(cmd, raw)
  local f = assert(io.popen(cmd, "r"))
  local s = assert(f:read("*a"))
  f:close()
  if raw then return s end
  s = string.gsub(s, "^%s+", "")
  s = string.gsub(s, "%s+$", "")
  s = string.gsub(s, "[\n\r]+", " ")
  return s
end

-- Find out whether an exe is available and in the PATH
-- @param program String of the application to check for
function os.executable(program)
  local installed = os.capture(
    "! command -v "
    .. program
    .. " &> /dev/null && echo 'n'"
  )
  if installed == "n" then
    Julog(
      "Executable "
      .. program
      .. " is not installed/in the PATH",
      "WARN"
    )
    return 1
  else
    return 0
  end
end

-- Wrapper around the `find` command
-- @param search String to search for, wrapped in double quotes to allow for wildcarding
-- @param path String input path to search within
-- @param depth Integer representing the max depth the command should search
function os.find(search, path, depth)
  -- Check find is available
  if os.executable("find") == 1 then
    local err_msg = "`find` command not available"
    Julog(err_msg, "ERROR")
    return err_msg
  end

  -- Handle unspecified maxdepth
  local depth_cmd = nil
  if depth ~= nil then
    depth_cmd = " -maxdepth " .. depth .. " "
  end

  return os.capture("find " .. path .. depth_cmd .. " -name \"" .. search .. "\"")
end

local function random_wallpaper(dir)
  local pape_list_raw = os.find("*.jpg", dir, 1)
  local pape_list = {}
  for pape in string.gmatch(pape_list_raw, "[^%s]+") do
    table.insert(pape_list, pape)
  end

  local random_pape = pape_list[math.random(#pape_list)]
  Julog("Setting wallpaper " .. random_pape, "INFO")
  return random_pape
end

local papedir = os.getenv("HOME") .. "/Pictures/normie-wallpapers/"
Gears.wallpaper.fit(random_wallpaper(papedir), nil, "#1E1E2E")
