local awful = require("awful")
local menubar = require("menubar")
local popkeys = require("awful.hotkeys_popup")
require("awful.autofocus")
require("awful.hotkeys_popup.keys")

awful.keyboard.append_global_keybindings({
  awful.key(
    { Modkey },
    "s",
    popkeys.show_help,
    { description = "show help", group = "awesome" }
  ),
  awful.key({ Modkey }, "w", function()
    Menu:show()
  end, { description = "show main menu", group = "awesome" }),
  awful.key(
    { Modkey, "Shift" },
    "r",
    awesome.restart,
    { description = "reload awesome", group = "awesome" }
  ),
  awful.key(
    { Modkey, "Shift" },
    "q",
    awesome.quit,
    { description = "quit awesome", group = "awesome" }
  ),
  awful.key({ Modkey }, "x", function()
    awful.prompt.run({
      prompt = "Run Lua code: ",
      textbox = awful.screen.focused().mypromptbox.widget,
      exe_callback = awful.util.eval,
      history_path = awful.util.get_cache_dir() .. "/history_eval",
    })
  end, { description = "lua execute prompt", group = "awesome" }),
  awful.key({ Modkey }, "Return", function()
    awful.spawn(Term)
  end, { description = "open a terminal", group = "launcher" }),
  awful.key({ Modkey }, "d", function()
    awful.screen.focused().mypromptbox:run()
  end, { description = "run prompt", group = "launcher" }),
  awful.key({ Modkey }, "p", function()
    menubar.show()
  end, { description = "show the menubar", group = "launcher" }),

  -- Volume keys
  awful.key({}, "XF86AudioRaiseVolume", function()
    awful.util.spawn("pactl set-sink-volume @DEFAULT_SINK@ +10%", false)
  end),
  awful.key({}, "XF86AudioLowerVolume", function()
    awful.util.spawn("pactl set-sink-volume @DEFAULT_SINK@ -10%", false)
  end),
  awful.key({}, "XF86AudioMute", function()
    awful.util.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle", false)
  end),

  -- Screenshot (flameshot)
  awful.key({}, "Print", function()
    awful.util.spawn(
      "flameshot gui -p " .. os.getenv("HOME") .. "/Pictures/captures",
      false
    )
  end),

  -- Brightness
  awful.key({}, "XF86MonBrightnessDown", function()
    awful.util.spawn("light -U 5", false)
  end),
  awful.key({}, "XF86MonBrightnessUp", function()
    awful.util.spawn("light -A 5", false)
  end),
  awful.key({ "Ctrl" }, "XF86MonBrightnessDown", function()
    awful.util.spawn("light -S 2", false)
  end),
  awful.key({ "Ctrl" }, "XF86MonBrightnessUp", function()
    awful.util.spawn("light -S 100", false)
  end),

  -- Lockscrin
  awful.key({ "Ctrl", "Shift", Modkey }, "l", function()
    awful.util.spawn("i3lock-fancy", false)
  end),
})

-- Tags related keybindings
awful.keyboard.append_global_keybindings({
  awful.key(
    { Modkey },
    "Left",
    awful.tag.viewprev,
    { description = "view previous", group = "tag" }
  ),
  awful.key(
    { Modkey },
    "Right",
    awful.tag.viewnext,
    { description = "view next", group = "tag" }
  ),
  awful.key(
    { Modkey },
    "Escape",
    awful.tag.history.restore,
    { description = "go back", group = "tag" }
  ),
})

-- Focus related keybindings
awful.keyboard.append_global_keybindings({
  awful.key({ Modkey }, "j", function()
    awful.client.focus.byidx(1)
  end, { description = "focus next by index", group = "client" }),
  awful.key({ Modkey }, "k", function()
    awful.client.focus.byidx(-1)
  end, { description = "focus previous by index", group = "client" }),
  awful.key({ Modkey }, "Tab", function()
    awful.client.focus.history.previous()
    if client.focus then
      client.focus:raise()
    end
  end, { description = "go back", group = "client" }),
  awful.key({ Modkey, "Control" }, "j", function()
    awful.screen.focus_relative(1)
  end, { description = "focus the next screen", group = "screen" }),
  awful.key({ Modkey }, "o", function()
    awful.screen.focus_relative(-1)
  end, { description = "focus the previous screen", group = "screen" }),
  awful.key({ Modkey, "Control" }, "n", function()
    local c = awful.client.restore()
    -- Focus restored client
    if c then
      c:activate({ raise = true, context = "key.unminimize" })
    end
  end, { description = "restore minimized", group = "client" }),
})

-- Layout related keybindings
awful.keyboard.append_global_keybindings({
  awful.key({ Modkey, "Shift" }, "j", function()
    awful.client.swap.byidx(1)
  end, { description = "swap with next client by index", group = "client" }),
  awful.key({ Modkey, "Shift" }, "k", function()
    awful.client.swap.byidx(-1)
  end, { description = "swap with previous client by index", group = "client" }),
  awful.key(
    { Modkey },
    "u",
    awful.client.urgent.jumpto,
    { description = "jump to urgent client", group = "client" }
  ),
  awful.key({ Modkey }, "l", function()
    awful.tag.incmwfact(0.05)
  end, { description = "increase master width factor", group = "layout" }),
  awful.key({ Modkey }, "h", function()
    awful.tag.incmwfact(-0.05)
  end, { description = "decrease master width factor", group = "layout" }),
  awful.key(
    { Modkey, "Shift" },
    "h",
    function()
      awful.tag.incnmaster(1, nil, true)
    end,
    { description = "increase the number of master clients", group = "layout" }
  ),
  awful.key(
    { Modkey, "Shift" },
    "l",
    function()
      awful.tag.incnmaster(-1, nil, true)
    end,
    { description = "decrease the number of master clients", group = "layout" }
  ),
  awful.key({ Modkey, "Control" }, "h", function()
    awful.tag.incncol(1, nil, true)
  end, { description = "increase the number of columns", group = "layout" }),
  awful.key({ Modkey, "Control" }, "l", function()
    awful.tag.incncol(-1, nil, true)
  end, { description = "decrease the number of columns", group = "layout" }),
  awful.key({ Modkey }, "space", function()
    awful.layout.inc(1)
  end, { description = "select next", group = "layout" }),
  awful.key({ Modkey, "Shift" }, "space", function()
    awful.layout.inc(-1)
  end, { description = "select previous", group = "layout" }),

  awful.key({ Modkey, "Shift", "Control" }, "k", function()
    awful.util.spawn("pkill awesome", false)
  end, { description = "kill sessions hard", group = "util" }),
})

awful.keyboard.append_global_keybindings({
  awful.key({
    modifiers = { Modkey },
    keygroup = "numrow",
    description = "only view tag",
    group = "tag",
    on_press = function(index)
      Screen = awful.screen.focused()
      Tag = Screen.tags[index]
      if Tag then
        Tag:view_only()
      end
    end,
  }),
  awful.key({
    modifiers = { Modkey, "Control" },
    keygroup = "numrow",
    description = "toggle tag",
    group = "tag",
    on_press = function(index)
      Screen = awful.screen.focused()
      Tag = Screen.tags[index]
      if Tag then
        awful.tag.viewtoggle(Tag)
      end
    end,
  }),
  awful.key({
    modifiers = { Modkey, "Shift" },
    keygroup = "numrow",
    description = "move focused client to tag",
    group = "tag",
    on_press = function(index)
      if client.focus then
        Tag = client.focus.screen.tags[index]
        if Tag then
          client.focus:move_to_tag(Tag)
        end
      end
    end,
  }),
  awful.key({
    modifiers = { Modkey, "Control", "Shift" },
    keygroup = "numrow",
    description = "toggle focused client on tag",
    group = "tag",
    on_press = function(index)
      if client.focus then
        Tag = client.focus.screen.tags[index]
        if Tag then
          client.focus:toggle_tag(Tag)
        end
      end
    end,
  }),
  awful.key({
    modifiers = { Modkey },
    keygroup = "numpad",
    description = "select layout directly",
    group = "layout",
    on_press = function(index)
      local t = awful.screen.focused().selected_tag
      if t then
        t.layout = t.layouts[index] or t.layout
      end
    end,
  }),
})

client.connect_signal("request::default_mousebindings", function()
  awful.mouse.append_client_mousebindings({
    awful.button({}, 1, function(c)
      c:activate({ context = "mouse_click" })
    end),
    awful.button({ Modkey }, 1, function(c)
      c:activate({ context = "mouse_click", action = "mouse_move" })
    end),
    awful.button({ Modkey }, 3, function(c)
      c:activate({ context = "mouse_click", action = "mouse_resize" })
    end),
  })
end)

client.connect_signal("request::default_keybindings", function()
  awful.keyboard.append_client_keybindings({
    awful.key({ Modkey }, "f", function(c)
      c.fullscreen = not c.fullscreen
      c:raise()
    end, { description = "toggle fullscreen", group = "client" }),
    awful.key({ Modkey }, "q", function(c)
      c:kill()
    end, { description = "close", group = "client" }),
    awful.key(
      { Modkey, "Control" },
      "space",
      awful.client.floating.toggle,
      { description = "toggle floating", group = "client" }
    ),
    awful.key({ Modkey, "Control" }, "Return", function(c)
      c:swap(awful.client.getmaster())
    end, { description = "move to master", group = "client" }),
    awful.key({ Modkey, "Shift" }, "o", function(c)
      c:move_to_screen()
    end, { description = "move to screen", group = "client" }),
    awful.key({ Modkey }, "t", function(c)
      c.ontop = not c.ontop
    end, { description = "toggle keep on top", group = "client" }),
    awful.key({ Modkey }, "n", function(c)
      -- The client currently has the input focus, so it cannot be
      -- minimized, since minimized clients can't have the focus.
      c.minimized = true
    end, { description = "minimize", group = "client" }),
    awful.key({ Modkey }, "m", function(c)
      c.maximized = not c.maximized
      c:raise()
    end, { description = "(un)maximize", group = "client" }),
    awful.key({ Modkey, "Control" }, "m", function(c)
      c.maximized_vertical = not c.maximized_vertical
      c:raise()
    end, { description = "(un)maximize vertically", group = "client" }),
    awful.key({ Modkey, "Shift" }, "m", function(c)
      c.maximized_horizontal = not c.maximized_horizontal
      c:raise()
    end, { description = "(un)maximize horizontally", group = "client" }),
  })
end)
