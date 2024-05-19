Awful.keyboard.append_global_keybindings({
  Awful.key({ Modkey, }, "s", Popkeys.show_help,
    { description = "show help", group = "awesome" }),
  Awful.key({ Modkey, }, "w", function() Menu:show() end,
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
