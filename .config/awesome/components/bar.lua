Julog("Loading bar.lua", "INFO")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- Variables
local editor = os.getenv("EDITOR") or "vim"
local editor_cmd = Term .. " -e " .. editor


-- Widgets ---------------------------------------------------------------------
-- Battery widget
local wi_battery -- Set as either a wibox.widget.textbox OR a battery_widget
local has_batter_widget, battery_widget = pcall(require, "components.battery-widget")
if not has_batter_widget then
  Julog(
    "Could not load battery widget. \
     Perhaps try `git clone https://github.com/deficient/battery-widget.git ~/.config/awesome/battery-widget`.",
    "WARN"
  )
  wi_battery = Wibox.widget.textbox("No battery status")
else
  -- Call wi_battery with args here
  wi_battery = battery_widget({
    alert_threshold = 10
  })
end
-- Textclock widget
local wi_clock = Wibox.widget.textclock()


-- Menu ------------------------------------------------------------------------
local myawesomemenu = {
  { "hotkeys",     function() Popkeys.show_help(nil, Awful.screen.focused()) end },
  { "manual",      Term .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. Awesome.conffile },
  { "restart",     Awesome.restart },
  { "quit",        function() Awesome.quit() end },
}

local menu_awesome = { "awesome", myawesomemenu, Beautiful.awesome_icon }
local menu_terminal = { "open terminal", Term }

local mymainmenu

if has_fdo then
  mymainmenu = freedesktop.menu.build({
    before = { menu_awesome },
    after = { menu_terminal }
  })
else
  mymainmenu = Awful.menu({
    items = {
      menu_awesome,
      { "Debian", debian.menu.Debian_menu.Debian },
      menu_terminal,
    }
  })
end

local mylauncher = Awful.widget.launcher({
  image = Beautiful.awesome_icon,
  menu = mymainmenu
})

-- Menubar configuration
Menubar.utils.terminal = Term


-- Bar stuff, tag, general -----------------------------------------------------
Screen.connect_signal("request::desktop_decoration", function(s)
  -- Each screen has its own tag table.
  Awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, Awful.layout.layouts[1])

  -- Create a promptbox for each screen
  s.mypromptbox = Awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = Awful.widget.layoutbox {
    screen  = s,
    buttons = {
      Awful.button({}, 1, function() Awful.layout.inc(1) end),
      Awful.button({}, 3, function() Awful.layout.inc(-1) end),
      Awful.button({}, 4, function() Awful.layout.inc(-1) end),
      Awful.button({}, 5, function() Awful.layout.inc(1) end),
    }
  }

  -- Create a taglist widget
  s.mytaglist = Awful.widget.taglist {
    screen  = s,
    filter  = Awful.widget.taglist.filter.all,
    buttons = {
      Awful.button({}, 1, function(t) t:view_only() end),
      Awful.button({ Modkey }, 1, function(t)
        if Client.focus then
          Client.focus:move_to_tag(t)
        end
      end),
      Awful.button({}, 3, Awful.tag.viewtoggle),
      Awful.button({ Modkey }, 3, function(t)
        if Client.focus then
          Client.focus:toggle_tag(t)
        end
      end),
      Awful.button({}, 4, function(t) Awful.tag.viewprev(t.screen) end),
      Awful.button({}, 5, function(t) Awful.tag.viewnext(t.screen) end),
    }
  }

  -- Create a tasklist widget
  s.mytasklist = Awful.widget.tasklist {
    screen  = s,
    filter  = Awful.widget.tasklist.filter.currenttags,
    buttons = {
      Awful.button({}, 1, function(c)
        c:activate { context = "tasklist", action = "toggle_minimization" }
      end),
      Awful.button({}, 3, function() Awful.menu.client_list { theme = { width = 250 } } end),
      Awful.button({}, 4, function() Awful.client.focus.byidx(-1) end),
      Awful.button({}, 5, function() Awful.client.focus.byidx(1) end),
    }
  }

  -- Create the wibox
  s.mywibox = Awful.wibar {
    position = "bottom",
    screen   = s,
    widget   = {
      layout = Wibox.layout.align.horizontal,
      -- Left
      {
        layout = Wibox.layout.fixed.horizontal,
        mylauncher,
        s.mytaglist,
        s.mypromptbox,
      },
      -- Middle
      s.mytasklist,
      -- Right
      {
        layout = Wibox.layout.fixed.horizontal,
        Wibox.widget.systray(),
        wi_battery,
        wi_clock,
        s.mylayoutbox,
      },
    }
  }
end)

Awful.mouse.append_global_mousebindings({
  Awful.button({}, 3, function() mymainmenu:toggle() end),
  Awful.button({}, 4, Awful.tag.viewprev),
  Awful.button({}, 5, Awful.tag.viewnext),
})


-- Layouts ---------------------------------------------------------------------
Tag.connect_signal("request::default_layouts", function()
  -- Awful.layout.append_default_layouts is also an option here
  Awful.layout.layouts = {
    Awful.layout.suit.tile,
    Awful.layout.suit.tile.top,
    Awful.layout.suit.spiral,
    Awful.layout.suit.max,
    Awful.layout.suit.floating,
  }
end)


-- Titlebars -------------------------------------------------------------------
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

  Awful.titlebar(c, { size = 20 }).widget = {
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
    layout = Wibox.layout.align.horizontal,
  }
end)


-- This is all we need to return... for now!
return mymainmenu
