local awful = require("awful")
local beautiful = require("beautiful")
local logging = require("helpers.logging")
local menubar = require("menubar")
local popkeys = require("awful.hotkeys_popup")
local wibox = require("wibox")

logging.julog("Loading bar.lua", "INFO")

require("awful.autofocus")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Load Debian menu entries
local debian = require("debian.menu")
local has_fdo, freedesktop = pcall(require, "freedesktop")

-- Variables
local editor = os.getenv("EDITOR") or "vim"
local editor_cmd = Term .. " -e " .. editor

-- Widgets ---------------------------------------------------------------------
-- Battery widget
local wi_battery -- Set as either a wibox.widget.textbox OR a battery_widget
local has_batter_widget, battery_widget =
  pcall(require, "components.battery-widget")
if not has_batter_widget then
  logging.julog(
    "Could not load battery widget. \
     Perhaps try `git clone https://github.com/deficient/battery-widget.git ~/.config/awesome/battery-widget`.",
    "WARN"
  )
  wi_battery = wibox.widget.textbox("No battery status")
else
  -- Call wi_battery with args here
  wi_battery = battery_widget({
    alert_threshold = 10,
  })
end
-- Textclock widget
local wi_clock = wibox.widget.textclock()

-- Menu ------------------------------------------------------------------------
local myawesomemenu = {
  {
    "hotkeys",
    function()
      popkeys.show_help(nil, awful.screen.focused())
    end,
  },
  { "manual", Term .. " -e man awesome" },
  { "edit config", editor_cmd .. " " .. awesome.conffile },
  { "restart", awesome.restart },
  {
    "quit",
    function()
      awesome.quit()
    end,
  },
}

local menu_awesome = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", Term }

local mymainmenu

if has_fdo then
  mymainmenu = freedesktop.menu.build({
    before = { menu_awesome },
    after = { menu_terminal },
  })
else
  mymainmenu = awful.menu({
    items = {
      menu_awesome,
      { "Debian", debian.menu.Debian_menu.Debian },
      menu_terminal,
    },
  })
end

local mylauncher = awful.widget.launcher({
  image = beautiful.awesome_icon,
  menu = mymainmenu,
})

-- Menubar configuration
menubar.utils.terminal = Term

-- Layouts ---------------------------------------------------------------------
-- layout.append_default_layouts is also an option here
awful.layout.layouts = {
  awful.layout.suit.tile,
  awful.layout.suit.tile.top,
  awful.layout.suit.spiral,
  awful.layout.suit.max,
  awful.layout.suit.floating,
}

-- Bar stuff, tag, general -----------------------------------------------------
screen.connect_signal("request::desktop_decoration", function(s)
  -- Each screen has its own tag table.
  awful.tag(
    { "一", "二", "三", "四", "五", "六", "七", "八", "九" },
    s,
    awful.layout.layouts[1]
  )

  -- Create a promptbox for each screen
  s.mypromptbox = awful.widget.prompt()

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox({
    screen = s,
    buttons = {
      awful.button({}, 1, function()
        awful.layout.inc(1)
      end),
      awful.button({}, 3, function()
        awful.layout.inc(-1)
      end),
      awful.button({}, 4, function()
        awful.layout.inc(-1)
      end),
      awful.button({}, 5, function()
        awful.layout.inc(1)
      end),
    },
  })

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist({
    screen = s,
    filter = awful.widget.taglist.filter.all,
    buttons = {
      awful.button({}, 1, function(t)
        t:view_only()
      end),
      awful.button({ Modkey }, 1, function(t)
        if client.focus then
          client.focus:move_to_tag(t)
        end
      end),
      awful.button({}, 3, awful.tag.viewtoggle),
      awful.button({ Modkey }, 3, function(t)
        if client.focus then
          client.focus:toggle_tag(t)
        end
      end),
      awful.button({}, 4, function(t)
        awful.tag.viewprev(t.screen)
      end),
      awful.button({}, 5, function(t)
        awful.tag.viewnext(t.screen)
      end),
    },
  })

  -- Create a tasklist widget
  s.mytasklist = awful.widget.tasklist({
    screen = s,
    filter = awful.widget.tasklist.filter.currenttags,
    layout = wibox.layout.fixed.horizontal(),
    -- TODO: Make it so that when there are a certain number of things open,
    --       we do the flex instead
    widget_template = function()
      return {
        {
          {
            {
              {
                widget = awful.widget.clienticon,
              },
              margins = 6,
              widget = wibox.container.margin,
            },
            {
              id = "text_role",
              widget = wibox.widget.textbox,
            },
            layout = wibox.layout.fixed.horizontal,
          },
          left = 10,
          right = 10,
          widget = wibox.container.margin,
        },
        forced_width = 250,
        id = "background_role",
        widget = wibox.container.background,
      }
    end,
    buttons = {
      awful.button({}, 1, function(c)
        c:activate({ context = "tasklist", action = "toggle_minimization" })
      end),
      awful.button({}, 3, function()
        awful.menu.client_list({ theme = { width = 250 } })
      end),
      awful.button({}, 4, function()
        awful.client.focus.byidx(-1)
      end),
      awful.button({}, 5, function()
        awful.client.focus.byidx(1)
      end),
    },
  })

  -- Create the wibox
  s.mywibox = awful.wibar({
    position = "bottom",
    screen = s,
    widget = {
      layout = wibox.layout.align.horizontal,
      -- Left
      {
        layout = wibox.layout.fixed.horizontal,
        mylauncher,
        s.mytaglist,
        s.mypromptbox,
      },
      -- Middle
      s.mytasklist,
      -- Right
      {
        layout = wibox.layout.fixed.horizontal,
        wibox.widget.systray(),
        wi_battery,
        wi_clock,
        s.mylayoutbox,
      },
    },
  })
end)

awful.mouse.append_global_mousebindings({
  awful.button({}, 3, function()
    mymainmenu:toggle()
  end),
  awful.button({}, 4, awful.tag.viewprev),
  awful.button({}, 5, awful.tag.viewnext),
})

-- Titlebars -------------------------------------------------------------------
client.connect_signal("request::titlebars", function(c)
  -- buttons for the titlebar
  local buttons = {
    awful.button({}, 1, function()
      c:activate({ context = "titlebar", action = "mouse_move" })
    end),
    awful.button({}, 3, function()
      c:activate({ context = "titlebar", action = "mouse_resize" })
    end),
  }

  awful.titlebar(c, { size = 20 }).widget = {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = buttons,
      layout = wibox.layout.fixed.horizontal,
    },
    { -- Middle
      { -- Title
        halign = "center",
        widget = awful.titlebar.widget.titlewidget(c),
      },
      buttons = buttons,
      layout = wibox.layout.fixed.horizontal,
    },
    { -- Right
      awful.titlebar.widget.floatingbutton(c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.stickybutton(c),
      awful.titlebar.widget.ontopbutton(c),
      awful.titlebar.widget.closebutton(c),
      layout = wibox.layout.fixed.horizontal(),
    },
    layout = wibox.layout.align.horizontal,
  }
end)

-- This is all we need to return... for now!
return mymainmenu
