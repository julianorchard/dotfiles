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
