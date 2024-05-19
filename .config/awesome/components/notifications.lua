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
