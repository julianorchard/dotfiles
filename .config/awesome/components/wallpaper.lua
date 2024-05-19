local helpers = require("helpers/helpers")

-- From the default config
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

-- NOTE: This function exists because it means we re-check how many screens
--       there are each time meaning no weirdness with indexing a monitor that
--       doesn't exist etc.
local function set_wallpaper_on_each_screen()
  for s = 1, screen.count() do
    helpers.set_wallpaper(s, "maximized")
  end
end
-- Set wallpapers for each screen and also if there's geometry changes!
set_wallpaper_on_each_screen()
-- screen.connect_signal("property::geometry", set_wallpaper_on_each_screen())
