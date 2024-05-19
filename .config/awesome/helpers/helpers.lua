-- These helper functions make use of even more helper functions
require("helpers/os_extra")

local helpers = {}

---random_wallpaper
---Return a random wallpaper using `find` (os.find)
---@param dir string The directory to search within
helpers.random_wallpaper = function(dir)
  local pape_list_raw = os.find("*.jpg", dir, 1)
  local pape_list = {}
  for pape in string.gmatch(pape_list_raw, "[^%s]+") do
    table.insert(pape_list, pape)
  end

  local random_pape = pape_list[math.random(#pape_list)]
  return random_pape
end

---set_wallpaper
---Set a wallpaper using Gears.wallpaper and random_wallpaper()
---@param screen integer The number of the screen to set the paper on
---@param pos? string Type of wallpaper positioning (defaults to fit)
helpers.set_wallpaper = function(screen, pos)
  if pos ~= "maximized" and
      pos ~= "centered" and
      pos ~= "fit" and
      pos ~= nil then
    Julog("Wallpaper position \"" .. pos
      .. "\" is not recognised. Defaulting to 'fit' position.", "INFO")
  end

  local papedir = os.getenv("HOME") .. "/Pictures/normie-wallpapers/"
  local wallpaper = helpers.random_wallpaper(papedir)

  if pos == "maximized" then
    Gears.wallpaper.maximized(wallpaper, screen)
  elseif pos == "centered" then
    Gears.wallpaper.centered(wallpaper, screen, "#1E1E2E")
  else
    Gears.wallpaper.fit(wallpaper, screen, "#1E1E2E")
  end
end

return helpers
