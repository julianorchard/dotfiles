local logging = {}

local gears = require("gears")

logging.julog = function(message, level)
  local date_string, debug_file

  -- ERROR, WARN, DEBUG, INFO
  if level == nil then
    level = "DEBUG"
  end
  date_string = os.date("%c")
  debug_file =
    io.open(gears.filesystem.get_configuration_dir() .. "awesome.log", "a")
  if debug_file ~= nil then
    debug_file:write(date_string .. " - " .. level .. ": " .. message .. "\n")
    debug_file:close()
  else
    error("Debug file does not exist")
  end
end

return logging
