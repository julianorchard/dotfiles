-- These add new and useful os.X commands!
local logging = require("helpers.logging")

---os.capture
---Capture executable output, https://stackoverflow.com/a/326715
---@param cmd string The command to execute
---@param raw? boolean Return without formatting the output
function os.capture(cmd, raw)
  local f = assert(io.popen(cmd, "r"))
  local s = assert(f:read("*a"))
  f:close()
  if raw then
    return s
  end
  s = string.gsub(s, "^%s+", "")
  s = string.gsub(s, "%s+$", "")
  s = string.gsub(s, "[\n\r]+", " ")
  return s
end

---os.executable
---Find out whether an exe is available and in the PATH
---@param program string Representing the application to check for
function os.executable(program)
  local installed =
    os.capture("! command -v " .. program .. " &> /dev/null && echo 'n'")
  if installed == "n" then
    logging.julog(
      "Executable " .. program .. " is not installed/in the PATH",
      "WARN"
    )
    return 1
  else
    return 0
  end
end

---os.find
---Wrapper around the `find` command
---@param search string Search string, wrapped in double quotes to allow for wildcarding
---@param path string Input path to search within
---@param depth? integer Number representing the max depth the command should search
function os.find(search, path, depth)
  -- Check find is available
  if os.executable("find") == 1 then
    local err_msg = "`find` command not available"
    logging.julog(err_msg, "ERROR")
    return err_msg
  end

  -- Handle unspecified maxdepth
  local depth_cmd = nil
  if depth ~= nil then
    depth_cmd = " -maxdepth " .. depth .. " "
  end

  return os.capture("find " .. path .. depth_cmd .. ' -name "' .. search .. '"')
end
