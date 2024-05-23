local M = {}

---table_contains
---Find out if a table contains a value
---@param tbl table The input table
---@param x any The value to find
---@return boolean # The result of the search
M.table_contains = function(tbl, x)
  for _, v in pairs(tbl) do
    if v == x then
      return true
    end
  end
  return false
end

return M

-- vi: ft=lua
