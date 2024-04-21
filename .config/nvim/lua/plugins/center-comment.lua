local M = {
  "julian/center-comment.vim",
  name = "center-comment.vim",
  dev = true
}

local function center_comment_any()
  -- Get the next key pressed
  local next_keypress = vim.fn.getcharstr()

  -- Include the next key as the center comment character
  vim.cmd(string.format([[:CenterComment %s]], next_keypress))
end

M.config = function ()
  vim.keymap.set("n", "<leader>cc", center_comment_any)
end

return M
